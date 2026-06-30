/*
** task_queue.c - Task::Queue implementation
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/error.h>
#include <mruby/variable.h>
#include "task.h"

typedef struct mrb_task_queue {
  uint8_t closed;
} mrb_task_queue;

static void
mrb_task_queue_free(mrb_state *mrb, void *ptr)
{
  mrb_free(mrb, ptr);
}

static const struct mrb_data_type mrb_task_queue_type = {
  "Task::Queue", mrb_task_queue_free,
};

static mrb_value wait_retry_;
static mrb_value wait_timeout_;
static struct RClass *task_error_class_;

/* Wake the highest-priority task waiting on this queue */
static void
queue_wake_one_waiter(mrb_state *mrb, mrb_task_queue *q)
{
  mrb_task_disable_irq();
  mrb_task *curr = q_waiting_;
  while (curr) {
    mrb_task *next = curr->next;
    if (curr->reason == MRB_TASK_REASON_QUEUE && curr->wait.queue.target == q) {
      mrb_task_q_delete(mrb, curr);
      curr->status = MRB_TASK_STATUS_READY;
      curr->reason = MRB_TASK_REASON_NONE;
      curr->wait.queue.target = NULL;
      curr->wait.queue.wakeup_tick = UINT32_MAX;
      mrb_task_q_insert(mrb, curr);
      switching_ = TRUE;
      break;
    }
    curr = next;
  }
  mrb_task_enable_irq();
}

/* Wake all tasks waiting on this queue (used by close) */
static void
queue_wake_all_waiters(mrb_state *mrb, mrb_task_queue *q)
{
  mrb_bool woke_any = FALSE;
  mrb_task_disable_irq();
  mrb_task *curr = q_waiting_;
  while (curr) {
    mrb_task *next = curr->next;
    if (curr->reason == MRB_TASK_REASON_QUEUE && curr->wait.queue.target == q) {
      mrb_task_q_delete(mrb, curr);
      curr->status = MRB_TASK_STATUS_READY;
      curr->reason = MRB_TASK_REASON_NONE;
      curr->wait.queue.target = NULL;
      curr->wait.queue.wakeup_tick = UINT32_MAX;
      mrb_task_q_insert(mrb, curr);
      woke_any = TRUE;
    }
    curr = next;
  }
  if (woke_any) {
    switching_ = TRUE;
  }
  mrb_task_enable_irq();
}

static mrb_value
queue_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_task_queue *q = (mrb_task_queue*)mrb_malloc(mrb, sizeof(mrb_task_queue));
  q->closed = 0;
  mrb_data_init(self, q, &mrb_task_queue_type);
  mrb_iv_set(mrb, self, MRB_IVSYM(items), mrb_ary_new(mrb));
  return self;
}

static mrb_value
queue_push(mrb_state *mrb, mrb_value self)
{
  mrb_value obj;
  mrb_get_args(mrb, "o", &obj);

  mrb_task_queue *q = (mrb_task_queue*)mrb_data_get_ptr(mrb, self, &mrb_task_queue_type);
  if (!q) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid queue");
  if (q->closed) mrb_raise(mrb, task_error_class_, "queue closed");

  mrb_value items = mrb_iv_get(mrb, self, MRB_IVSYM(items));
  mrb_ary_push(mrb, items, obj);
  queue_wake_one_waiter(mrb, q);
  return self;
}

/*
 * __deadline: convert a millisecond timeout into an absolute tick deadline.
 *
 * The conversion (ceil to ticks, range check, sentinel normalization) is done
 * once in C and the result is passed back to __pop_try on every retry. Working
 * in C tick space keeps the comparison wrap-safe (see queue_pop_try) and avoids
 * the 32-bit wrap-around of Task.tick that a Ruby-side remaining-time
 * subtraction would suffer. timeout_ms is already validated as a non-negative
 * Integer by Queue#pop; the checks here are the defensive C-level backstop.
 */
static mrb_value
queue_deadline(mrb_state *mrb, mrb_value self)
{
  mrb_int timeout_ms;
  mrb_get_args(mrb, "i", &timeout_ms);
  if (timeout_ms < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "timeout_ms must be non-negative");
  }

  uint64_t ticks = ((uint64_t)timeout_ms + MRB_TICK_UNIT - 1) / MRB_TICK_UNIT;
  if (INT32_MAX < ticks) {
    mrb_raise(mrb, E_RANGE_ERROR, "timeout_ms is too large");
  }

  uint32_t deadline = mrb_task_normalize_wakeup(tick_ + (uint32_t)ticks);
  return mrb_int_value(mrb, (mrb_int)deadline);
}

/*
 * __pop_try: try to pop one item. Called as __pop_try(non_block, deadline)
 * where deadline is an absolute tick (from __deadline) or nil for no timeout.
 * Returns:
 *   - the item if available
 *   - nil if closed and empty
 *   - raises Task::Error if non_block and empty
 *   - Task::Queue::WAIT_TIMEOUT sentinel when the deadline has passed
 *   - Task::Queue::WAIT_RETRY sentinel if the current task was put to WAITING
 *
 * Ruby-level pop loops on WAIT_RETRY and maps WAIT_TIMEOUT to nil.
 */
static mrb_value
queue_pop_try(mrb_state *mrb, mrb_value self)
{
  mrb_bool non_block = FALSE;
  mrb_value deadline_value = mrb_nil_value();
  mrb_get_args(mrb, "|bo", &non_block, &deadline_value);

  mrb_task_queue *q = (mrb_task_queue*)mrb_data_get_ptr(mrb, self, &mrb_task_queue_type);
  if (!q) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid queue");

  mrb_value items = mrb_iv_get(mrb, self, MRB_IVSYM(items));

  /* Item available - return it */
  if (RARRAY_LEN(items) > 0) {
    return mrb_ary_shift(mrb, items);
  }

  /* Closed and empty */
  if (q->closed) {
    return mrb_nil_value();
  }

  /* Non-blocking and empty */
  if (non_block) {
    mrb_raise(mrb, task_error_class_, "queue empty");
  }

  /* Deadline already reached (covers timeout_ms: 0) - give up without parking.
   * The comparison is wrap-safe: deadline and tick_ are both uint32 tick counts
   * and (int32_t)(deadline - tick_) stays correct across the tick wrap. */
  mrb_bool has_deadline = !mrb_nil_p(deadline_value);
  uint32_t deadline = 0;
  if (has_deadline) {
    deadline = (uint32_t)mrb_integer(deadline_value);
    if ((int32_t)(deadline - tick_) <= 0) {
      return wait_timeout_;
    }
  }

  /* Blocking pop only works inside a task */
  if (mrb->c == mrb->root_c) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "blocking pop can only be called from within a task");
  }

  /* Blocking pop requires the scheduler to be running */
  task_check_scheduler_lock(mrb);

  /* Guard against yielding from inside a C function boundary */
  mrb_callinfo *ci;
  for (ci = mrb->c->ci; ci >= mrb->c->cibase; ci--) {
    if (ci->cci > 0) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "blocking pop cannot be called from within a C function boundary");
    }
  }

  /* Move current task to WAITING */
  mrb_task *current = MRB2TASK(mrb);
  mrb_task_disable_irq();
  mrb_task_q_delete(mrb, current);
  current->status = MRB_TASK_STATUS_WAITING;
  current->reason = MRB_TASK_REASON_QUEUE;
  current->wait.queue.target = q;
  current->wait.queue.wakeup_tick = has_deadline ? deadline : UINT32_MAX;
  if (has_deadline &&
      (wakeup_tick_ == UINT32_MAX ||
       (int32_t)(deadline - wakeup_tick_) < 0)) {
    wakeup_tick_ = deadline;
  }
  mrb_task_q_insert(mrb, current);
  mrb_task_enable_irq();
  switching_ = TRUE;

  /* Return sentinel; the Ruby pop loop will retry after wakeup */
  return wait_retry_;
}

static mrb_value
queue_size(mrb_state *mrb, mrb_value self)
{
  mrb_value items = mrb_iv_get(mrb, self, MRB_IVSYM(items));
  return mrb_int_value(mrb, RARRAY_LEN(items));
}

static mrb_value
queue_empty_p(mrb_state *mrb, mrb_value self)
{
  mrb_value items = mrb_iv_get(mrb, self, MRB_IVSYM(items));
  return mrb_bool_value(RARRAY_LEN(items) == 0);
}

static mrb_value
queue_clear(mrb_state *mrb, mrb_value self)
{
  mrb_value items = mrb_iv_get(mrb, self, MRB_IVSYM(items));
  mrb_ary_clear(mrb, items);
  return self;
}

static mrb_value
queue_close(mrb_state *mrb, mrb_value self)
{
  mrb_task_queue *q = (mrb_task_queue*)mrb_data_get_ptr(mrb, self, &mrb_task_queue_type);
  if (!q) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid queue");
  if (!q->closed) {
    q->closed = 1;
    queue_wake_all_waiters(mrb, q);
  }
  return self;
}

static mrb_value
queue_closed_p(mrb_state *mrb, mrb_value self)
{
  mrb_task_queue *q = (mrb_task_queue*)mrb_data_get_ptr(mrb, self, &mrb_task_queue_type);
  if (!q) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid queue");
  return mrb_bool_value(q->closed);
}

static mrb_value
queue_num_waiting(mrb_state *mrb, mrb_value self)
{
  mrb_task_queue *q = (mrb_task_queue*)mrb_data_get_ptr(mrb, self, &mrb_task_queue_type);
  if (!q) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid queue");
  uint32_t count = 0;
  mrb_task_disable_irq();
  mrb_task *curr = q_waiting_;
  while (curr) {
    if (curr->reason == MRB_TASK_REASON_QUEUE && curr->wait.queue.target == q) {
      count++;
    }
    curr = curr->next;
  }
  mrb_task_enable_irq();
  return mrb_int_value(mrb, (mrb_int)count);
}

void
mrb_init_task_queue(mrb_state *mrb, struct RClass *task_class)
{
  struct RClass *queue_class;

  queue_class = mrb_define_class_under_id(mrb, task_class, MRB_SYM(Queue), mrb->object_class);
  MRB_SET_INSTANCE_TT(queue_class, MRB_TT_DATA);

  task_error_class_ = mrb_class_get_under_id(mrb, task_class, MRB_SYM(Error));

  /* Allocate and store WAIT_RETRY sentinel (rooted by the class constant table) */
  wait_retry_ = mrb_obj_new(mrb, mrb->object_class, 0, NULL);
  wait_timeout_ = mrb_obj_new(mrb, mrb->object_class, 0, NULL);
  mrb_define_const_id(mrb, queue_class, MRB_SYM(WAIT_RETRY), wait_retry_);
  mrb_define_const_id(mrb, queue_class, MRB_SYM(WAIT_TIMEOUT), wait_timeout_);

  mrb_define_method_id(mrb, queue_class, MRB_SYM(initialize),  queue_initialize,  MRB_ARGS_NONE());
  mrb_define_method_id(mrb, queue_class, MRB_SYM(__push),      queue_push,        MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, queue_class, MRB_SYM(__pop_try),   queue_pop_try,     MRB_ARGS_OPT(2));
  mrb_define_method_id(mrb, queue_class, MRB_SYM(__deadline),  queue_deadline,    MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, queue_class, MRB_SYM(size),        queue_size,        MRB_ARGS_NONE());
  mrb_define_method_id(mrb, queue_class, MRB_SYM(length),      queue_size,        MRB_ARGS_NONE());
  mrb_define_method_id(mrb, queue_class, MRB_SYM_Q(empty),     queue_empty_p,     MRB_ARGS_NONE());
  mrb_define_method_id(mrb, queue_class, MRB_SYM(clear),       queue_clear,       MRB_ARGS_NONE());
  mrb_define_method_id(mrb, queue_class, MRB_SYM(close),       queue_close,       MRB_ARGS_NONE());
  mrb_define_method_id(mrb, queue_class, MRB_SYM_Q(closed),    queue_closed_p,    MRB_ARGS_NONE());
  mrb_define_method_id(mrb, queue_class, MRB_SYM(num_waiting), queue_num_waiting, MRB_ARGS_NONE());
}
