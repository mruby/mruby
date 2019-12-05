#include "carbuncle/avl.h"

struct avl_node
{
  mrb_int value;
  void *data;
  struct avl_node* left;
  struct avl_node* right;
  size_t height;
};

struct mrb_AVL
{
  struct avl_node* root;
  mrb_avl_free_fn free_function;
};

static struct avl_node *
avl_create_node(mrb_state *mrb, mrb_int value, void *data)
{
  struct avl_node *node = mrb_malloc(mrb, sizeof *node);
  node->value = value;
  node->data = data;
  node->left = node->right = NULL;
  node->height = 0;
  return node;
}

static size_t
avl_get_height(struct avl_node *node)
{
  if (!node) { return -1; }
  return node->height;
}

static void
avl_update_height(struct avl_node *node)
{
  int lh = avl_get_height(node->left);
  int rh = avl_get_height(node->right);
  node->height = (lh > rh) ? (lh + 1) : (rh + 1);
}

static struct avl_node *
avl_rotate_left(struct avl_node* node)
{
  struct avl_node *center = node->right;

  // The center's left child and n "trade places" in the tree.
  node->right = center->left;
  center->left = node;

  avl_update_height(node);
  avl_update_height(center);

  return center;
}

static struct avl_node *
avl_rotate_right(struct avl_node* node)
{
  struct avl_node* center = node->left;
  node->left = center->right;
  center->right = node;
  avl_update_height(node);
  avl_update_height(center);
  return center;
}

static size_t
avl_balance_factor(struct avl_node *node)
{
  return avl_get_height(node->right) - avl_get_height(node->left);
}

static struct avl_node *
avl_balance(struct avl_node *node)
{
  size_t bf = avl_balance_factor(node);
  if (bf < -1)
  {
    if (avl_balance_factor(node->left) > 0)
    {
      node->left = avl_rotate_left(node->left);
    }
    return avl_rotate_right(node);

  }
  else if (bf > 1)
  {
    if (avl_balance_factor(node->right) < 0)
    {
      node->right = avl_rotate_right(node->right);
    }
    return avl_rotate_left(node);
  }
  else
  {
    avl_update_height(node);
    return node;
  }
}

static struct avl_node *
avl_subtree_insert(mrb_state *mrb, struct avl_node *node, mrb_int value, void *data)
{
  if (node == NULL)
  {
    return avl_create_node(mrb, value, data);
  }
  else if (value < node->value)
  {
    node->left = avl_subtree_insert(mrb, node->left, value, data);
  }
  else
  {
    node->right = avl_subtree_insert(mrb, node->right, value, data);
  }
  return avl_balance(node);
}

static size_t
avl_subtree_min_value(struct avl_node *node)
{
  while (node->left != NULL)
  {
    node = node->left;
  }
  return node->value;
}

static struct avl_node *
avl_subtree_remove(mrb_state *mrb, struct mrb_AVL *tree, struct avl_node* node, mrb_int value)
{
  if (node == NULL)
  {
    return NULL;
  }
  else if (value < node->value)
  {
    node->left = avl_subtree_remove(mrb, tree, node->left, value);
    return avl_balance(node);
  }
  else if (value > node->value)
  {
    node->right = avl_subtree_remove(mrb, tree, node->right, value);
    return avl_balance(node);
  }
  else
  {
    if (node->left && node->right)
    {
      node->value = avl_subtree_min_value(node->right);
      node->right = avl_subtree_remove(node, tree, node->right, node->value);
      return avl_balance(node);
    }
    else if (node->left)
    {
      struct avl_node *left_child = node->left;
      tree->free_function(mrb, node->data);
      mrb_free(mrb, node);
      return left_child;
    }
    else if (node->right != NULL)
    {
      struct avl_node *right_child = node->right;
      tree->free_function(mrb, node->data);
      mrb_free(mrb, node);
      return right_child;
    }
    else
    {
      mrb_free(mrb, node);
      return NULL;
    }
  }
}

struct mrb_AVL *
mrb_carbuncle_avl_new(mrb_state *mrb, mrb_avl_free_fn free_function)
{
  struct mrb_AVL *tree = mrb_malloc(mrb, sizeof *tree);
  tree->root = NULL;
  tree->free_function = free_function;
  return tree;
}

void
mrb_carbuncle_avl_free(mrb_state *mrb, struct mrb_AVL *tree)
{
  while (!mrb_carbuncle_avl_is_empty(tree)) {
    mrb_carbuncle_avl_remove(mrb, tree->root->value, tree);
  }
}

mrb_bool
mrb_carbuncle_avl_is_empty(struct mrb_AVL* tree)
{
  return tree->root == NULL;
}

void
mrb_carbuncle_avl_insert(mrb_state *mrb, struct mrb_AVL *tree, mrb_int value, void *data)
{
  tree->root = avl_subtree_insert(mrb, tree->root, value, data);
}

void
mrb_carbuncle_avl_remove(mrb_state *mrb, struct mrb_AVL *tree, mrb_int value)
{
  tree->root = avl_subtree_remove(mrb, tree, tree->root, value);
}

mrb_bool
mrb_carbuncle_avl_contains(struct mrb_AVL *tree, mrb_int value)
{
  struct avl_node *current = tree->root;
  while (current != NULL)
  {
    if (value == current->value) { return TRUE; }
    else if (value < current->value) { current = current->left; }
    else { current = current->right; }
  }
  return FALSE;
}

void *
mrb_carbuncle_avl_data(struct mrb_AVL *tree, mrb_int value)
{
  struct avl_node *current = tree->root;
  while (current != NULL)
  {
    if (value == current->value) { return current->data; }
    else if (value < current->value) { current = current->left; }
    else { current = current->right; }
  }
  return NULL;
}