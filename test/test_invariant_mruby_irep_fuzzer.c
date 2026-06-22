#include <check.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

/* Declare the fuzzer entry point from mruby_irep_fuzzer.c */
extern int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t size);

START_TEST(test_irep_loader_memory_safety)
{
    /* Invariant: The irep loader must not crash or corrupt memory regardless of input */
    
    /* Payload 1: Corrupted size field - oversized pool count in minimal header */
    const uint8_t payload_corrupted_size[] = {
        'R', 'I', 'T', 'E', '0', '3', '0', '0',  /* RITE header */
        0xFF, 0xFF, 0xFF, 0xFF,                   /* Corrupted size field */
        0x00, 0x00, 0x00, 0x01,                   /* Instruction count */
        0xFF, 0xFF, 0xFF, 0xFF                    /* Oversized pool entries */
    };
    
    /* Payload 2: Empty input - boundary case */
    const uint8_t payload_empty[] = { 0 };
    
    /* Payload 3: Valid minimal mruby bytecode header */
    const uint8_t payload_valid[] = {
        'R', 'I', 'T', 'E', '0', '3', '0', '0',
        0x00, 0x00, 0x00, 0x10,
        0x00, 0x00, 0x00, 0x00
    };

    struct {
        const uint8_t *data;
        size_t size;
    } payloads[] = {
        { payload_corrupted_size, sizeof(payload_corrupted_size) },
        { payload_empty, 0 },
        { payload_valid, sizeof(payload_valid) }
    };

    for (size_t i = 0; i < sizeof(payloads) / sizeof(payloads[0]); i++) {
        /* The fuzzer must return without crashing - return value is informational */
        int result = LLVMFuzzerTestOneInput(payloads[i].data, payloads[i].size);
        /* Fuzzer returns 0 on success, non-zero is acceptable for rejected input */
        ck_assert_msg(result >= -1 && result <= 1,
                      "Fuzzer returned unexpected value %d for payload %zu", result, i);
    }
}
END_TEST

Suite *security_suite(void)
{
    Suite *s;
    TCase *tc_core;

    s = suite_create("Security");
    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_irep_loader_memory_safety);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = security_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}