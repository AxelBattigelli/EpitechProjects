#include <criterion/criterion.h>
#include <stdio.h>
#include "../include/hashtable.h"
#include "../include/my.h"

Test(hash, hash_gen) {
    char *str = "a";
    int nb = 6;

    int result = hash(str, nb);

    cr_assert_eq(result, 3517090, "Test failure: The output must match the regular expression '^OK$', but it was 'KO: output differs");
}