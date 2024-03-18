/*
** EPITECH PROJECT, 2023
** test_mini_printf
** File description:
** test of printf
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "include/my.h"

void redirect_all_std(void)
{
    cr_redirect_stdout();
    cr_redirect_stderr();
}

Test (mini_printf, simple_string, .init = redirect_all_std)
{
    mini_printf("hello world");
    cr_assert_stdout_eq_str("hello world");
}

Test (mini_printf, simple_string_c, .init = redirect_all_std)
{
    mini_printf("hello %c", 'W');
    cr_assert_stdout_eq_str("hello W");
}

Test (mini_printf, simple_string_s, .init = redirect_all_std)
{
    mini_printf("hello %s", " world");
    cr_assert_stdout_eq_str("hello world");
}

Test (mini_printf, simple_string_d, .init = redirect_all_std)
{
    mini_printf("hello %d", 42);
    cr_assert_stdout_eq_str("hello 42");
}

Test (mini_printf, simple_string_i, .init = redirect_all_std)
{
    mini_printf("hello %i", 42);
    cr_assert_stdout_eq_str("hello 42");
}

Test (mini_printf, simple_string_prc, .init = redirect_all_std)
{
    mini_printf("hello %% world");
    cr_assert_stdout_eq_str("hello % world");
}

