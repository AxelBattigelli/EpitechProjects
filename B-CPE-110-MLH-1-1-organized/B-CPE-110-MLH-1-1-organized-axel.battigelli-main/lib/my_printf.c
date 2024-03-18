/*
** EPITECH PROJECT, 2023
** my_printf
** File description:
** main of printf
*/

#include <stddef.h>
#include "./include/my.h"
#include "./include/my_printf.h"
#include <stdarg.h>

static void list_first_flag(int (**first_flag)())
{
    first_flag[0] = hashtag;
    first_flag[1] = my_putchar_r;
}

static void list_second_flag(int (**second_flag)())
{
    second_flag[0] = my_putchar_r;
    second_flag[1] = my_len_nbr;
    second_flag[2] = my_len_nbr;
    second_flag[3] = my_putstr_len;
    second_flag[4] = decimal_into_octal;
    second_flag[5] = my_putstr;
    second_flag[6] = my_put_unsigned_nbr;
    second_flag[7] = decimal_into_hexadecimal_min;
    second_flag[8] = decimal_into_hexadecimal_up;
    second_flag[9] = decimal_into_binary;
    second_flag[10] = my_showstr;
}

static int if_space(char const *str)
{
    int i = 0;
    int len = 0;

    if (str[i] == ' ') {
        my_putchar(' ');
    }
    for (; str[i] == ' '; i++) {
        len++;
    }
    return len;
}

int second_flag(va_list list, const char *format)
{
    int len = 0;
    int j = 0;
    int (*second_flag[13])(void *);
    char *second_str = my_strdup("cidsopuxXbS");

    list_second_flag(second_flag);
    for (; (format[0] != second_str[j] && second_str[j]); j++);
    if (format[0] == second_str[j]) {
        len += (*second_flag[j])(va_arg(list, void *));
    } else if (format[0] == '%') {
        my_putchar('%');
        len++;
    }
    return len;
}

int first_flag(va_list list, const char *format)
{
    int i = 0;
    int len = 0;
    int j = 0;
    int (*first_flag[4])(const char *);
    char *first_str = my_strdup("#.0-+hlqMjzZt");

    list_first_flag(first_flag);
    i += if_space(format);
    if (i != 0)
        len++;
    for (; (format[i] != first_str[j] && first_str[j]); j++);
    if (format[i] == first_str[j])
        len += (*first_flag[j])(format);
    if (format[i + 1] && format[i] == first_str[j]) {
        len += second_flag(list, &format[i + 1]);
    } else
        len += second_flag(list, &format[i]);
    return len;
}

static int get_flaglen(char const *str)
{
    int i = 0;
    char *list_flag = my_strdup("cidsopuxXbS%");

    for (; str[i] == ' ' && str[i] != 0; i++);
    for (int j = 0; list_flag[j]; j++) {
        if (str[i] == list_flag[j]) {
            i++;
            return i;
        }
    }
    i += get_flaglen(&str[i + 1]);
    return i + 1;
}

int my_printf(const char *format, ...)
{
    int i = 0;
    int len = 0;
    va_list list;
    int strlen = my_strlen(format);

    va_start(list, format);
    for (; i < strlen; i++) {
        if (format[i] == '%' && format[i + 1]) {
            len += first_flag(list, &format[i + 1]);
            i += get_flaglen(&format[i + 1]);
        } else {
            len++;
            my_putchar(format[i]);
        }
    }
    va_end(list);
    return len;
}
