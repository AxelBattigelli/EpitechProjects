/*
** EPITECH PROJECT, 2023
** pointer_into_hexadecimal
** File description:
** display content of void*
*/

#include "include/my_printf.h"
#include "include/my.h"

int pointer_into_hexadecimal(long *nb)
{
    int len = 0;

    my_putstr("0x");
    len += 2;
    len += decimal_into_hexadecimal_min(&nb);
    return len;
}
