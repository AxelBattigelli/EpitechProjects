/*
** EPITECH PROJECT, 2023
** decimal_into_hexadecimal
** File description:
** convert dec into hexa
*/

#include "include/my.h"

static int decimal_into_hexadecimal_min_core(long divide, int i, int j)
{
    char char_hex[12];
    char char_hexf[12];
    int rest;
    char nm[16] = {48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98,
        99, 100, 101, 102};
    int len = 0;

    while (divide > 0) {
        rest = divide % 16;
        char_hex[i] = nm[rest];
        divide /= 16;
        i++;
        len++;
    }
    for (; i >= 0; i--) {
        char_hexf[j] = char_hex[i];
        my_putchar(char_hexf[j]);
        j++;
    }
    return len;
}

int decimal_into_hexadecimal_min(long divide)
{
    int i = 0;
    int j = 0;
    int len;

    len = decimal_into_hexadecimal_min_core(divide, i, j);
    return len;
}
