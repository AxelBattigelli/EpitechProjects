/*
** EPITECH PROJECT, 2023
** my_showstr
** File description:
** display non printable caracter in hexadecimal
*/

#include "my.h"

// convert and display hexa caracter
void my_hexa_conv(char str)
{
    int i = 0;
    int j = 0;
    char char_hex[4];
    char char_hexf[4];
    int divide = str;
    int rest;
    char nm[16] = {48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 65, 66,
        67, 68, 69, 70};

    while (divide > 0) {
        rest = divide % 16;
        char_hex[i] = nm[rest];
        divide /= 16;
        i++;
    }
    for (; i >= 0; i--) {
        char_hexf[j] = char_hex[i];
        my_putchar(char_hexf[j]);
        j++;
    }
}

// find the non printable and display the other
int my_showstr(char const *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if (str[i] > 31 && str[i] < 177)
            my_putchar(str[i]);
        else
            my_hexa_conv(str[i]);
        i++;
    }
    return 0;
}
