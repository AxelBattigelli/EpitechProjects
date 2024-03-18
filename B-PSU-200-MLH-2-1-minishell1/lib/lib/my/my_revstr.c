/*
** EPITECH PROJECT, 2023
** my_revstr
** File description:
** reverse a string
*/

#include "my.h"

char *my_revstr(char *str)
{
    int tmp1;
    int tmp2;
    int i = 0;
    int start = 0;
    int end;

    while (str[i] != '\0') {
        tmp2 = str[i];
        end = i;
        i++;
    }
    while (start < end) {
        tmp1 = str[start];
        tmp2 = str[end];
        str[start] = tmp2;
        str[end] = tmp1;
        start++;
        end--;
    }
    return str;
}
