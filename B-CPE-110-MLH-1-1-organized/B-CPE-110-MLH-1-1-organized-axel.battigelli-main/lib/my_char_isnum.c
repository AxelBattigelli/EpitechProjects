/*
** EPITECH PROJECT, 2023
** my_char_isnum.c
** File description:
** return 0 if caracter is a num else 1
*/

int my_char_isnum(char c)
{
    if (c >= '0' && c <= '9')
        return 0;
    return 1;
}
