/*
** EPITECH PROJECT, 2023
** my_char_islower
** File description:
** 1 is a char is lowercase, 0 otherwise
*/

int my_char_islower(char c)
{
    if ('a' <= c && c <= 'z')
        return 1;
    return 0;
}
