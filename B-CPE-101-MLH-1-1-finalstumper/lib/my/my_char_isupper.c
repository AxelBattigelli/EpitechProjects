/*
** EPITECH PROJECT, 2023
** my_char_isupper
** File description:
** 1 is a char is uppercase, 0 otherwise
*/

int my_char_isupper(char c)
{
    if ('A' <= c && c <= 'Z')
        return 1;
    return 0;
}
