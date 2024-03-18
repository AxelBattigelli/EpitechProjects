/*
** EPITECH PROJECT, 2023
** my_strcnpy
** File description:
** include a number of element from a string to another one
*/

char *my_strncpy(char *dest, char const *src, int n)
{
    int i = 0;

    while (i < n && src[i] != '\0') {
        dest[i] = src[i];
        i++;
    }
    if (n > i) {
        dest[i] = '\0';
    }
    return dest;
}
