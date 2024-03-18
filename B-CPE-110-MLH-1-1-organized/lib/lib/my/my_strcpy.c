/*
** EPITECH PROJECT, 2023
** my_strcpy
** File description:
** return characters in a string from an other string
*/

char *my_strcpy(char *dest, char const *src)
{
    int i = 0;

    while (src[i] != '\0') {
        dest[i] = src[i];
        i++;
    }
    dest[i] = '\0';
    return dest;
}
