/*
** EPITECH PROJECT, 2023
** my_strncat
** File description:
** concatenate two strings
*/

int my_strlen_strncat(char const *str)
{
    int i = 0;

    while (str[i] != '\0') {
        i++;
    }
    return i;
}

char *my_strncat(char *dest, char const *src, int nb)
{
    int i = 0;
    int len_dest = my_strlen_strncat(dest);
    int len_src = my_strlen_strncat(src);

    while (i != len_src && i < nb) {
        dest[i + len_dest] = src[i];
        i++;
    }
    dest[len_dest + nb] = '\0';
    return dest;
}
