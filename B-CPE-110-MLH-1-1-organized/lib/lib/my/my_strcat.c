/*
** EPITECH PROJECT, 2023
** my_strcat
** File description:
** concatenate two strings
*/

int my_strlen_strcat(char const *str)
{
    int i = 0;

    while (str[i] != '\0') {
        i++;
    }
    return i;
}

char *my_strcat(char *dest, char const *src)
{
    int i = 0;
    int len_dest = my_strlen_strcat(dest);
    int len_src = my_strlen_strcat(src);

    while (i != len_src) {
        dest[i + len_dest] = src[i];
        i++;
    }
    dest[len_dest + len_src] = '\0';
    return dest;
}
