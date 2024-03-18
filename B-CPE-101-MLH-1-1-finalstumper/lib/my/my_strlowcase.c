/*
** EPITECH PROJECT, 2023
** my_strlowcase
** File description:
** Puts every letter of every word in lowercase.
*/

char *my_strlowcase(char *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if ('A' <= str[i] && str[i] <= 'Z')
            str[i] += 32;
        i++;
    }
    return str;
}
