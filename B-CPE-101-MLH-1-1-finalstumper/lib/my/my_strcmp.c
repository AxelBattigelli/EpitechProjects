/*
** EPITECH PROJECT, 2023
** my_strcmp
** File description:
** Compares two string. Returns 0 if they're equal,
** 1 if a str1 is bigger than str2 and 2 if str2 is bigger than str1.
*/

int my_strcmp(char const *s1, char const *s2)
{
    int i = 0;

    while (s1[i] != '\0' && s2[i] != '\0' && s1[i] == s2[i])
        i++;
    return (s1[i] - s2[i]);
}
