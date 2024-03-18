/*
** EPITECH PROJECT, 2023
** my_strncmp
** File description:
** Compares the n first bytes of two strings.
** Returns 0 if they are equal,
** 1 if s1 is bigger than s2 and 2 if s2 is bigger than s1
*/

int my_strncmp(char const *s1, char const *s2, int n)
{
    int i = 0;

    while (s1[i] != '\0' && s2[i] != '\0' && i < n && s1[i] == s2[i])
        i++;
    if (i == n)
        return 0;
    return (s1[i] - s2[i]);
}
