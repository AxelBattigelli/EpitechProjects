/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

char **so_boat(char **tab, char *str, int pos, int length)
{
    int from;
    int to;
    int tmp = length + 48;

    if (str[3] <= str[6]) {
        from = str[3] - 48;
        to = str[6] - 48;
    } else {
        from = str[6] - 48;
        to = str[3] - 48;
    }
    if (to - from != length - 1)
        return (NULL);
    from++;
    while (length > 0) {
        tab[from][pos] = tmp;
        length--;
        from++;
    }
    return (tab);
}

char **linear_boat(char **tab, char *str, int pos, int length)
{
    int from = (str[3] - 48) + 1;
    int tmp = length + 48;

    while (length > 0) {
        tab[from][pos] = tmp;
        pos = pos + 2;
        length--;
    }
    return (tab);
}
