/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

int check_line(char *str)
{
    if (str[0] < '0' || str[0] >= '9' || str[1] != ':')
        return (-1);
    if (str[2] < 65 || str[2] > 72 || str[5] < 65 || str[5] > 72)
        return (-1);
    if (str[3] < '0' || str[3] >= '9' || str[4] != ':')
        return (-1);
    if (str[6] < '0' || str[6] >= '9')
        return (-1);
}

int check_boat(int c, char *str)
{
    static int nb = 0;
    static int pass = 0;
    static int for_2 = 0;

    nb = nb + c;
    pass++;
    if (c == 2)
        for_2 = for_2 + 2;
    if (c == 3)
        for_2 = for_2 + 3;
    if (c == 4)
        for_2 = for_2 + 4;
    if (c == 5)
        for_2 = for_2 + 5;
    if (pass > 4)
        return (-1);
    if ((nb == 14 && for_2 != 14) || nb > 14 || check_line(str) == -1)
        return (-1);
    return (0);
}
