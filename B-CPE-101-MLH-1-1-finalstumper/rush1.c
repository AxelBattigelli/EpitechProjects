/*
** EPITECH PROJECT, 2023
** rush1-1
** File description:
** main file of rush1-1
*/

#include "include/my.h"
#include "final_stumper.h"
#include <stdlib.h>

char *rush1_1(int x, int y)
{
    char *string = malloc(sizeof(char) * (x + 1) * y + 1);

    if (x == 0 || y == 0) {
        string[0] = '\0';
        return string;
    }
    if (y > 0)
        my_strcat(string, line(x, 'o', '-', 'o'));
    for (int i = 1; i < y - 1; i++)
        my_strcat(string, line(x, '|', ' ', '|'));
    if (y > 1)
        my_strcat(string, line(x, 'o', '-', 'o'));
    my_strcat(string, "\0");
    return string;
}

char *rush1_2(int x, int y)
{
    char *string = malloc(sizeof(char) * (x + 1) * y + 1);

    if (x == 0 || y == 0) {
        string[0] = '\0';
        return string;
    }
    if (x == 1 || y == 1) {
        for (int i = 0; i < y; i ++)
            my_strcat(string, line(x, '*', '*', '*'));
        my_strcat(string, "\0");
        return string;
    }
    my_strcat(string, line(x, '/', '*', '\\'));
    for (int i = 1; i < y - 1; i++)
        my_strcat(string, line(x, '*', ' ', '*'));
    my_strcat(string, line(x, '\\', '*', '/'));
    my_strcat(string, "\0");
    return string;
}

char *rush1_3(int x, int y)
{
    char *string = malloc(sizeof(char) * (x + 1) * y + 1);

    if (x == 0 || y == 0) {
        string[0] = '\0';
        return string;
    }
    if (x == 1 || y == 1) {
        for (int i = 0; i < y; i ++)
            my_strcat(string, line(x, 'B', 'B', 'B'));
        my_strcat(string, "\0");
        return string;
    }
    my_strcat(string, line(x, 'A', 'B', 'A'));
    for (int i = 1; i < y - 1; i++)
        my_strcat(string, line(x, 'B', ' ', 'B'));
    my_strcat(string, line(x, 'C', 'B', 'C'));
    my_strcat(string, "\0");
    return string;
}

char *rush1_4(int x, int y)
{
    char *string = malloc(sizeof(char) * (x + 1) * y + 1);

    if (x == 0 || y == 0) {
        string[0] = '\0';
        return string;
    }
    if (x == 1 || y == 1) {
        for (int i = 0; i < y; i ++)
            my_strcat(string, line(x, 'B', 'B', 'B'));
        my_strcat(string, "\0");
        return string;
    }
    my_strcat(string, line(x, 'A', 'B', 'C'));
    for (int i = 1; i < y - 1; i++)
        my_strcat(string, line(x, 'B', ' ', 'B'));
    my_strcat(string, line(x, 'A', 'B', 'C'));
    my_strcat(string, "\0");
    return string;
}

char *rush1_5(int x, int y)
{
    char *string = malloc(sizeof(char) * (x + 1) * y + 1);

    if (x == 0 || y == 0) {
        string[0] = '\0';
        return string;
    }
    if (x == 1 || y == 1) {
        for (int i = 0; i < y; i ++)
            my_strcat(string, line(x, 'B', 'B', 'B'));
        my_strcat(string, "\0");
        return string;
    }
    my_strcat(string, line(x, 'A', 'B', 'C'));
    for (int i = 1; i < y - 1; i++)
        my_strcat(string, line(x, 'B', ' ', 'B'));
    my_strcat(string, line(x, 'C', 'B', 'A'));
    my_strcat(string, "\0");
    return string;
}
