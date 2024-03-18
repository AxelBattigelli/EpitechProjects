/*
** EPITECH PROJECT, 2023
** final_stumper
** File description:
** Returns lenght and height of the square
*/

#include "final_stumper.h"
#include "rush_array.h"

int length(char *str)
{
    int length = 0;

    for (length; str[length] != '\n' && str[length] != '\0'; length++);
    return length;
}

int height(char *str)
{
    int i = 0;
    int height = 0;

    for (i; str[i] != '\0'; i++)
        if (str[i] == '\n')
            height++;
    return height;
}

void print_result(int i, int x, int y)
{
    my_putstr("[rush1-");
    my_put_nbr(i);
    my_putstr("] ");
    my_put_nbr(x);
    my_putchar(' ');
    my_put_nbr(y);
}

void rush3(char *buff)
{
    int x = length(buff);
    int y = height(buff);
    int pipe = 0;

    if (x == 0 || y == 0) {
        my_putstr("none");
        my_putchar('\n');
        return;
    }
    for (int i = 0; i < 5; i++) {
        if (pipe && my_strcmp(RUSH[i].fct(x, y), buff) == 0)
            my_putstr(" || ");
        if (my_strcmp(RUSH[i].fct(x, y), buff) == 0) {
            print_result(i + 1, x, y);
            pipe = 1;
        }
    }
    if (pipe == 0)
        my_putstr("none");
    my_putchar('\n');
}
