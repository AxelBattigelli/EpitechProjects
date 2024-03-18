/*
** EPITECH PROJECT, 2023
** my_rand
** File description:
** get random nb
*/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

static int get_rand(void)
{
    int nb = rand();

    nb = nb % 780;
    return nb;
}

int my_rand(void)
{
    int nb;

    srand(time(NULL));
    nb = get_rand();
    return nb;
}
