/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** error
*/

#include "error.h"

#include <stdio.h>

int print_usage(const char *progname)
{
    printf("USAGE: %s -p port -x width -y height -n name1 name2 ... "
        "-c clientsNb -f freq\n", progname);
    return 84;
}
