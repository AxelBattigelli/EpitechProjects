/*
** EPITECH PROJECT, 2023
** main
** File description:
** main file for organized
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "include/shell.h"
#include "include/my.h"

int nb_elem_args(char **args)
{
    int len = 0;

    while (args[len] != NULL)
        len++;
    return len;
}

int main(int ac, char **av)
{
    struct Node *head = NULL;

    if (ac > 1) {
        if (my_strcmp(av[1], "-h") || my_strcmp(av[1], "--help")) {
            my_printf("organized [-h]\n\nDESCRIPTION\n\tWorkshop shell\n\tYou"\
                " can use add [TYPE NAME], delete [ID], sort [SORT], diplay"\
                " and exit\n");
            return 0;
        } else {
            my_printf("organized [-h]\n\nDESCRIPTION\n\tWorkshop shell\n\tYou"\
                " can use add [TYPE NAME], delete [ID], sort [SORT], diplay"\
                " and exit\n");
            return 84;
        }
    }
    init_write_nb("./id.txt");
    return workshop_shell(&head);
}
