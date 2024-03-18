/*
** EPITECH PROJECT, 2023
** disp
** File description:
** disp
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "include/shell.h"
#include "include/my.h"

int disp(void *data, char **args)
{
    struct Node *node = *(struct Node**)data;

    if (nb_elem_args(args) != 0)
        return 84;
    if (nb_elem_args(args) > 0)
        return 84;
    if (node == NULL)
        return 0;
    while (node != NULL) {
        my_printf("%s n°%d - \"%s\"\n", node->type, node->index, node->name);
        node = node->next;
    }
    return 0;
}
