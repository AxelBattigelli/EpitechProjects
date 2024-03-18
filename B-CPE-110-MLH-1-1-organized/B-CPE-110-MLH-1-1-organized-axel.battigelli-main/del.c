/*
** EPITECH PROJECT, 2023
** del
** File description:
** del
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "include/shell.h"
#include "include/my.h"

static int tmp(struct Node *prev, struct Node **head_ref, struct Node *current)
{
    if (prev == NULL)
        *head_ref = current->next;
    else
        prev->next = current->next;
    my_printf("%s n°%d - \"%s\" deleted.\n", current->type,
        current->index, current->name);
    free(current->name);
    free(current->type);
    free(current);
    return 1;
}

static int deleteelement(struct Node **head_ref, int index_to_delete)
{
    struct Node *current = *head_ref;
    struct Node *prev = NULL;

    while (current != NULL) {
        if (current->index == index_to_delete) {
            return tmp(prev, head_ref, current);
        }
        prev = current;
        current = current->next;
    }
    return 0;
}

int del(void *data, char **args)
{
    struct Node **head_ref = (struct Node**)data;
    int index_to_delete;

    if (nb_elem_args(args) == 0)
        return 84;
    for (int i = 0; args[i] != NULL; i++) {
        if (my_str_isnum(args[i]) == 1)
            index_to_delete = my_getnbr(args[i]);
        else
            return 84;
        if (!deleteelement(head_ref, index_to_delete))
            return 84;
        if (args[i + 1] != NULL)
            add(data, &args[i + 1]);
    }
    return 0;
}
