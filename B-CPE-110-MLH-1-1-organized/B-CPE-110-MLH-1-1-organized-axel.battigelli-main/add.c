/*
** EPITECH PROJECT, 2023
** add
** File description:
** add function
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "include/shell.h"
#include "include/my.h"

static int check_type(const char *nw_typ)
{
    if (my_strcmp(nw_typ, "ACTUATOR") != 0 &&
        my_strcmp(nw_typ, "DEVICE") != 0 &&
        my_strcmp(nw_typ, "PROCESSOR") != 0 &&
        my_strcmp(nw_typ, "SENSOR") != 0 &&
        my_strcmp(nw_typ, "WIRE") != 0) {
        return 84;
    }
    return 0;
}

static void initialize_node(struct Node *new_node, const char *nw_nam,
    const char *nw_typ, struct Node **head_ref)
{
    new_node->index = read_nb("id.txt");
    new_node->name = my_strdup(nw_nam);
    new_node->type = my_strdup(nw_typ);
    new_node->next = *head_ref;
    my_printf("%s n°%d - \"%s\" added.\n", nw_typ, new_node->index, nw_nam);
    *head_ref = new_node;
    increment_digit_nb("id.txt");
}

int add(void *data, char **args)
{
    struct Node **head_ref = (struct Node **)data;
    struct Node *new_node = (struct Node *)malloc(sizeof(struct Node));
    char *nw_nam;
    char *nw_typ;

    if (nb_elem_args(args) < 2 || nb_elem_args(args) % 2 != 0)
        return 84;
    nw_nam = args[1];
    nw_typ = args[0];
    if (check_type(nw_typ) == 84) {
        free(new_node);
        return 84;
    }
    initialize_node(new_node, nw_nam, nw_typ, head_ref);
    if (args[2] != NULL)
        add(data, &args[2]);
    else
        return 0;
}
