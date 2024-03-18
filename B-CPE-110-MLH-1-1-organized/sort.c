/*
** EPITECH PROJECT, 2023
** sort
** File description:
** sort
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "include/shell.h"
#include "include/my.h"

static void swap_items_chained_list(struct Node *a, struct Node *b)
{
    int tmp1 = a->index;
    char *tmp2 = a->name;
    char *tmp3 = a->type;

    a->index = b->index;
    a->name = b->name;
    a->type = b->type;
    b->index = tmp1;
    b->name = tmp2;
    b->type = tmp3;
}

static int sortlist_id_conditionnal(struct Node *ptr1)
{
    if (ptr1->index > ptr1->next->index) {
        swap_items_chained_list(ptr1, ptr1->next);
        return 1;
    }
    return 0;
}

static void sortlist_id(struct Node *head)
{
    int swapped;
    struct Node *ptr1;
    struct Node *lptr = NULL;

    if (head == NULL)
        return;
    do {
        swapped = 0;
        ptr1 = head;
        while (ptr1->next != lptr) {
            swapped = sortlist_id_conditionnal(ptr1);
            ptr1 = ptr1->next;
        }
        lptr = ptr1;
    } while (swapped);
}

static int sortlist_type_conditionnal(struct Node *ptr1)
{
    if (ptr1->type[0] > ptr1->next->type[0]) {
        swap_items_chained_list(ptr1, ptr1->next);
        return 1;
    }
    return 0;
}

static void sortlist_type(struct Node *head)
{
    int swapped;
    struct Node *ptr1;
    struct Node *lptr = NULL;

    if (head == NULL)
        return;
    do {
        swapped = 0;
        ptr1 = head;
        while (ptr1->next != lptr) {
            swapped = sortlist_type_conditionnal(ptr1);
            ptr1 = ptr1->next;
        }
        lptr = ptr1;
    } while (swapped);
}

static int sortlist_name_conditionnal(struct Node *ptr1)
{
    if (my_strcmp(ptr1->name, ptr1->next->name) > 0) {
        swap_items_chained_list(ptr1, ptr1->next);
        return 1;
    }
    return 0;
}

static void sortlist_name(struct Node *head)
{
    int swapped;
    struct Node *ptr1;
    struct Node *lptr = NULL;

    if (head == NULL)
        return;
    do {
        swapped = 0;
        ptr1 = head;
        while (ptr1->next != lptr) {
            swapped = sortlist_name_conditionnal(ptr1);
            ptr1 = ptr1->next;
        }
        lptr = ptr1;
    } while (swapped);
}

static void sortlist_reverse(struct Node *head)
{
    int swapped;
    struct Node *ptr1;
    struct Node *lptr = NULL;

    if (head == NULL)
        return;
    do {
        swapped = 0;
        ptr1 = head;
        while (ptr1->next != lptr) {
            swap_items_chained_list(ptr1, ptr1->next);
            swapped = 1;
            ptr1 = ptr1->next;
        }
        lptr = ptr1;
    } while (swapped);
}

// int sort(void *data, char **args)
// {
//     struct Node **head_ref = (struct Node**)data;
//     int i = 0;

//     if (nb_elem_args(args) < 1)
//         return 84;
//     while (args[i]) {
//         if (my_strcmp(args[i], "TYPE") == 0)
//             sortList_type(*head_ref);
//         if (my_strcmp(args[i], "ID") == 0)
//             sortList_id(*head_ref);
//         if (my_strcmp(args[i], "NAME") == 0)
//             sortList_name(*head_ref);
//         if (my_strcmp(args[i], "-r") == 0)
//             sortList_reverse(*head_ref);
//         i++;
//     }
//     return 0;
// }
int sort(void *data, char **args)
{
    struct Node **head_ref = (struct Node**)data;
    int i = 0;

    if (nb_elem_args(args) < 1)
        return 84;
    while (args[i]) {
        if (my_strcmp(args[i], "TYPE") == 0)
            sortlist_type(*head_ref);
        if (my_strcmp(args[i], "ID") == 0)
            sortlist_id(*head_ref);
        if (my_strcmp(args[i], "NAME") == 0)
            sortlist_name(*head_ref);
        if (my_strcmp(args[i], "-r") == 0)
            sortlist_reverse(*head_ref);
        i++;
    }
    return 0;
}
