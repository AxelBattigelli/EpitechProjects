/*
** EPITECH PROJECT, 2024
** secured
** File description:
** insert elem hashtable
*/

#include "../include/my.h"
#include "../include/hashtable.h"
#include <stdlib.h>
#include <unistd.h>

static int error_handling(hashtable_t *ht)
{
    if (ht == NULL) {
        write(2, "Memory allocation error\n", 24);
        return 1;
    }
    if (!ht->table) {
        write(2, "Memory allocation error\n", 24);
        return 1;
    }
    return 0;
}

void ht_dump(hashtable_t *ht)
{
    entry_t **begin;

    if (error_handling(ht) == 1)
        return;
    for (int i = 0; i < ht->size; i++) {
        my_printf("[%d]:\n", i);
        begin = &ht->table[i];
        for (entry_t *current = *begin; current; current = current->next) {
            my_printf("> %d - %s\n", current->key, current->value);
        }
    }
}
