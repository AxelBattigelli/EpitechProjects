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
    if (!ht->table || !ht->size) {
        write(2, "Memory allocation error\n", 24);
        return 1;
    }
    return 0;
}

void delete_hashtable(hashtable_t *ht)
{
    entry_t **begin;
    entry_t *temp;

    if (error_handling(ht) == 1)
        return;
    for (int i = 0; i < ht->size; i++) {
        begin = &ht->table[i];
        for (entry_t *current = *begin; current != NULL;) {
            temp = current;
            current = current->next;
            free(temp);
        }
    }
    ht->table = NULL;
    ht = NULL;
}
