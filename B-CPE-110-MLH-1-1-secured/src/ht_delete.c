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

static int error_handling(hashtable_t *ht, char *key)
{
    if (ht == NULL || !ht->table || !ht->size || !ht->hash) {
        write(2, "Memory allocation error\n", 24);
        return 1;
    }
    if (my_strlen(key) == 0) {
        write(2, "Key is empty\n", 13);
        return 1;
    }
    return 0;
}

int ht_delete(hashtable_t *ht, char *key)
{
    int i;
    entry_t *prev = NULL;

    if (error_handling(ht, key) == 1)
        return 84;
    i = ht->hash(key, ht->size);
    for (entry_t *list = ht->table[i % ht->size]; list; list = list->next) {
        if (i == list->key && prev) {
            prev->next = list->next;
            free(list);
            return 0;
        }
        if (i == list->key && !prev) {
            ht->table[i % ht->size] = list->next;
            free(list);
            return 0;
        }
        prev = list;
    }
    return 1;
}
