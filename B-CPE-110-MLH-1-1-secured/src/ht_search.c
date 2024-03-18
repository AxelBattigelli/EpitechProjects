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
    if (ht == NULL || !ht->table || !key) {
        write(2, "Memory allocation failed\n", 26);
        return 1;
    }
    if (my_strlen(key) == 0) {
        write(2, "Value or Key empty\n", 19);
        return 1;
    }
    if (!ht->size || ht->size <= 0) {
        write(2, "Invalid size\n", 13);
        return 1;
    }
    if (!ht->hash) {
        write(2, "Invalid hash fonction assigned\n", 31);
        return 1;
    }
    return 0;
}

char *ht_search(hashtable_t *ht, char *key)
{
    int index;
    entry_t *current;

    if (error_handling(ht, key) == 1)
        return "Error 84";
    index = ht->hash(key, ht->size);
    current = ht->table[index % ht->size];
    while (current) {
        if (index == current->key)
            return current->value;
        current = current->next;
    }
    return "Key not found";
}
