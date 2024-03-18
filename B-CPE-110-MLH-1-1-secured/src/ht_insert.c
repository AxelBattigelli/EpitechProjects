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

static int error_handling(hashtable_t *ht, char *key, char *value, entry_t *n)
{
    if (!n || ht == NULL || !ht->table || !key || !value) {
        write(2, "Memory allocation failed\n", 26);
        return 1;
    }
    if (my_strlen(key) == 0 || my_strlen(value) == 0) {
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

int check_duplicate(hashtable_t *ht, char *key, char *value)
{
    entry_t **begin = &ht->table[(ht->hash(key, ht->size) % 10)];

    for (entry_t *list = *begin; list; list = list->next) {
        if (list->key == ht->hash(key, ht->size)) {
            list->value = value;
            return 1;
        }
    }
    return 0;
}

int ht_insert(hashtable_t *ht, char *key, char *value)
{
    int index;
    entry_t *new_entry = malloc(sizeof(entry_t));
    entry_t **current;

    if (error_handling(ht, key, value, new_entry) == 1)
        return 84;
    index = (ht->hash(key, ht->size)) % ht->size;
    current = &ht->table[index];
    new_entry->key = (ht->hash(key, ht->size));
    new_entry->value = my_strdup(value);
    if (check_duplicate(ht, key, value) == 1)
        return 0;
    new_entry->next = *current;
    *current = new_entry;
    return 0;
}
