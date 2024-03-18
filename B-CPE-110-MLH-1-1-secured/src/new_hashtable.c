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

static int error_handling(hashtable_t *ht, int (*hash)(char *, int), int len)
{
    if (ht == NULL) {
        write(2, "Memory allocation failed\n", 26);
        return 1;
    }
    if (len <= 0) {
        write(2, "Invalid size\n", 13);
        return 1;
    }
    if (hash == NULL) {
        write(2, "Invalid hash fonction assigned\n", 31);
        return 1;
    }
    return 0;
}

hashtable_t *new_hashtable(int (*hash)(char *, int), int len)
{
    hashtable_t *ht = malloc(sizeof(hashtable_t));
    int i = 0;

    if (error_handling(ht, hash, len) == 1)
        return NULL;
    ht->size = len;
    ht->table = malloc(sizeof(entry_t) * len);
    for (; i < len; i++) {
        ht->table[i] = malloc(sizeof(entry_t));
        ht->table[i] = 0;
    }
    ht->table[i] = NULL;
    ht->hash = hash;
    return ht;
}
