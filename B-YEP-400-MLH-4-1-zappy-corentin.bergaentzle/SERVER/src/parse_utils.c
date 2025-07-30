/*
** EPITECH PROJECT, 2025
** zappy
** File description:
** parse_int
*/

#include <limits.h>
#include <stdlib.h>
#include <string.h>

int parse_int(const char *str, int *result)
{
    char *endptr;
    long val;

    if (!str || !result)
        return -1;
    val = strtol(str, &endptr, 10);
    if (endptr == str || *endptr != '\0' || val < 0 || val > INT_MAX)
        return -1;
    *result = (int)val;
    return 0;
}

int parse_player_id(const char *params, int *player_id)
{
    if (!params || !player_id)
        return -1;
    if (params[0] != '#')
        return -1;
    return parse_int(params + 1, player_id);
}

int parse_two_ints(const char *params, int *x, int *y)
{
    int result = -1;
    char *params_copy;
    char *first;
    char *second;
    char *extra;

    if (!params || !x || !y)
        return -1;
    params_copy = strdup(params);
    if (!params_copy)
        return -1;
    first = strtok(params_copy, " ");
    second = strtok(NULL, " ");
    extra = strtok(NULL, " ");
    if (first && second && !extra) {
        if (parse_int(first, x) == 0 && parse_int(second, y) == 0)
            result = 0;
    }
    free(params_copy);
    return result;
}
