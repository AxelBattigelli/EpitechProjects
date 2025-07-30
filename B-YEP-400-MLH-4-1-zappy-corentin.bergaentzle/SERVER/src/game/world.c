/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** game_world
*/

#include "game.h"
#include "command.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>

static int max_ressources(world_t *world, int res, int max)
{
    int count = 0;

    for (int i = 0; i < world->height; i++) {
        for (int j = 0; j < world->width; j++) {
            count += world->tiles[i][j].resources[res];
        }
    }
    if (count >= max)
        return 1;
    return 0;
}

void make_snprintf(world_t *world, int x, int y, client_t *clients)
{
    char *msg = malloc(sizeof(char) * 1024);

    snprintf(msg, 1024,
        "bct %d %d %d %d %d %d %d %d %d\n",
        x, y,
        world->tiles[x][y].resources[FOOD],
        world->tiles[x][y].resources[LINEMATE],
        world->tiles[x][y].resources[DERAUMERE],
        world->tiles[x][y].resources[SIBUR],
        world->tiles[x][y].resources[MENDIANE],
        world->tiles[x][y].resources[PHIRAS],
        world->tiles[x][y].resources[THYSTAME]);
    send_to_grahicals(clients, msg);
}

void world_spawn_resources(world_t *world, client_t *clients)
{
    int total_tiles = world->width * world->height;
    int quantity = 0;
    int x;
    int y;

    for (int res = 0; res < RESOURCE_COUNT; res++) {
        quantity = total_tiles * resource_density[res];
        if (max_ressources(world, res, quantity) == 1)
            continue;
        if (quantity == 0)
            quantity = 1;
        for (int i = 0; i < quantity; i++) {
            x = rand() % world->width;
            y = rand() % world->height;
            world->tiles[y][x].resources[res]++;
            make_snprintf(world, x, y, clients);
        }
    }
}
