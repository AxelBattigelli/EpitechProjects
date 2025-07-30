/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** game_init
*/

#include "game.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>

game_t *game_init(server_t *server)
{
    game_t *game = calloc(1, sizeof(game_t));

    if (!game)
        return NULL;
    game->world = world_create(server->width, server->height);
    if (!game->world) {
        free(game);
        return NULL;
    }
    gettimeofday(&game->world->timing.last_resource_spawn, NULL);
    game->world->timing.last_food_update = (struct timeval){0, 0};
    game->world->timing.last_game_update = (struct timeval){0, 0};
    game->world->timing.time_unit_counter = 0;
    gettimeofday(&game->world->last_resource_spawn, NULL);
    return game;
}

static void init_resources(world_t *world, int x, int y)
{
    for (int res = 0; res < RESOURCE_COUNT; res++) {
        world->tiles[y][x].resources[res] = 0;
    }
}

world_t *world_create(int width, int height)
{
    world_t *world = calloc(1, sizeof(world_t));

    if (!world)
        return NULL;
    world->width = width;
    world->height = height;
    world->time_unit = 100;
    world->tiles = calloc(height, sizeof(tile_t *));
    for (int y = 0; y < height; y++) {
        world->tiles[y] = calloc(width, sizeof(tile_t));
        for (int x = 0; x < width; x++) {
            init_resources(world, x, y);
            world->tiles[y][x].players = NULL;
            world->tiles[y][x].player_count = 0;
            world->tiles[y][x].egg_count = 0;
        }
    }
    return world;
}

static void delete_game_lists(game_t *game)
{
    action_t *action_next = NULL;
    action_t *current = NULL;
    egg_t *egg_next = NULL;
    egg_t *egg = NULL;

    current = game->action_queue;
    while (current) {
        action_next = current->next;
        free(current->command);
        free(current);
        current = action_next;
    }
    while (egg) {
        egg_next = egg->next;
        free(egg);
        egg = egg_next;
    }
}

void game_destroy(game_t *game)
{
    if (!game)
        return;
    if (game->world) {
        world_destroy(game->world);
    }
    delete_game_lists(game);
    if (game->winning_team) {
        free(game->winning_team);
    }
    free(game);
}

static void free_tile(world_t *world, int x, int y)
{
    if (world->tiles[y][x].players) {
        free(world->tiles[y][x].players);
    }
}

void world_destroy(world_t *world)
{
    if (!world)
        return;
    for (int y = 0; y < world->height; y++) {
        for (int x = 0; x < world->width; x++) {
            free_tile(world, x, y);
        }
        free(world->tiles[y]);
    }
    if (world->tiles) {
        free(world->tiles);
    }
    free(world);
}
