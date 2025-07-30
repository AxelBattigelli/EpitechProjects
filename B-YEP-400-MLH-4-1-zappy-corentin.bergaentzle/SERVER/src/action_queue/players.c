/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** players
*/

#include "command.h"
#include "client.h"
#include "game.h"
#include "loop.h"
#include "action.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

char *recover_players(game_t *game, client_t *clients, client_t *clt,
    int level)
{
    const int *requirements = elevation_requirements[level - 1];
    int count = 0;
    char *player_list = calloc(512, sizeof(char));
    tile_t *tile = &game->world->tiles[clt->y][clt->x];
    char *temp;

    for (client_t *othr = clients; othr != NULL; othr = othr->next)
        if (othr->x == clt->x && othr->y == clt->y && othr->level == level) {
            count++;
            temp = malloc(sizeof(char) * 32);
            snprintf(temp, sizeof(temp), " #%d", othr->player_id);
            strcat(player_list, temp);
            free(temp);
        }
    if (count < requirements[0])
        return NULL;
    for (int i = 1; i < 7; i++)
        if (tile->resources[i] < requirements[i])
            return NULL;
    return player_list;
}

int check_incantation(client_t *clients, action_t *oldest, game_t *game)
{
    char *msg = malloc(sizeof(char) * 1024);
    client_t *client = find_by_fd(clients, oldest->client_fd);
    int level = client->level;
    char *player_list = NULL;

    if (level < 1 || level > 7) {
        return 0;
    }
    player_list = recover_players(game, clients, client, level);
    if (player_list == NULL)
        return 0;
    snprintf(msg, 1024, "pic %d %d %d%s\n", client->x, client->y, level,
        player_list);
    send_to_grahicals(clients, msg);
    oldest->started = 1;
    return 1;
}

void check_send_graphical(client_t *current, client_t *clients)
{
    char *msg;

    while (current) {
        msg = malloc(sizeof(char) * 1024);
        if (current->type != CLIENT_AI) {
            current = current->next;
            continue;
        }
        if (current->inventory[FOOD] > 0) {
            current->inventory[FOOD]--;
        } else {
            send(current->fd, "dead\n", 5, 0);
            printf("Client %d died from starvation\n", current->fd);
            snprintf(msg, sizeof(msg), "pdi #%d\n", current->player_id);
            send_to_grahicals(clients, msg);
        }
        current = current->next;
        free(msg);
    }
}

void handle_food_consumption(client_t *clients, server_t *server, game_t *game)
{
    struct timeval current_time;
    long elapsed_ms;
    long food_interval_ms;
    client_t *current = clients;

    gettimeofday(&current_time, NULL);
    elapsed_ms = (current_time.tv_sec -
        game->world->timing.last_food_update.tv_sec) * 1000 +
        (current_time.tv_usec - game->world->timing.last_food_update.tv_usec)
        / 1000;
    food_interval_ms = (126 * 1000) / server->freq;
    if (game->world->timing.last_food_update.tv_sec == 0 ||
        elapsed_ms >= food_interval_ms) {
        check_send_graphical(current, clients);
        game->world->timing.last_food_update = current_time;
    }
}

void handle_resource_spawning(game_t *game, server_t *server, client_t *client)
{
    struct timeval current_time;
    long elapsed_ms;
    long spawn_interval_ms;

    gettimeofday(&current_time, NULL);
    elapsed_ms = (current_time.tv_sec -
        game->world->timing.last_resource_spawn.tv_sec) * 1000 +
        (current_time.tv_usec -
            game->world->timing.last_resource_spawn.tv_usec) / 1000;
    spawn_interval_ms = (20 * 1000) / server->freq;
    if ((game->world->timing.last_resource_spawn.tv_sec == 0) ||
        (elapsed_ms >= spawn_interval_ms)) {
        world_spawn_resources(game->world, client);
        game->world->timing.last_resource_spawn = current_time;
    }
}
