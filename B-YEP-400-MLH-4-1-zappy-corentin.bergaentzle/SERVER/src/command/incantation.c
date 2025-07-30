/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** client_incantation
*/

#include "client.h"
#include "loop.h"
#include "command.h"
#include "action.h"

#include <stdio.h>

static int count_levels(client_t *clients, int x, int y, int level)
{
    int count = 0;

    for (client_t *other = clients; other != NULL; other = other->next) {
        if (other->x == x && other->y == y && other->level == level)
            count++;
    }
    return count;
}

static int check_resources_available(tile_t *tile, const int *requirements)
{
    for (int i = 1; i < 7; i++) {
        if (tile->resources[i] < requirements[i])
            return 0;
    }
    return 1;
}

static void consume_resources(tile_t *tile, const int *requirements)
{
    for (int i = 1; i < 7; i++) {
        tile->resources[i] -= requirements[i];
    }
}

static void level_msg(client_t *client, client_t *clients)
{
    char buffer[64];
    char msg[1024];

    snprintf(msg, sizeof(msg), "plv #%d %d\n",
        client->player_id, client->level);
    send_to_grahicals(clients, msg);
    snprintf(buffer, sizeof(buffer), "Current level: %d\n", client->level);
    send(client->fd, buffer, strlen(buffer), 0);
}

static void elevate_players_at_position(client_t *clients, int x,
    int y, int level)
{
    for (client_t *other = clients; other != NULL; other = other->next) {
        if (other->x == x && other->y == y && other->level == level) {
            other->level++;
            send(other->fd, "Elevation underway\n", 20, 0);
            level_msg(other, clients);
        }
    }
}

int incantation_function(action_t *action, client_t *clients,
    server_t *, game_t *game)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    int level = client->level;
    const int *requirements = elevation_requirements[level - 1];
    tile_t *tile = &game->world->tiles[client->y][client->x];

    if (level < 1 || level > 7) {
        send(client->fd, "ko\n", 3, 0);
        return -1;
    }
    if (count_levels(clients, client->x, client->y, level) < requirements[0]) {
        send(client->fd, "ko\n", 3, 0);
        return 0;
    }
    if (!check_resources_available(tile, requirements)) {
        send(client->fd, "ko\n", 3, 0);
        return 0;
    }
    consume_resources(tile, requirements);
    elevate_players_at_position(clients, client->x, client->y, level);
    return 1;
}
