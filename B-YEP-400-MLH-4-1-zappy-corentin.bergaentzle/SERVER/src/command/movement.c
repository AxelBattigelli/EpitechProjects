/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** client_movement
*/

#include "client.h"
#include "loop.h"
#include "command.h"
#include "action.h"

#include <stdio.h>

static void which_direction(client_t *client, server_t *server)
{
    switch (client->dir) {
        case DIR_NORTH:
            client->y = (client->y - 1 + server->height) % server->height;
            break;
        case DIR_EAST:
            client->x = (client->x + 1) % server->width;
            break;
        case DIR_SOUTH:
            client->y = (client->y + 1) % server->height;
            break;
        case DIR_WEST:
            client->x = (client->x - 1 + server->width) % server->width;
            break;
    }
}

int forward_function(action_t *action, client_t *clients, server_t *server,
    game_t *)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    char msg[1024];

    which_direction(client, server);
    snprintf(msg, sizeof(msg), "ppo #%d %d %d %d\n", client->player_id,
        client->x, client->y, client->dir);
    send_to_grahicals(clients, msg);
    printf("Client moved to (%d, %d)\n", client->x, client->y);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}

int right_function(action_t *action, client_t *clients, server_t *, game_t *)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    char msg[1024];

    client->dir = (client->dir + 1) % 4;
    snprintf(msg, sizeof(msg), "ppo #%d %d %d %d\n", client->player_id,
        client->x, client->y, client->dir);
    send_to_grahicals(clients, msg);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}

int left_function(action_t *action, client_t *clients, server_t *, game_t *)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    char msg[1024];

    client->dir = (client->dir + 3) % 4;
    snprintf(msg, sizeof(msg), "ppo #%d %d %d %d\n", client->player_id,
        client->x, client->y, client->dir);
    send_to_grahicals(clients, msg);
    send(client->fd, "ok\n", 3, 0);
    return 0;
}
