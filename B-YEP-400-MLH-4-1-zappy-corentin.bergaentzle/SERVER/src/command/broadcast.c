/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** client
*/

#include "client.h"
#include "loop.h"
#include "command.h"
#include "action.h"

#include <stdio.h>
#include <math.h>

static int get_shorter(int coord1, int coord2, int lenght)
{
    int res = coord2 - coord1;

    if (abs(res) > lenght / 2 && res > 0)
        res = res - lenght;
    if (abs(res) > lenght / 2 && res < 0)
        res = lenght + res;
    return res;
}

static int get_origine(int x, int y, client_t *reciever, client_t *sender)
{
    if (reciever->x == sender->x && reciever->y == sender->y)
        return 0;
    if (y > 0 && sender->x == reciever->x)
        return 5;
    if (y < 0 && sender->x == reciever->x)
        return 1;
    if (sender->y == reciever->y && x > 0)
        return 3;
    if (sender->y == reciever->y && x < 0)
        return 7;
    if (y > 0 && x > 0)
        return 4;
    if (y > 0 && x < 0)
        return 6;
    if (y < 0 && x > 0)
        return 2;
    if (y < 0 && x < 0)
        return 8;
    return 0;
}

int get_sound_direction(client_t *sender, client_t *receiver, game_t *game)
{
    int res;
    int x = get_shorter(sender->x, receiver->x, game->world->width);
    int y = get_shorter(sender->y, receiver->y, game->world->height);

    if (sender->x == receiver->x && sender->y == receiver->y)
        return 0;
    res = get_origine(x, y, receiver, sender);
    if (receiver->dir == DIR_EAST)
        res += 2;
    if (receiver->dir == DIR_SOUTH)
        res += 4;
    if (receiver->dir == DIR_WEST)
        res += 6;
    if (res > 8)
        res -= 8;
    return res;
}

static void send_msg(action_t *action, client_t *clients, client_t *client,
    char *text)
{
    char msg[1024];

    snprintf(msg, sizeof(msg), "pbc #%d %s", client->player_id,
        action->command);
    send_to_grahicals(clients, msg);
    send(client->fd, "ok\n", 3, 0);
    free(text);
}

void broadcast_function(action_t *action, client_t *clients,
    server_t *server, game_t *game)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    int direction;
    const char *prefix = "Broadcast ";
    char *txt;
    char result[1024];

    if (strncmp(action->command, prefix, strlen(prefix)) == 0)
        txt = strdup(action->command + strlen(prefix));
    else
        txt = strdup(action->command);
    for (client_t *other = clients; other != NULL; other = other->next) {
        if (other->type == CLIENT_AI &&
            other->unique_id != client->unique_id) {
            direction = get_sound_direction(client, other, game);
            snprintf(result, sizeof(result), "message %d, %s", direction, txt);
            send(other->fd, result, strlen(result), 0);
        }
    }
    send_msg(action, clients, client, txt);
}
