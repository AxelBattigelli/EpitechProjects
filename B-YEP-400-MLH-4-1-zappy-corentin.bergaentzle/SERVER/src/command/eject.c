/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** client_eject
*/

#include "client.h"
#include "loop.h"
#include "command.h"
#include "action.h"

#include <stdio.h>

static int get_direction(client_t *emit, client_t *dest, game_t *game)
{
    int dx = (dest->x - emit->x + game->world->width) % game->world->width;
    int dy = (dest->y - emit->y + game->world->height) % game->world->height;
    int absolute_dir;

    if (dx > game->world->width / 2)
        dx -= game->world->width;
    if (dy > game->world->height / 2)
        dy -= game->world->height;
    if (dx == 1 && dy == 0)
        return (4 + DIR_WEST - dest->dir);
    if (dx == -1 && dy == 0)
        return (4 + DIR_EAST - dest->dir);
    if (dy == 1 && dx == 0)
        return (4 + DIR_SOUTH - dest->dir);
    if (dy == -1 && dx == 0)
        return (4 + DIR_NORTH - dest->dir);
    else
        return 0;
}

static void get_direction_offset(int dir, int *dx, int *dy)
{
    *dx = 0;
    *dy = 0;
    switch (dir) {
        case DIR_NORTH:
            *dy = 1;
            break;
        case DIR_EAST:
            *dx = 1;
            break;
        case DIR_SOUTH:
            *dy = -1;
            break;
        case DIR_WEST:
            *dx = -1;
            break;
    }
}

static void eject_msg(int k, client_t *c)
{
    char msg[1024];

    snprintf(msg, sizeof(msg), "eject: %d\n", k + 1);
    send(c->fd, msg, strlen(msg), 0);
}

static int eject_clients_at_position(client_t *ejector,
    client_t *clients, game_t *game)
{
    int dx;
    int dy;
    int ejected_any = 0;
    int k;

    get_direction_offset(ejector->dir, &dx, &dy);
    for (client_t *c = clients; c; c = c->next) {
        if (c == ejector || c->x != ejector->x ||
            c->y != ejector->y || c->type != CLIENT_AI)
            continue;
        c->x = (c->x + dx + game->world->width) % game->world->width;
        c->y = (c->y + dy + game->world->height) % game->world->height;
        k = get_direction(ejector, c, game);
        eject_msg(k, c);
        ejected_any = 1;
    }
    return ejected_any;
}

static void delete_egg_from_list(egg_t **head, egg_t *prev, egg_t *curr)
{
    if (prev == NULL) {
        *head = curr->next;
    } else {
        prev->next = curr->next;
    }
    free(curr);
}

static void delete_eggs_from_team(team_t *team, int x, int y)
{
    egg_t *prev = NULL;
    egg_t *curr = team->eggs;

    while (curr != NULL) {
        if (curr->x == x && curr->y == y) {
            printf("Deleting egg at x = %d, y = %d\n", curr->x, curr->y);
            delete_egg_from_list(&team->eggs, prev, curr);
            curr = (prev == NULL) ? team->eggs : prev->next;
        } else {
            prev = curr;
            curr = curr->next;
        }
    }
}

static void delete_eggs_at_position(server_t *server, int x, int y)
{
    for (int i = 0; i < server->team_count; i++) {
        delete_eggs_from_team(&server->teams[i], x, y);
    }
}

int eject_function(action_t *action, client_t *clients,
    server_t *server, game_t *game)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    int ejected_any = eject_clients_at_position(client, clients, game);
    char msg[1024];

    delete_eggs_at_position(server, client->x, client->y);
    snprintf(msg, sizeof(msg), "pex #%d\n", client->player_id);
    send_to_grahicals(clients, msg);
    if (ejected_any)
        send(client->fd, "ok\n", 3, 0);
    else
        send(client->fd, "ko\n", 3, 0);
    return 0;
}
