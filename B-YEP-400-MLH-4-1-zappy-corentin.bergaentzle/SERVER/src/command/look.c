/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** look command
*/

#include "client.h"
#include "loop.h"g
#include "command.h"
#include "action.h"

#include <stdio.h>

static int get_tile_x(int cx, int depth, int offset, int dir)
{
    switch (dir) {
        case 0:
            return cx + offset;
        case 1:
            return cx + depth;
        case 2:
            return cx - offset;
        case 3:
            return cx - depth;
        default:
            return cx;
    }
}

static int get_tile_y(int cy, int depth, int offset, int dir)
{
    switch (dir) {
        case 0:
            return cy - depth;
        case 1:
            return cy + offset;
        case 2:
            return cy + depth;
        case 3:
            return cy - offset;
        default:
            return cy;
    }
}

static void add_all_players(client_t *clients, int tx, int ty, char *buf)
{
    for (client_t *c = clients; c; c = c->next) {
        if (c->x != tx || c->y != ty || c->type != CLIENT_AI)
            continue;
        strcat(buf, " player");
    }
}

static void add_all_resources(tile_t *tile, char *buf)
{
    for (int i = 0; i < RESOURCE_COUNT; i++) {
        for (int k = 0; k < tile->resources[i]; k++) {
            strcat(buf, " ");
            strcat(buf, resource_names[i]);
        }
    }
}

static void finalize_look_response(char *res)
{
    int len = strlen(res);

    if (len > 1 && res[len - 1] == ',') {
        res[len - 1] = ']';
        res[len] = '\n';
        res[len + 1] = '\0';
    } else {
        strcat(res, "]\n");
    }
}

static void build_look_tiles(client_t *client, client_t *clients,
    world_t *world, char *res)
{
    int tx;
    int ty;
    tile_t *tile = NULL;
    char tile_buf[1024];

    for (int depth = 0; depth <= client->level; depth++) {
        for (int offset = -depth; offset <= depth; offset++) {
            tx = get_tile_x(client->x, depth, offset, client->dir);
            ty = get_tile_y(client->y, depth, offset, client->dir);
            tx = (tx + world->width) % world->width;
            ty = (ty + world->height) % world->height;
            tile = &world->tiles[ty][tx];
            tile_buf[0] = '\0';
            add_all_players(clients, tx, ty, tile_buf);
            add_all_resources(tile, tile_buf);
            strcat(res, tile_buf);
            strcat(res, ",");
        }
    }
}

int look_function(action_t *action, client_t *clients, server_t *,
    game_t *game)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    world_t *world = game->world;
    char res[8192] = "[";

    build_look_tiles(client, clients, world, res);
    finalize_look_response(res);
    send(client->fd, res, strlen(res), 0);
    return 0;
}
