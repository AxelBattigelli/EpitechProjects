/*
** EPITECH PROJECT, 2025
** zappy
** File description:
** command_map
*/

#include "loop.h"
#include "parse_utils.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int map_size(client_t *, server_t *server, game_t *, loop_t *loop)
{
    char response[64];

    snprintf(response, sizeof(response), "msz %d %d\n",
        server->width, server->height);
    send(loop->new_socket, response, strlen(response), 0);
    return 0;
}

int tile_content(client_t *, server_t *server, game_t *game,
    loop_t *loop)
{
    tile_t *ti;
    int x;
    int y;
    char res[256];

    if (parse_two_ints(loop->buffer, &x, &y) != 0) {
        send(loop->new_socket, "sbp\n", 4, 0);
        return -1;
    }
    if (x < 0 || x >= server->width || y < 0 || y >= server->height) {
        send(loop->new_socket, "sbp\n", 4, 0);
        return -1;
    }
    ti = &game->world->tiles[y][x];
    snprintf(res, sizeof(res), "bct %d %d %d %d %d %d %d %d %d\n", x, y,
        ti->resources[FOOD], ti->resources[LINEMATE], ti->resources[DERAUMERE],
        ti->resources[SIBUR], ti->resources[MENDIANE],
        ti->resources[PHIRAS], ti->resources[THYSTAME]);
    send(loop->new_socket, res, strlen(res), 0);
    return 0;
}

int map_content(client_t *, server_t *server, game_t *game, loop_t *loop)
{
    tile_t *tile;
    char response[256];

    for (int y = 0; y < server->height; y++) {
        for (int x = 0; x < server->width; x++) {
            tile = &game->world->tiles[y][x];
            snprintf(response, sizeof(response),
                "bct %d %d %d %d %d %d %d %d %d\n",
                x, y,
                tile->resources[FOOD],
                tile->resources[LINEMATE],
                tile->resources[DERAUMERE],
                tile->resources[SIBUR],
                tile->resources[MENDIANE],
                tile->resources[PHIRAS],
                tile->resources[THYSTAME]);
            send(loop->new_socket, response, strlen(response), 0);
        }
    }
    return 0;
}
