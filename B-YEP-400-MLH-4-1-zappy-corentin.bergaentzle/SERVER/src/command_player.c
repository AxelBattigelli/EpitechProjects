/*
** EPITECH PROJECT, 2025
** zappy
** File description:
** command_player
*/

#include "client.h"
#include "loop.h"
#include "parse_utils.h"
#include "action.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int players_position(client_t *client, server_t *, game_t *, loop_t *loop)
{
    client_t *target;
    int player_id;
    char response[64];

    if (parse_player_id(loop->buffer, &player_id) != 0) {
        send(loop->new_socket, "sbp\n", 4, 0);
        return -1;
    }
    target = find_client_by_id(client, player_id);
    if (!target || target->type != CLIENT_AI) {
        send(loop->new_socket, "sbp\n", 4, 0);
        return -1;
    }
    snprintf(response, sizeof(response), "ppo #%d %d %d %d\n",
        player_id, target->x, target->y, target->dir + 1);
    send(loop->new_socket, response, strlen(response), 0);
    return 0;
}

int players_level(client_t *client, server_t *, game_t *, loop_t *loop)
{
    client_t *target;
    int player_id;
    char response[64];

    if (parse_player_id(loop->buffer, &player_id) != 0) {
        send(loop->new_socket, "sbp\n", 4, 0);
        return -1;
    }
    target = find_client_by_id(client, player_id);
    if (!target || target->type != CLIENT_AI) {
        send(loop->new_socket, "sbp\n", 4, 0);
        return -1;
    }
    snprintf(response, sizeof(response), "plv #%d %d\n", player_id,
        target->level);
    send(loop->new_socket, response, strlen(response), 0);
    return 0;
}

int players_inventory(client_t *client, server_t *, game_t *, loop_t *loop)
{
    client_t *target;
    int player_id;
    char res[256];

    if (parse_player_id(loop->buffer, &player_id) != 0) {
        send(loop->new_socket, "sbp\n", 4, 0);
        return -1;
    }
    target = find_client_by_id(client, player_id);
    if (!target || target->type != CLIENT_AI) {
        send(loop->new_socket, "sbp\n", 4, 0);
        return -1;
    }
    snprintf(res, sizeof(res), "pin #%d %d %d %d %d %d %d %d %d %d\n",
        player_id, target->x, target->y, target->inventory[FOOD],
        target->inventory[LINEMATE], target->inventory[DERAUMERE],
        target->inventory[SIBUR], target->inventory[MENDIANE],
        target->inventory[PHIRAS], target->inventory[THYSTAME]);
    send(loop->new_socket, res, strlen(res), 0);
    return 0;
}
