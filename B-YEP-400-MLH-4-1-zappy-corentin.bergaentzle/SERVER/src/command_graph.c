/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** command_graph
*/

#include "client.h"
#include "loop.h"
#include "command.h"
#include "action.h"
#include "parse_utils.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int teams_name(client_t *, server_t *server, game_t *, loop_t *loop)
{
    char res[64];

    for (int i = 0; i < server->team_count; i++) {
        snprintf(res, sizeof(res), "tna %s\n", server->teams[i].name);
        send(loop->new_socket, res, strlen(res), 0);
    }
    return 0;
}

int time_unit(client_t *, server_t *server, game_t *, loop_t *loop)
{
    char response[64];

    snprintf(response, sizeof(response), "sgt %d\n", server->freq);
    send(loop->new_socket, response, strlen(response), 0);
    return 0;
}

int change_time_unit(client_t *, server_t *server, game_t *, loop_t *loop)
{
    int new_freq;
    char response[64];

    if (parse_int(loop->buffer, &new_freq) != 0 || new_freq <= 0) {
        send(loop->new_socket, "sbp\n", 4, 0);
        return -1;
    }
    server->freq = new_freq;
    snprintf(response, sizeof(response), "sst %d\n", new_freq);
    send(loop->new_socket, response, strlen(response), 0);
    return 0;
}

void send_to_grahicals(client_t *clients, char *message)
{
    for (client_t *current = clients; current != NULL;
        current = current->next) {
        if (current->type == CLIENT_GRAPHIC)
            send(current->fd, message, strlen(message), 0);
    }
}
