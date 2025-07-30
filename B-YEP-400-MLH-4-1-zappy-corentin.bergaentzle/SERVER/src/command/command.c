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

static void inventory_gui(client_t *clients, client_t *client)
{
    char msg[1024];

    snprintf(msg, sizeof(msg), "pin #%d %d %d %d %d %d %d %d %d %d\n",
        client->player_id, client->x, client->y, client->inventory[FOOD],
        client->inventory[LINEMATE], client->inventory[DERAUMERE],
        client->inventory[SIBUR], client->inventory[MENDIANE],
        client->inventory[PHIRAS], client->inventory[THYSTAME]);
    send_to_grahicals(clients, msg);
}

static void inventory_loop(client_t *client, char *res, int i)
{
    char buffer[64];

    snprintf(buffer, sizeof(buffer), "%s %d",
        resource_names[i], client->inventory[i]);
    strcat(res, buffer);
    if (i < RESOURCE_COUNT - 1)
        strcat(res, ", ");
}

int inventory_function(action_t *action, client_t *clients,
    server_t *, game_t *)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    char res[1024];

    strcpy(res, "[");
    for (int i = 0; i < RESOURCE_COUNT; i++) {
        inventory_loop(client, res, i);
    }
    strcat(res, "]\n");
    send(client->fd, res, strlen(res), 0);
    inventory_gui(clients, client);
    return 0;
}

int get_index(server_t *server, client_t *client)
{
    if (!client)
        return -1;
    for (int i = 0; i < server->team_count; i++) {
        printf("team: %s\n", server->teams[i].name);
        if (strcmp(server->teams[i].name, client->team_name) == 0)
            return i;
    }
    return -1;
}

int connect_nbr_function(action_t *action, client_t *clients, server_t *server,
    game_t *)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    int idx = get_index(server, client);
    int count = server->teams[idx].nb_slot;
    char msg[32];

    for (client_t *current = clients; current != NULL;
        current = current->next) {
        if (idx < 0) {
            count = 0;
            break;
        }
        if (current->type == CLIENT_AI && strcmp(current->team_name,
            client->team_name) == 0) {
            count--;
        }
    }
    snprintf(msg, sizeof(msg), "%d\n", count);
    send(client->fd, msg, strlen(msg), 0);
    return 0;
}
