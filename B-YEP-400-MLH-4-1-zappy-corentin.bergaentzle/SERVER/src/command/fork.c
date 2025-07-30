/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** client_fork
*/

#include "client.h"
#include "loop.h"
#include "command.h"
#include "action.h"

#include <stdio.h>

static egg_t *create_egg(int x, int y)
{
    egg_t *egg = malloc(sizeof(egg_t));

    if (!egg)
        return NULL;
    egg->x = x;
    egg->y = y;
    egg->next = NULL;
    return egg;
}

static void fork_gui(server_t *server, int i, client_t *client,
    client_t *clients)
{
    char msg[1024];

    snprintf(msg, sizeof(msg), "enw #%d #%d %d %d\n",
        server->teams[i].nb_eggs, client->player_id, client->x,
        client->y);
    send_to_grahicals(clients, msg);
}

int fork_function(action_t *action, client_t *clients, server_t *server,
    game_t *)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    egg_t *new_egg;

    for (int i = 0; server->teams[i].name != NULL; i++) {
        if (strcmp(server->teams[i].name, client->team_name) == 0) {
            new_egg = create_egg(client->x, client->y);
            new_egg->next = server->teams[i].eggs;
            server->teams[i].nb_slot++;
            server->teams[i].eggs = new_egg;
            fork_gui(server, i, client, clients);
            send(client->fd, "ok\n", 3, 0);
            return 0;
        }
    }
    return -1;
}
