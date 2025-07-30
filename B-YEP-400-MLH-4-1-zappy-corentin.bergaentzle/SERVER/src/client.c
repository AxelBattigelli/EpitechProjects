/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** loop
*/

#include "loop.h"
#include "parse_args.h"
#include "client.h"
#include "server.h"
#include "command.h"

#include <stdio.h>

int check_client_id(client_t *crt, int id, char *message)
{
    if (crt->unique_id == id) {
        if (crt->type == CLIENT_UNKNOWN || crt->type == CLIENT_WAITING)
            return 1;
        send(crt->fd, message, strlen(message), 0);
    }
    return 0;
}

int send_to_id(client_t *clts, int id, char *message)
{
    for (client_t *crt = clts; crt != NULL; crt = crt->next) {
        if (check_client_id(crt, id, message) == 1)
            return 1;
    }
    return 1;
}

static int is_valid_team(server_t *server, const char *team_name)
{
    for (int i = 0; server->teams[i].name; i++) {
        if (strcmp(team_name, server->teams[i].name) == 0)
            return 1;
    }
    return 0;
}

void handle_new_connection(loop_t *loop, server_t *, client_t **clients)
{
    int fd = loop->new_socket;
    client_t *new_client = calloc(1, sizeof(client_t));

    if (!new_client) {
        close(fd);
        return;
    }
    new_client->fd = fd;
    new_client->type = CLIENT_WAITING;
    new_client->player_id = -1;
    new_client->team_name = NULL;
    new_client->next = *clients;
    *clients = new_client;
    send(fd, "WELCOME\n", 8, 0);
    printf("New connection (fd=%d), waiting for team name\n", fd);
}

static void send_basic_info(client_t *client, server_t *server)
{
    char msg[1024];
    char msg2[1024];

    snprintf(msg, sizeof(msg), "msz %d %d\n", server->width, server->height);
    snprintf(msg2, sizeof(msg2), "sgt %d\n", server->freq);
    send(client->fd, msg2, strlen(msg2), 0);
    send(client->fd, msg, strlen(msg), 0);
}

bool process_team_or_graphic(client_t *client, char *buffer, server_t *server)
{
    buffer[strcspn(buffer, "\n")] = '\0';
    if (strcmp(buffer, "GRAPHIC") == 0) {
        client->type = CLIENT_GRAPHIC;
        client->unique_id = server->unique_id;
        server->unique_id++;
        send_basic_info(client, server);
        printf("Client (fd=%d) is now a graphic client\n", client->fd);
        return false;
    }
    if (!is_valid_team(server, buffer)) {
        printf("Client (fd=%d) sent invalid team: %s\n", client->fd, buffer);
        send(client->fd, "ko\n", 3, 0);
        return false;
    }
    return true;
}

static void remove_egg(server_t *server, int team_index)
{
    egg_t *egg_to_remove = server->teams[team_index].eggs;

    server->teams[team_index].eggs = egg_to_remove->next;
    free(egg_to_remove);
    server->teams[team_index].nb_eggs--;
}

static bool check_team_eggs(server_t *server, client_t *client, char *buffer)
{
    if (server->teams[get_index(server, client)].nb_slot <= 0) {
        printf("Client (fd=%d) sent too much client in team: %s\n",
            client->fd, buffer);
        send(client->fd, "ko\n", 3, 0);
        return false;
    }
    if (server->teams[get_index(server, client)].eggs) {
        client->x = server->teams[get_index(server, client)].eggs->x;
        client->y = server->teams[get_index(server, client)].eggs->y;
        remove_egg(server, get_index(server, client));
    } else {
        client->x = rand() % server->width;
        client->y = rand() % server->height;
    }
    return true;
}

static void send_messages(client_t *client, char *buffer, server_t *server,
    client_t *clients)
{
    char *msg = malloc(sizeof(char) * 64);
    char *msg2 = malloc(sizeof(char) * 64);

    snprintf(msg, 64, "%d\n%d %d\n",
        server->clients_per_team, server->width, server->height);
    send(client->fd, msg, strlen(msg), 0);
    snprintf(msg2, 64, "pnw #%d %d %d %d %d %s\n",
        client->player_id, client->x, client->y, client->dir + 1,
        client->level, client->team_name);
    send_to_grahicals(clients, msg2);
    printf("Client (fd=%d) joined team '%s' at (%d,%d)\n",
        client->fd, buffer, client->x, client->y);
}

void handle_team_selection(client_t *client, char *buffer, server_t *server,
    client_t *clients)
{
    if (!process_team_or_graphic(client, buffer, server))
        return;
    printf("%d\n", server->player_id);
    client->team_name = strdup(buffer);
    client->type = CLIENT_AI;
    client->level = 1;
    client->player_id = server->player_id;
    client->unique_id = server->unique_id;
    server->unique_id++;
    server->player_id++;
    if (!check_team_eggs(server, client, buffer))
        return;
    client->dir = rand() % 4;
    client->inventory[0] = 10;
    send_messages(client, buffer, server, clients);
}
