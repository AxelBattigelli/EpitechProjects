/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** client
*/

#include "action.h"
#include "loop.h"
#include "command.h"

#include <regex.h>
#include <stdio.h>

static int get_resource_index(const char *name)
{
    for (int i = 0; i < RESOURCE_COUNT; ++i) {
        if (strcmp(resource_names[i], name) == 0)
            return i;
    }
    return -1;
}

static int parse_command_resource_index(const char *command)
{
    int word_count = 0;
    int res_index;
    char *cmd = strdup(command);
    char *word = strtok(cmd, " \n");
    char *words[3] = { NULL };

    while (word != NULL && word_count < 3) {
        words[word_count] = word;
        word_count++;
        word = strtok(NULL, " \n");
    }
    res_index = (words[1] != NULL) ? get_resource_index(words[1]) : -1;
    free(cmd);
    return res_index;
}

static void take_resource(client_t *client, client_t *clients, game_t *game,
    int res_index)
{
    char msg[1024];

    if (game->world->tiles[client->y][client->x].resources[res_index] > 0) {
        game->world->tiles[client->y][client->x].resources[res_index]--;
        client->inventory[res_index]++;
        snprintf(msg, sizeof(msg), "pgt #%d %d\n", client->player_id,
            res_index);
        send_to_grahicals(clients, msg);
        send(client->fd, "ok\n", 3, 0);
    } else {
        send(client->fd, "ko\n", 3, 0);
    }
}

static void take_function(action_t *action, client_t *clients, server_t *,
    game_t *game)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    int res_index = parse_command_resource_index(action->command);

    if (res_index == -1 || client == NULL) {
        send(client->fd, "ko\n", 3, 0);
        return;
    }
    take_resource(client, clients, game, res_index);
}

static void set_resource(client_t *client, client_t *clients, game_t *game,
    int res_index)
{
    char msg[1024];

    if (client->inventory[res_index] > 0) {
        client->inventory[res_index]--;
        game->world->tiles[client->y][client->x].resources[res_index]++;
        snprintf(msg, sizeof(msg), "pdr #%d %d\n", client->player_id,
            res_index);
        send_to_grahicals(clients, msg);
        send(client->fd, "ok\n", 3, 0);
    } else {
        send(client->fd, "ko\n", 3, 0);
    }
}

static void set_function(action_t *action, client_t *clients, server_t *,
    game_t *game)
{
    client_t *client = find_by_fd(clients, action->client_fd);
    int res_index = parse_command_resource_index(action->command);

    if (res_index == -1 || client == NULL) {
        send(client->fd, "ko\n", 3, 0);
        return;
    }
    set_resource(client, clients, game, res_index);
}

void parse_command_complexe(action_t *action, client_t *clients,
    server_t *server, game_t *game)
{
    regex_t regex;
    int reti;

    regcomp(&regex, "^Broadcast .+\n$", REG_EXTENDED);
    reti = regexec(&regex, action->command, 0, NULL, 0);
    regfree(&regex);
    if (reti == 0)
        return broadcast_function(action, clients, server, game);
    regcomp(&regex, "^Take [^ \n]+\n$", REG_EXTENDED);
    reti = regexec(&regex, action->command, 0, NULL, 0);
    regfree(&regex);
    if (reti == 0)
        return take_function(action, clients, server, game);
    regcomp(&regex, "^Set [^ \n]+\n$", REG_EXTENDED);
    reti = regexec(&regex, action->command, 0, NULL, 0);
    regfree(&regex);
    if (reti == 0)
        return set_function(action, clients, server, game);
}
