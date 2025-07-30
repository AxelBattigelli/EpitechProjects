/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** oldest
*/

#include "command.h"
#include "client.h"
#include "game.h"
#include "loop.h"
#include "action.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

int get_oldest(action_t *current, action_t **oldest, int client_fd)
{
    action_t *temp = *oldest;

    if (current->client_fd == client_fd) {
        if (!*oldest) {
            *oldest = current;
            return 1;
        }
        if (current->start_time.tv_sec <= temp->start_time.tv_sec ||
            (current->start_time.tv_sec == temp->start_time.tv_sec &&
                current->start_time.tv_usec <= temp->start_time.tv_usec)) {
            *oldest = current;
        }
        if (current->priority == true) {
            *oldest = current;
            return 2;
        }
    }
    return 0;
}

void loop_oldest(action_t *current, action_t **oldest, int client_fd)
{
    int return_value = 0;

    while (current) {
        return_value = get_oldest(current, oldest, client_fd);
        if (return_value == 1)
            continue;
        if (return_value == 2)
            break;
        current = current->next;
    }
}

action_t *manage_incantation(client_t *clients, action_t *oldest, game_t *game,
    client_t *client)
{
    if (strcmp("Incantation\n", oldest->command) == 0) {
        if (check_incantation(clients, oldest, game) == 0) {
            remove_specific_action(game, oldest);
            send(client->fd, "ko\n", 3, 0);
            return NULL;
        }
        oldest->started = 1;
    }
    if (strcmp("Incantation_wait\n", oldest->command) == 0) {
        if (check_incantation(clients, oldest, game) == 0) {
            remove_specific_action(game, oldest);
            return NULL;
        }
        oldest->started = 1;
    }
    return oldest;
}

action_t *check_execution(client_t *clients, action_t *oldest, game_t *game,
    client_t *client)
{
    char *msg = malloc(sizeof(char) * 1024);

    if (strcmp("Fork\n", oldest->command) == 0) {
        snprintf(msg, sizeof(msg), "pfk #%d\n", client->player_id);
        send_to_grahicals(clients, msg);
        oldest->started = 1;
    }
    oldest = manage_incantation(clients, oldest, game, client);
    return oldest;
}

action_t *find_oldest_client_action(client_t *clients, int client_fd,
    int freq, game_t *game)
{
    action_t *oldest = NULL;
    action_t *current = game->action_queue;
    client_t *client;

    loop_oldest(current, &oldest, client_fd);
    if (!oldest)
        return NULL;
    client = find_by_fd(clients, oldest->client_fd);
    if (!oldest->started && client)
        oldest = check_execution(clients, oldest, game, client);
    if (!oldest)
        return NULL;
    if (!action_time_elapsed(oldest, freq)) {
        return NULL;
    }
    if (queue_cd_check(game, oldest, freq, clients) == 1)
        return NULL;
    return oldest;
}
