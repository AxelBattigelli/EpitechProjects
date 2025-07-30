/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Action queue
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

const command_entry_t command_table[] = {
    { "Forward\n", forward_function },
    { "Right\n", right_function },
    { "Left\n", left_function },
    { "Look\n", look_function },
    { "Inventory\n", inventory_function },
    { "Connect_nbr\n", connect_nbr_function },
    { "Fork\n", fork_function },
    { "Eject\n", eject_function },
    { "Incantation\n", incantation_function },
    { NULL, NULL }
};

client_t *find_client_by_id(client_t *clients, int id)
{
    client_t *current = clients;

    while (current) {
        if (current->player_id == id)
            return current;
        current = current->next;
    }
    return NULL;
}

int action_time_elapsed(action_t *action, int freq)
{
    struct timeval current_time;
    long elapsed_ms;
    long required_ms;

    gettimeofday(&current_time, NULL);
    elapsed_ms = (current_time.tv_sec - action->start_time.tv_sec) * 1000 +
        (current_time.tv_usec - action->start_time.tv_usec) / 1000;
    required_ms = (action->duration * 1000) / freq;
    return elapsed_ms >= required_ms;
}

static int execute_command(action_t *action, client_t *client,
    server_t *server, game_t *game)
{
    int res = -1;

    printf("executing command: %s", action->command);
    for (int i = 0; command_table[i].name != NULL; i++) {
        if (strcmp(action->command, command_table[i].name) == 0) {
            res = command_table[i].function(action, client, server, game);
            printf("res code = %d\n", res);
            return res;
        }
    }
    return res;
}

int queue_cd_check(game_t *, action_t *oldest, int freq,
    client_t *clients)
{
    struct timeval current_time;
    long elapsed_since_last;
    long required_time_ms;
    client_t *client = find_by_fd(clients, oldest->client_fd);

    if (!client)
        return 1;
    gettimeofday(&current_time, NULL);
    elapsed_since_last = (current_time.tv_sec - client->last_command.tv_sec)
        * 1000 + (current_time.tv_usec - client->last_command.tv_usec) / 1000;
    required_time_ms = (oldest->duration * 1000) / freq;
    if (client->last_command.tv_sec != 0 &&
        elapsed_since_last <= required_time_ms) {
        return 1;
    }
    gettimeofday(&client->last_command, NULL);
    return 0;
}

static int remove_action(action_t *current, action_t *action_to_remove,
    action_t *prev, game_t *game)
{
    if (current == action_to_remove) {
        if (prev) {
            prev->next = current->next;
        } else {
            game->action_queue = current->next;
        }
            return 1;
        }
    return 0;
}

void remove_specific_action(game_t *game, action_t *action_to_remove)
{
    action_t *current = game->action_queue;
    action_t *prev = NULL;
    int return_value = 0;

    while (current) {
        return_value = remove_action(current, action_to_remove, prev, game);
        if (return_value == 1) {
            free(current->command);
            free(current);
            return;
        }
        prev = current;
        current = current->next;
    }
}

static void execute_action(action_t *action, client_t *clients,
    server_t *server, game_t *game)
{
    if (action) {
        if (execute_command(action, clients, server, game) == -1)
            parse_command_complexe(action, clients, server, game);
        remove_specific_action(game, action);
    }
}

void process_action_queue(game_t *game, client_t *clients, server_t *server)
{
    action_t *action;
    int client_list[1024];
    int client_count = get_clients_with_actions(game->action_queue,
        client_list, 1024);

    for (int i = 0; i < client_count; i++) {
        action = find_oldest_client_action(
            clients, client_list[i], server->freq, game);
        execute_action(action, clients, server, game);
    }
}
