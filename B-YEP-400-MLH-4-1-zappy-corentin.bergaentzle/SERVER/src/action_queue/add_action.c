/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** add_action
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

static int fill_informations(char **command, game_t *game, int client_id,
    int *duration)
{
    char *test = strdup(*command);

    test = create_command(test);
    if (!test)
        return -3;
    *command = test;
    if (count_client_actions(game->action_queue, client_id) >= 10)
        return -1;
    *duration = get_command_duration(*command);
    if (*duration < 0)
        return -2;
    return 0;
}

static int fill_action(action_t *new_action, int client_id, char *command,
    int duration)
{
    if (!new_action)
        return -3;
    if (strcmp(command, "Incantation_wait\n") == 0)
        (new_action)->priority = true;
    else
        (new_action)->priority = false;
    (new_action)->client_fd = client_id;
    (new_action)->command = command;
    (new_action)->duration = duration;
    gettimeofday(&(new_action)->start_time, NULL);
    return 0;
}

int loop_action(game_t *game, int client_id, action_t *new_action,
    char *command_copy)
{
    char *command;
    int return_value;
    int duration;
    char *token;

    token = strtok(command_copy, "\n");
    while (token != NULL) {
        command = strdup(token);
        return_value = fill_informations(&command, game, client_id, &duration);
        if (return_value < 0)
            return return_value;
        new_action = calloc(1, sizeof(action_t));
        return_value = fill_action(new_action, client_id, command, duration);
        if (return_value < 0)
            return return_value;
        new_action->next = game->action_queue;
        game->action_queue = new_action;
        token = strtok(NULL, "\n");
    }
    return 0;
}

int count_client_actions(action_t *queue, int client_fd)
{
    int count = 0;
    action_t *current = queue;

    while (current) {
        if (current->client_fd == client_fd)
            count++;
        current = current->next;
    }
    return count;
}

int add_action_to_queue(game_t *game, int client_fd, const char *commands)
{
    char *command_copy = strdup(commands);
    int return_value;
    action_t *new_action = NULL;

    if (!command_copy)
        return -3;
    return_value = loop_action(game, client_fd, new_action, command_copy);
    if (return_value < 0) {
        free(command_copy);
        return return_value;
    }
    free(command_copy);
    return 0;
}
