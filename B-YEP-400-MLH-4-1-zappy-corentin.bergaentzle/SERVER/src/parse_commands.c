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

const gui_command_entry_t gui_command_table[] = {
    { "msz\n", map_size },
    { "bct\n", tile_content },
    { "mct\n", map_content },
    { "tna\n", teams_name },
    { "ppo\n", players_position },
    { "plv\n", players_level },
    { "pin\n", players_inventory },
    { "sgt\n", time_unit },
    { "sst\n", change_time_unit },
    { NULL, NULL }
};

client_t *find_by_fd(client_t *clients, int fd)
{
    for (client_t *curr = clients; curr != NULL; curr = curr->next) {
        if (curr->fd == fd)
            return curr;
    }
    return NULL;
}

void parse_command_ia(loop_t *loop, game_t *game, client_t *clients,
    server_t *)
{
    client_t *client = find_by_fd(clients, loop->new_socket);

    if (!client) {
        send(loop->new_socket, "ko\n", 4, 0);
        printf("%d %s\n", loop->new_socket, loop->buffer);
        exit(84);
        return;
    }
    if (client->type == CLIENT_GRAPHIC) {
        send(client->fd, "ko\n", 4, 0);
        return;
    }
    if (add_action_to_queue(game, client->fd, loop->buffer) < -1) {
        return;
    }
}

static char *parse_command_graph_buffer(loop_t *loop)
{
    static char command_storage[1024];
    char temp_buffer[1024];
    char *command;
    char *params;

    strncpy(temp_buffer, loop->buffer, sizeof(temp_buffer) - 1);
    command = strtok(temp_buffer, " \n");
    params = strtok(NULL, "\n");
    if (params != NULL) {
        strncpy(loop->buffer, params, sizeof(loop->buffer) - 1);
        loop->buffer[sizeof(loop->buffer) - 1] = '\0';
    } else
        loop->buffer[0] = '\0';
    if (!command) {
        send(loop->new_socket, "suc\n", 4, 0);
        return NULL;
    }
    strncpy(command_storage, command, sizeof(command_storage) - 1);
    command_storage[sizeof(command_storage) - 1] = '\0';
    return command_storage;
}

static char *handle_command_graph(char *command)
{
    size_t command_len = strlen(command);
    char *command_with_newline = malloc(sizeof(char) * command_len + 2);

    snprintf(command_with_newline,
        sizeof(command_with_newline), "%s\n", command);
    return command_with_newline;
}

void parse_command_graph(loop_t *loop, game_t *game, client_t *client,
    server_t *server)
{
    char *command = parse_command_graph_buffer(loop);
    char *command_newline = NULL;

        if (!command || command[0] == '\0') {
        send(loop->new_socket, "suc\n", 4, 0);
        return;
    }
    printf("Command: '%s'\n", command);
    printf("Params: '%s'\n", loop->buffer);
    command_newline = handle_command_graph(command);
    for (int i = 0; gui_command_table[i].name != NULL; i++)
        if (strcmp(command_newline, gui_command_table[i].name) == 0) {
            printf("Found matching command: %s with params: %s\n",
                command, loop->buffer);
            free(command_newline);
            gui_command_table[i].function(client, server, game, loop);
            return;
        }
    free(command_newline);
    send(loop->new_socket, "suc\n", 4, 0);
}
