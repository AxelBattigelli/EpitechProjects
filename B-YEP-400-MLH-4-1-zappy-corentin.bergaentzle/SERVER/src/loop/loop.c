/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** loop
*/

#include "command.h"
#include "client.h"
#include "game.h"
#include "loop.h"
#include "parse_args.h"
#include "action.h"

#include <stdio.h>

static void send_map(client_t *client, server_t *server, game_t *game,
    loop_t *loop)
{
    if (client->type != CLIENT_GRAPHIC)
        return;
    map_content(client, server, game, loop);
}

static void split_selector(loop_t *loop, client_t *clients, server_t *server,
    game_t *game)
{
    client_t *current = find_by_fd(clients, loop->new_socket);

    if (current->type == CLIENT_AI)
        parse_command_ia(loop, game, current, server);
    if (current->type == CLIENT_GRAPHIC)
        parse_command_graph(loop, game, clients, server);
    if (current->type == CLIENT_WAITING) {
        handle_team_selection(current, loop->buffer, server, clients);
        send_map(current, server, game, loop);
    }
}

static void loop_select_client(loop_t *loop, client_t *clients,
    server_t *server, game_t *game)
{
    client_t *current = clients;
    int fd = loop->new_socket;

    if (fd == -1)
        return;
    while (current != NULL) {
        if (current->fd == fd) {
            split_selector(loop, clients, server, game);
            break;
        }
        current = current->next;
    }
}

void remove_free_client(client_t **clients, client_t *prev, client_t *current)
{
    if (prev == NULL && current->next == NULL) {
        free(current);
        *clients = NULL;
        return;
    } else if (prev == NULL)
        *clients = current->next;
    else
        prev->next = current->next;
    free(current);
    return;
}

void remove_client(client_t **clients, int fd)
{
    client_t *current = *clients;
    client_t *prev = NULL;

    while (current != NULL) {
        if (current->fd == fd) {
            remove_free_client(clients, prev, current);
            return;
        }
        prev = current;
        current = current->next;
    }
}

static int read_client_data(int fd, char *buffer)
{
    return read(fd, buffer, 1024);
}

static void disconnect_client(loop_t *loop, client_t **clients, int i)
{
    printf("Client disconnected (fd=%d)\n", loop->fds[i].fd);
    remove_client(clients, loop->fds[i].fd);
    close(loop->fds[i].fd);
    loop->fds[i] = loop->fds[loop->nfds - 1];
    loop->nfds--;
}

static void loop_read(loop_t *loop, client_t **clients,
    server_t *server, game_t *game)
{
    int valread;

    for (int i = 1; i < loop->nfds; i++) {
        if (!(loop->fds[i].revents & POLLIN))
            continue;
        loop->new_socket = loop->fds[i].fd;
        valread = read_client_data(loop->fds[i].fd, loop->buffer);
        if (valread <= 0) {
            disconnect_client(loop, clients, i);
            i--;
            continue;
        }
        loop->buffer[valread] = '\0';
        printf("Message recv (fd=%d) : %s", loop->new_socket, loop->buffer);
        loop_select_client(loop, *clients, server, game);
    }
}

static int my_accept(loop_t *loop, server_t *server, client_t **clients)
{
    if (loop->fds[0].revents & POLLIN) {
        loop->new_socket =
            accept(loop->server_fd, (struct sockaddr *)&loop->address,
            (socklen_t *)&loop->addrlen);
        if (loop->new_socket < 0) {
            perror("accept");
            return 2;
        }
        if (loop->nfds <= MAX_CLIENTS) {
            loop->fds[loop->nfds].fd = loop->new_socket;
            loop->fds[loop->nfds].events = POLLIN;
            loop->fds[loop->nfds].revents = 0;
            loop->nfds++;
            handle_new_connection(loop, server, clients);
        } else {
            send(loop->new_socket, "ko\n", 3, 0);
            close(loop->new_socket);
        }
    }
    return 0;
}

void loop(loop_t *loop, game_t *game, server_t *server)
{
    int activity;
    client_t *clients = NULL;

    while (1) {
        activity = poll(loop->fds, loop->nfds, 100);
        if (activity < 0) {
            perror("poll error");
            printf("errno: %d (%s)\n", errno, strerror(errno));
            break;
        }
        my_accept(loop, server, &clients);
        loop_read(loop, &clients, server, game);
        process_action_queue(game, clients, server);
        handle_food_consumption(clients, server, game);
        handle_resource_spawning(game, server, clients);
    }
}
