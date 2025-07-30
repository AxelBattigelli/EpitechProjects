/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** loop_init
*/

#include "command.h"
#include "client.h"
#include "game.h"
#include "loop.h"
#include "parse_args.h"
#include "action.h"

#include <stdio.h>

static int create_and_configure_socket(void)
{
    int server_fd;
    int opt = 1;

    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd == 0) {
        perror("socket failed");
        exit(84);
    }
    if (setsockopt(server_fd, SOL_SOCKET,
        SO_REUSEADDR, &opt, sizeof(opt)) < 0) {
        perror("setsockopt");
        close(server_fd);
        exit(84);
    }
    return server_fd;
}

static void setup_server_address(struct sockaddr_in *address, int port)
{
    address->sin_family = AF_INET;
    address->sin_addr.s_addr = INADDR_ANY;
    address->sin_port = htons(port);
}

static void bind_and_listen_socket(int server_fd, struct sockaddr_in *address)
{
    if (bind(server_fd, (struct sockaddr *)address, sizeof(*address)) < 0) {
        perror("bind failed");
        close(server_fd);
        exit(84);
    }
    if (listen(server_fd, MAX_CLIENTS) < 0) {
        perror("listen");
        close(server_fd);
        exit(84);
    }
}

static void initialize_poll_fds(struct pollfd *fds, int server_fd)
{
    fds[0].fd = server_fd;
    fds[0].events = POLLIN;
    fds[0].revents = 0;
    for (int i = 1; i < MAX_CLIENTS + 1; i++) {
        fds[i].fd = -1;
        fds[i].events = 0;
        fds[i].revents = 0;
    }
}

loop_t loop_init(server_t *server)
{
    loop_t loop;

    loop.server_fd = create_and_configure_socket();
    setup_server_address(&loop.address, server->port);
    bind_and_listen_socket(loop.server_fd, &loop.address);
    loop.addrlen = sizeof(loop.address);
    loop.nfds = 1;
    initialize_poll_fds(loop.fds, loop.server_fd);
    printf("Server listen on port %d\n", server->port);
    return loop;
}

int loop_quit(loop_t *loop)
{
    for (int i = 0; i < loop->nfds; i++) {
        close(loop->fds[i].fd);
    }
    return 0;
}
