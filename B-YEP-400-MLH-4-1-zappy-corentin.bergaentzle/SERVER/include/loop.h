/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** loop
*/

#ifndef LOOP_H_
    #define LOOP_H_

    #include "game.h"
    #include "parse_args.h"
    #include "server.h"

    #include <stdlib.h>
    #include <string.h>
    #include <unistd.h>
    #include <errno.h>
    #include <netinet/in.h>
    #include <sys/socket.h>
    #include <poll.h>

/**
 * Data structure loop
 * @param address server address
 * @param fds files descriptors
 * @param buffer
 * @param server_fd fd of server
 * @param new_socket socket
 * @param nfds
 * @param addrlen
 */
typedef struct loop_s {
    struct sockaddr_in address;
    struct pollfd fds[MAX_CLIENTS + 1];
    char buffer[1024];
    int server_fd;
    int new_socket;
    int nfds;
    int addrlen;
} loop_t;

/**
 * .
 * @param . .
 */
loop_t loop_init(server_t *);

/**
 * .
 * @param . .
 */
void loop(loop_t *, game_t *, server_t *);

/**
 * .
 * @param . .
 */
int loop_quit(loop_t *);

#endif /* LOOP_H_ */
