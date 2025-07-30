/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** client
*/

#ifndef CLIENT_H_
    #define CLIENT_H_

    #include "server.h"
    #include "game.h"

typedef struct loop_s loop_t;

typedef enum {
    CLIENT_WAITING,
    CLIENT_AI,
    CLIENT_GRAPHIC,
    CLIENT_UNKNOWN
} client_type_t;

typedef enum direction_e {
    DIR_NORTH = 0,
    DIR_EAST = 1,
    DIR_SOUTH = 2,
    DIR_WEST = 3
} direction_t;

/**
 * Data structure for client
 * @param next
 * @param type
 * @param team_name
 * @param fd
 * @param level
 * @param x
 * @param y
 * @param dir
 * @param inventory
 */
typedef struct client_s {
    struct client_s *next;
    client_type_t type;
    direction_t dir;
    struct timeval last_command;
    char *team_name;
    int fd;
    int player_id;
    int level;
    int x;
    int y;
    int inventory[7];
    int unique_id;
} client_t;

void handle_new_connection(loop_t *, server_t *, client_t **);
void handle_team_selection(client_t *, char *, server_t *, client_t *);

int send_to_id(client_t *, int, char *);

#endif /* CLIENT */
