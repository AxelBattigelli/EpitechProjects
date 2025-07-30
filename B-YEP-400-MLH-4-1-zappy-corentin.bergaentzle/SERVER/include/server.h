/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** server
*/

#ifndef SERVER_H_
    #define SERVER_H_

    #define MAX_CLIENTS 1024
    #define MAX_TEAMS 32

typedef struct egg_s {
    struct egg_s *next;
    int id;
    int x;
    int y;
} egg_t;

typedef struct team_s {
    egg_t *eggs;
    char *name;
    int nb_slot;
    int nb_eggs;
} team_t;

/**
 * Data structure for parameters
 * @param teams store the teams
 * @param port server port
 * @param width size of the world
 * @param height size of the world
 * @param clients_per_team nb of client per team
 * @param freq time unit for execution of actions
 * @param team_count number of teams
 * @param server_fd
 */
typedef struct server_s {
    struct team_s *teams;
    int port;
    int width;
    int height;
    int clients_per_team;
    int freq;
    int team_count;
    int player_id;
    int unique_id;
} server_t;

#endif /* SERVER_H_ */
