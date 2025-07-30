/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** game.h
*/

#ifndef GAME_H_
    #define GAME_H_

    #include <sys/time.h>
    #include <stdbool.h>

    #include "server.h"
    #include "client.h"
typedef struct client_s client_t;


/**
 * Data structure for ressource_type
 * @param FOOD 0
 * @param LINEMATE 1
 * @param DERAUMERE 2
 * @param SIBUR 3
 * @param MENDIANE 4
 * @param PHIRAS 5
 * @param THYSTAME 6
 * @param RESOURCE_COUNT 7
 */
typedef enum {
    FOOD = 0,
    LINEMATE,
    DERAUMERE,
    SIBUR,
    MENDIANE,
    PHIRAS,
    THYSTAME,
    RESOURCE_COUNT
} resource_type_t;

/**
 * Data structure for elevation requirements
 * @param REQ_PLAYERS 0
 * @param REQ_LINEMATE 1
 * @param REQ_DERAUMERE 2
 * @param REQ_SIBUR 3
 * @param REQ_MENDIANE 4
 * @param REQ_PHIRAS 5
 * @param REQ_THYSTAME 6
 */
typedef enum {
    REQ_PLAYERS = 0,
    REQ_LINEMATE,
    REQ_DERAUMERE,
    REQ_SIBUR,
    REQ_MENDIANE,
    REQ_PHIRAS,
    REQ_THYSTAME
} elevation_req_t;

/**
 * double containing ressources density
 * @param FOOD 0.5
 * @param LINEMATE 0.3
 * @param DERAUMERE 0.15
 * @param SIBUR 0.1
 * @param MENDIANE 0.1
 * @param PHIRAS 0.08
 * @param THYSTAME 0.05
 */
static const double resource_density[RESOURCE_COUNT] = {
    0.5,   //FOOD
    0.3,   //LINEMATE
    0.15,  //DERAUMERE
    0.1,   //SIBUR
    0.1,   //MENDIANE
    0.08,  //PHIRAS
    0.05   //THYSTAME
};

/**
 * ressources names
 * @param food
 * @param linemate
 * @param deraumere
 * @param sibur
 * @param mendiane
 * @param phiras
 * @param thystame
 */
static const char *resource_names[RESOURCE_COUNT] = {
    "food",
    "linemate",
    "deraumere",
    "sibur",
    "mendiane",
    "phiras",
    "thystame"
};

/**
 * elevation requirements
 * 0 = nb_player, 1 = linemate, 2 = deraumere,
 * 3 = sibur, 4 = mendiane, 5 = phiras, 6 = thystane
 * @param 1->2
 * @param 2->3
 * @param 3->4
 * @param 4->5
 * @param 5->6
 * @param 6->7
 * @param 7->8
 */
static const int elevation_requirements[7][7] = {
//0 = nbPlayer, 1 = linemate, 2 = deraumere, ...
    {1, 1, 0, 0, 0, 0, 0}, //1->2
    {2, 1, 1, 1, 0, 0, 0}, //2->3
    {2, 2, 0, 1, 0, 2, 0}, //3->4
    {4, 1, 1, 2, 0, 1, 0}, //4->5
    {4, 1, 2, 1, 3, 0, 0}, //5->6
    {6, 1, 2, 3, 0, 1, 0}, //6->7
    {6, 2, 2, 2, 2, 2, 1}  //7->8
};

/**
 * struct for timing check
 * @param last_resource_spawn
 * @param last_food_update
 * @param last_game_update
 * @param time_unit_counter
 */
typedef struct timing_s {
    struct timeval last_resource_spawn;
    struct timeval last_food_update;
    struct timeval last_game_update;
    int time_unit_counter;
} timing_t;

typedef struct tile_s {
    struct client_s **players;
    int resources[RESOURCE_COUNT];
    int player_count;
    int egg_count;
} tile_t;

typedef struct world_s {
    struct timeval last_resource_spawn;
    timing_t timing;
    tile_t **tiles;
    int width;
    int height;
    int time_unit;
} world_t;

typedef struct action_s {
    struct action_s *next;
    struct timeval start_time;
    char *command;
    int client_fd;
    int duration;
    int started;
    bool priority;
} action_t;

typedef struct game_s {
    world_t *world;
    action_t *action_queue;
    char *winning_team;
    int game_ended;
} game_t;

game_t *game_init(server_t *);
void game_destroy(game_t *);

world_t *world_create(int, int);
void world_destroy(world_t *);
void world_spawn_resources(world_t *, client_t *);

#endif /* GAME_H_ */
