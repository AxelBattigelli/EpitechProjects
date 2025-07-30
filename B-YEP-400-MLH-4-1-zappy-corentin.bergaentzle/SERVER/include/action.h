/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** action
*/

#ifndef ACTION_H_
    #define ACTION_H_

    #include "game.h"

typedef int (*gui_command_func_t)(client_t *, server_t *, game_t *, loop_t *);

typedef struct time_command_s {
    const char *name;
    gui_command_func_t function;
} time_command_t;

int add_action_to_queue(game_t *, int, const char *);
int get_command_duration(const char *);
client_t *find_client_by_unique_id(client_t *, int);
void process_action_queue(game_t *, client_t *, server_t *);
void handle_food_consumption(client_t *, server_t *, game_t *);
void handle_resource_spawning(game_t *, server_t *, client_t *);
client_t *find_client_by_id(client_t *, int);
char *create_command(char *);
int count_client_actions(action_t *, int);
int check_incantation(client_t *, action_t *, game_t *);
void remove_specific_action(game_t *, action_t *);
int queue_cd_check(game_t *, action_t *, int, client_t *);
int action_time_elapsed(action_t *, int);
action_t *find_oldest_client_action(client_t *, int, int, game_t *);
int get_clients_with_actions(action_t *, int *, int);

#endif /* ACTION_H_ */
