/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** command
*/

#ifndef COMMAND_H_
    #define COMMAND_H_

    #include "loop.h"

typedef int (*command_func_t)(action_t *, client_t *, server_t *, game_t *);

typedef struct command_entry_s {
    const char *name;
    command_func_t function;
} command_entry_t;

typedef int (*gui_command_func_t)(client_t *, server_t *, game_t *, loop_t *);

typedef struct gui_command_entry_s {
    const char *name;
    gui_command_func_t function;
} gui_command_entry_t;

void parse_command_complexe(action_t *, client_t *, server_t *, game_t *);
void parse_command_ia(loop_t *, game_t *, client_t *, server_t *);
void parse_command_graph(loop_t *, game_t *, client_t *, server_t *);
int get_sound_direction(client_t *, client_t *, game_t *);
int forward_function(action_t *, client_t *, server_t *, game_t *);
int right_function(action_t *, client_t *, server_t *, game_t *);
int left_function(action_t *, client_t *, server_t *, game_t *);
int look_function(action_t *, client_t *, server_t *, game_t *);
int inventory_function(action_t *, client_t *, server_t *, game_t *);
int connect_nbr_function(action_t *, client_t *, server_t *, game_t *);
int fork_function(action_t *, client_t *, server_t *, game_t *);
int eject_function(action_t *, client_t *, server_t *, game_t *);
int incantation_function(action_t *, client_t *, server_t *, game_t *);
void broadcast_function(action_t *, client_t *, server_t *, game_t *);

int map_size(client_t *, server_t *, game_t *, loop_t *);
int tile_content(client_t *, server_t *, game_t *, loop_t *);
int map_content(client_t *, server_t *, game_t *, loop_t *);
int teams_name(client_t *, server_t *, game_t *, loop_t *);
int players_position(client_t *, server_t *, game_t *, loop_t *);
int players_level(client_t *, server_t *, game_t *, loop_t *);
int players_inventory(client_t *, server_t *, game_t *, loop_t *);
int time_unit(client_t *, server_t *, game_t *, loop_t *);
int change_time_unit(client_t *, server_t *, game_t *, loop_t *);
int get_index(server_t *, client_t *);
void send_to_grahicals(client_t *, char *);
client_t *find_by_fd(client_t *, int);

#endif /* COMMAND */
