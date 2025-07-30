/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** main
*/

#include "game.h"
#include "loop.h"
#include "parse_args.h"
#include "server.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char **argv)
{
    server_t server;
    loop_t loop_val;
    game_t *game;

    if (parse_args(argc, argv, &server) != 0) {
        fprintf(stderr, "Failed to parse arguments.\n");
        return 84;
    }
    printf("Map Size: %dx%d\n", server.width, server.height);
    printf("Clients per Team: %d\n", server.clients_per_team);
    printf("Frequency: %d\n", server.freq);
    printf("Team Count: %d\n", server.team_count);
    printf("Teams:\n");
    for (int i = 0; i < server.team_count; i++)
        printf("  - %s\n", server.teams[i].name);
    srand(time(NULL));
    game = game_init(&server);
    loop_val = loop_init(&server);
    loop(&loop_val, game, &server);
    return 0;
}
