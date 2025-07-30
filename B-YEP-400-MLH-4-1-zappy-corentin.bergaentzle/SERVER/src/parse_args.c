/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** parse_args
*/

#include "error.h"
#include "server.h"
#include "parse_args.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <bits/getopt_core.h>

static int init_server_defaults(server_t *server)
{
    if (!server)
        return 84;
    memset(server, 0, sizeof(server_t));
    server->player_id = 1;
    server->unique_id = 0;
    server->freq = 100;
    return 0;
}

static int check_team_names(server_t *server, char **argv, int index)
{
    for (int i = 0; i < server->team_count; i++) {
        if (strcmp(server->teams[i].name, argv[index]) == 0) {
            fprintf(stderr, "Duplicate team name: %s\n", argv[index]);
            return 84;
        }
    }
    if (server->team_count >= MAX_TEAMS) {
        fprintf(stderr, "Too many teams, max %d allowed.\n", MAX_TEAMS);
        return 84;
    }
    return 0;
}

static int parse_team_names(int argc, char **argv, server_t *server)
{
    int index = optind;

    server->team_count = 0;
    server->teams = malloc(sizeof(team_t) * MAX_TEAMS);
    if (!server->teams)
        return 84;
    server->teams[server->team_count] =
        (team_t){NULL, strdup(optarg), server->clients_per_team, 0};
    server->team_count++;
    while (index < argc && argv[index][0] != '-') {
        if (check_team_names(server, argv, index) == 84)
            return 84;
        server->teams[server->team_count] =
            (team_t){NULL, strdup(argv[index]), server->clients_per_team, 0};
        server->team_count++;
        index++;
    }
    optind = index;
    return 0;
}

static int validate_server_config(server_t *server)
{
    if (!server->team_count) {
        fprintf(stderr, "Team names (-n) are mandatory.\n");
        return 84;
    }
    if (server->port <= 0 || server->width <= 0 || server->height <= 0 ||
        server->clients_per_team <= 0 || server->freq <= 0) {
        fprintf(stderr, "Invalid numeric arguments.\n");
        return 84;
    }
    return 0;
}

static int handle_map_options(int opt, server_t *server)
{
    if (opt == 'p') {
        server->port = atoi(optarg);
        return 0;
    }
    if (opt == 'x') {
        server->width = atoi(optarg);
        return 0;
    }
    if (opt == 'y') {
        server->height = atoi(optarg);
        return 0;
    }
    return -1;
}

static int handle_gameplay_options(int opt, int argc, char **argv,
    server_t *server)
{
    if (opt == 'c') {
        server->clients_per_team = atoi(optarg);
        return 0;
    }
    if (opt == 'n') {
        if (parse_team_names(argc, argv, server) != 0)
            return 84;
        return 0;
    }
    if (opt == 'f') {
        server->freq = atoi(optarg);
        return 0;
    }
    return -1;
}

static int handle_option(int opt, char **argv, int argc, server_t *server)
{
    if (handle_map_options(opt, server) == 0)
        return 0;
    if (handle_gameplay_options(opt, argc, argv, server) == 0)
        return 0;
    return 84;
}

int parse_args(int argc, char **argv, server_t *server)
{
    int opt;

    if (argc < 13 || init_server_defaults(server) != 0)
        return print_usage(argv[0]);
    opt = getopt(argc, argv, "p:x:y:n:c:f:");
    while (opt != -1) {
        if (handle_option(opt, argv, argc, server) != 0)
            return print_usage(argv[0]);
        opt = getopt(argc, argv, "p:x:y:n:c:f:");
    }
    for (int i = 0; i < server->team_count; i++)
        server->teams[i].nb_slot = server->clients_per_team;
    if (validate_server_config(server) != 0)
        return print_usage(argv[0]);
    return 0;
}
