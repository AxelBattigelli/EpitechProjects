/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** parse_args
*/

#ifndef PARSE_ARGS_H_
    #define PARSE_ARGS_H_

    #include "server.h"

/**
 * Parse arguments from CLI
 * @param argc nb args
 * @param argv tab of args
 * @param server pointer to server_t struct
 * @return 0 return code
 */
int parse_args(int, char **, server_t *);

#endif /* PARSE_ARGS_H_ */
