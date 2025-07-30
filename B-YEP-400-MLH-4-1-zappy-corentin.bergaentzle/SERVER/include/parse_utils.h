/*
** EPITECH PROJECT, 2025
** zappy
** File description:
** parse_utils
*/

#ifndef PARSE_UTILS_H_
    #define PARSE_UTILS_H_

/**
 * Parse an integer from a string
 * @param str the string to parse
 * @param result pointer to store the parsed integer
 */
int parse_int(const char *str, int *result);

/**
 * Parse a player ID from a string
 * @param params the string containing the player ID
 * @param player_id pointer to store the parsed player ID
 */
int parse_player_id(const char *params, int *player_id);

/**
 * Parse two integers from a string
 * @param params the string containing the two integers
 * @param x pointer to store the first integer
 * @param y pointer to store the second integer
 */
int parse_two_ints(const char *params, int *x, int *y);

#endif /* !PARSE_UTILS_H_ */
