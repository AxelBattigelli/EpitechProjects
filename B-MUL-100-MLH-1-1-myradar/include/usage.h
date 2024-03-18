/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Header file for usage
*/

#ifndef USAGE_H_
    #define USAGE_H_

    #define MY_EXIT_OPTION          192

int check_args(int ac, char **av);
int check_options(char *arg);
void print_help(void);
void print_bad_ac(int ac, char **av);
#endif
