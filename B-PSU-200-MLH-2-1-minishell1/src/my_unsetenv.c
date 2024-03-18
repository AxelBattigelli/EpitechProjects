/*
** EPITECH PROJECT, 2024
** mysh
** File description:
** main file for mysh
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdint.h>
#include <sys/wait.h>
#include <stdbool.h>
#include "../include/my.h"
#include "../include/mysh.h"

int get_line_variable(cp_env_t *cp_env, char *str)
{
    int line = -1;

    for (int i = 0; cp_env->cp_env[i] != NULL; i++) {
        if (my_strncmp(cp_env->cp_env[i], str, my_strlen(str)) == 0) {
            line = i;
            break;
        }
    }
    return (line);
}

int my_unsetenv_core(char **arg_cmd, cp_env_t *cp_env)
{
    int line = get_line_variable(cp_env, arg_cmd[1]);
    int size = 0;
    int j = 0;
    char **new_cp_env;

    if (line == -1)
        return (0);
    while (cp_env->cp_env[size])
        size++;
    new_cp_env = malloc(sizeof(char *) * (size));
    if (new_cp_env == NULL)
        return (-1);
    for (int i = 0; i < size; i++)
        if (i != line) {
            new_cp_env[j] = cp_env->cp_env[i];
            j++;
        }
    new_cp_env[j] = NULL;
    cp_env->cp_env = new_cp_env;
    return (0);
}

int my_unsetenv(char **arg_cmd, cp_env_t *cp_env)
{
    if (!arg_cmd[1]) {
        my_putstr("unsetenv: Too few arguments.\n");
        return 1;
    }
    if (arg_cmd[2])
        return 1;
    return (my_unsetenv_core(arg_cmd, cp_env));
}
