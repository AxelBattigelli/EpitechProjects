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

char *parse_user_cd(cp_env_t *cp_env)
{
    char *line = NULL;
    char *user = NULL;

    for (int i = 0; cp_env->cp_env[i] != NULL; i++) {
        if (my_strncmp(cp_env->cp_env[i], "HOME=", 5) == 0) {
            line = cp_env->cp_env[i];
            break;
        }
    }
    if (line != NULL)
        user = my_strdup(line + 5);
    else
        user = NULL;
    return user;
}

char *parse_oldpwd_cd(cp_env_t *cp_env)
{
    char *line = NULL;
    char *path = NULL;

    for (int i = 0; cp_env->cp_env[i] != NULL; i++) {
        if (my_strncmp(cp_env->cp_env[i], "OLDPWD=", 7) == 0) {
            line = cp_env->cp_env[i];
            break;
        }
    }
    if (line != NULL)
        path = my_strdup(line + 7);
    else
        path = NULL;
    return path;
}

void part1(char **arg_cmd, cp_env_t *cp_env, char *buffer)
{
    char *path = parse_oldpwd_cd(cp_env);

    getcwd(buffer, 200);
    if (my_strcmp(arg_cmd[1], "-\0") == 0) {
        if (path) {
            chdir(path);
        } else
            my_putstr(": No such file or directory.\n");
    } else if (chdir(arg_cmd[1]) == -1) {
        my_putstr(arg_cmd[1]);
        my_putstr(": Not a directory.\n");
    }
}

void part2(cp_env_t *cp_env, char *buffer)
{
    char **tab1 = malloc(sizeof(char *) * 2);
    char **tab2 = malloc(sizeof(char *) * 4);

    tab1[0] = my_strdup("unsetenv");
    tab1[1] = my_strdup("OLDPWD");
    my_unsetenv(tab1, cp_env);
    tab2[0] = my_strdup("setenv");
    tab2[1] = my_strdup("OLDPWD");
    tab2[2] = my_strdup(buffer);
    tab2[3] = NULL;
    my_setenv(tab2, cp_env);
}

int my_cd(char **arg_cmd, cp_env_t *cp_env)
{
    char buffer[200];

    if (arg_cmd[1] && arg_cmd[2])
        return (1);
    if (!arg_cmd[1])
        chdir(parse_user_cd(cp_env));
    if (arg_cmd[1]) {
        part1(arg_cmd, cp_env, buffer);
    }
    if (arg_cmd[1] && my_strcmp(arg_cmd[1], ".\0") == 1) {
        part2(cp_env, buffer);
    }
    return (0);
}
