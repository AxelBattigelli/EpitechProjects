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

static int my_char_isalpha(char const str)
{
    if (str > 64 && str < 91 || str > 96 && str < 123)
        return (0);
    return (1);
}

static int my_char_isalpha_num(char const str)
{
    if (str > 64 && str < 91 || str > 96 && str < 123
    || str > 47 && str < 58 || str == 95)
        return (0);
    return (1);
}

static int check_alpha_num_underscore_in_str(char *str)
{
    int i = 0;

    while (str[i]) {
        if (my_char_isalpha_num(str[i]) == 1)
            return 1;
        i++;
    }
    return 0;
}

char **create_tab_for_ustenv(char **arg_cmd)
{
    char **tab = malloc(sizeof(char *) * 3);

    tab[0] = "unsetenv";
    tab[1] = my_strdup(arg_cmd[1]);
    tab[2] = NULL;
    return (tab);
}

void dump_into_struct(char **arg_cmd, cp_env_t *cp_env, char **tmp)
{
    int i = 0;
    int size = 0;
    char *tmp2 = malloc(sizeof(char *) * 1024);
    char **tmp3 = create_tab_for_ustenv(arg_cmd);
    int j = my_unsetenv(tmp3, cp_env);

    while (tmp[size])
        size++;
    cp_env->cp_env = malloc(sizeof(char **) * (size + 2));
    for (i = 0; tmp[i]; i++)
        cp_env->cp_env[i] = my_strdup(tmp[i]);
    if (!arg_cmd[2])
        arg_cmd[2] = "";
    tmp2[0] = '\0';
    tmp2 = my_strcat(tmp2, arg_cmd[1]);
    tmp2 = my_strcat(tmp2, "=\0");
    tmp2 = my_strcat(tmp2, arg_cmd[2]);
    cp_env->cp_env[i] = my_strdup(tmp2);
    cp_env->cp_env[i + 1] = NULL;
}

int my_setenv2(char **arg_cmd, cp_env_t *cp_env, char **tmp)
{
    if (my_char_isalpha(arg_cmd[1][0]) != 0) {
        my_putstr("setenv: Variable name must begin with a letter.\n");
        return (1);
    }
    if (check_alpha_num_underscore_in_str(arg_cmd[1]) != 0) {
        my_putstr("setenv: Variable name must contain alphanumeric"
        "characters.\n");
        return (1);
    }
    dump_into_struct(arg_cmd, cp_env, tmp);
    return (0);
}

int my_setenv(char **arg_cmd, cp_env_t *cp_env)
{
    char **tmp = cp_env->cp_env;
    int nb_arg = 0;

    while (arg_cmd[nb_arg])
        nb_arg++;
    if (nb_arg > 3) {
        my_putstr("setenv: Too many arguments.\n");
        return (1);
    }
    if (nb_arg == 1) {
        disp_env(&arg_cmd[0], cp_env);
        return (0);
    }
    return (my_setenv2(arg_cmd, cp_env, tmp));
}
