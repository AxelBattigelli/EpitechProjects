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

int disp_env(char **arg_cmd, cp_env_t *cp_env)
{
    int i = 0;

    if (arg_cmd[1]) {
        my_putstr("env: ‘");
        my_putstr(arg_cmd[1]);
        my_putstr("’: No such file or directory\n");
        return (84);
    }
    while (cp_env->cp_env[i] && cp_env->cp_env[i] != NULL) {
        my_printf("%s\n", cp_env->cp_env[i]);
        i++;
    }
    return (0);
}
