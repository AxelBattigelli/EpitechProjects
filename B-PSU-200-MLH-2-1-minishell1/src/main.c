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


void display_help(void)
{
    my_putstr("-h qqc...\n");
    exit(0);
}

void distrib_actions(char **arg_cmd, cp_env_t *cp_env)
{
    if (my_strcmp(arg_cmd[0], "exit") == 0)
        my_exit(arg_cmd);
    if (my_strcmp(arg_cmd[0], "env") == 0)
        disp_env(arg_cmd, cp_env);
    if (my_strcmp(arg_cmd[0], "setenv") == 0)
        my_setenv(arg_cmd, cp_env);
    if (my_strcmp(arg_cmd[0], "unsetenv") == 0)
        my_unsetenv(arg_cmd, cp_env);
    if (my_strcmp(arg_cmd[0], "cd") == 0)
        my_cd(arg_cmd, cp_env);
    if (my_strcmp(arg_cmd[0], "exit") != 0 && my_strcmp(arg_cmd[0], "env") != 0
    && my_strcmp(arg_cmd[0], "setenv") != 0 && my_strcmp(arg_cmd[0],
    "unsetenv") != 0 && my_strcmp(arg_cmd[0], "cd") != 0)
        installed_program(arg_cmd, cp_env);
}

// after while for prompt:  write(1, "$>", 2);
int main(int ac, char **av, char **env)
{
    char *str = NULL;
    cp_env_t *cp_env = malloc(sizeof(cp_env_t));
    char **arg_cmd;
    size_t str_sz = 0;
    ssize_t gl_ret = 0;

    cp_env->cp_env = env;
    if (ac != 1)
        display_help();
    while (true) {
        gl_ret = getline(&str, &str_sz, stdin);
        if (gl_ret == -1)
            break;
        str[gl_ret - 1] = '\0';
        arg_cmd = my_str_to_word_array_custom(str);
        if (!arg_cmd[0])
            continue;
        distrib_actions(arg_cmd, cp_env);
    }
    exit(0);
}
