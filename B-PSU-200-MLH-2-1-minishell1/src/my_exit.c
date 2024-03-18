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

void my_exit(char **arg_cmd)
{
    int nb = 0;

    if (arg_cmd[1]) {
        nb = my_getnbr(arg_cmd[1]);
        exit(nb);
    } else
        exit(nb);
}
