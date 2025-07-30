/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** command_management
*/

#include "command.h"
#include "client.h"
#include "game.h"
#include "loop.h"
#include "action.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

static int get_next_command_duration(const char *command)
{
    if (strncmp(command, "Take", 4) == 0)
        return 7;
    if (strncmp(command, "Set", 3) == 0)
        return 7;
    if (strncmp(command, "Incantation", 11) == 0)
        return 300;
    if (strncmp(command, "Incantation_wait", 16) == 0)
        return 300;
    return -1;
}

int get_command_duration(const char *command)
{
    if (strncmp(command, "Forward", 7) == 0)
        return 7;
    if (strncmp(command, "Right", 5) == 0)
        return 7;
    if (strncmp(command, "Left", 4) == 0)
        return 7;
    if (strncmp(command, "Look", 4) == 0)
        return 7;
    if (strncmp(command, "Inventory", 9) == 0)
        return 1;
    if (strncmp(command, "Broadcast", 9) == 0)
        return 7;
    if (strncmp(command, "Connect_nbr", 11) == 0)
        return 0;
    if (strncmp(command, "Fork", 4) == 0)
        return 42;
    if (strncmp(command, "Eject", 5) == 0)
        return 7;
    return get_next_command_duration(command);
}

char *create_command(char *command)
{
    char *command_with_newline;
    int len = strlen(command);

    if (command[len - 1] != '\n') {
        command_with_newline = malloc(len + 2);
        if (!command_with_newline)
            return NULL;
        strcpy(command_with_newline, command);
        strcat(command_with_newline, "\n");
        command = command_with_newline;
    }
    return command;
}
