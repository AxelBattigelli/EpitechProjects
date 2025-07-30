/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** clients
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

int search_client(int count, int *client_list, action_t *current, int found)
{
    for (int i = 0; i < count; i++) {
        if (client_list[i] == current->client_fd) {
            found = 1;
            break;
        }
    }
    return found;
}

int get_clients_with_actions(action_t *queue, int *client_list, int max_client)
{
    action_t *current = queue;
    int count = 0;
    int found = 0;

    while (current && count < max_client) {
        found = 0;
        found = search_client(count, client_list, current, found);
        if (!found) {
            client_list[count] = current->client_fd;
            count++;
        }
        current = current->next;
    }
    return count;
}
