/*
** EPITECH PROJECT, 2023
** qqc
** File description:
** Source file for structure sim_states
*/

#include <stdlib.h>
#include <SFML/Graphics.h>
#include "sim_states.h"

sim_states_t *sim_states_create(void)
{
    sim_states_t *states = malloc(sizeof(sim_states_t));

    if (!states)
        return (NULL);
    states->show_hitbox = sfTrue;
    states->show_sprites = sfTrue;
    return (states);
}

void sim_states_destroy(sim_states_t *states)
{
    if (states)
        free(states);
}
