/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Header file for events
*/

#ifndef EVENTS_H_
    #define EVENTS_H_

    #include <SFML/Graphics.h>
    #include "sim.h"

void sim_poll_events(sfRenderWindow *render, states_t *state);
void switch_pressed_key(sfEvent *event, states_t *states);
#endif
