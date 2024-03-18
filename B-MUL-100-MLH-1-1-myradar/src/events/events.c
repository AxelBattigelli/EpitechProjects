/*
** EPITECH PROJECT, 2023
** events
** File description:
** events for my_radar
*/

#include <SFML/Graphics.h>
#include "events.h"
#include "draw.h"
#include "sim.h"

void sim_poll_events(sfRenderWindow *render, states_t *state)
{
    sfEvent event;

    while (sfRenderWindow_pollEvent(render, &event)) {
        if (event.type == sfEvtClosed ||
            (event.type == sfEvtKeyPressed &&
            (event.key.code == sfKeyEscape || event.key.code == sfKeyQ))) {
            sfRenderWindow_close(render);
        }
        if (event.type == sfEvtKeyReleased) {
            switch_pressed_key(&event, state);
        }
    }
}

void switch_pressed_key(sfEvent *event, states_t *states)
{
    if (event->key.code == sfKeyL)
        states->show_hitbox = (states->show_hitbox) ? sfFalse : sfTrue;
    else if (event->key.code == sfKeyS)
        states->show_sprites = (states->show_sprites) ? sfFalse : sfTrue;
}
