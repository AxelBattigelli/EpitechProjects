/*
** EPITECH PROJECT, 2023
** draw_plane
** File description:
** draw plane
*/

#include <SFML/Graphics.h>
#include "plane.h"
#include "sim_states.h"

void draw_plane(sfRenderWindow *render, plane_t *plane, states_t *states)
{
    if (states->show_sprites)
        sfRenderWindow_drawSprite(render, plane->sprite, NULL);
    if (states->show_hitbox)
        sfRenderWindow_drawRectangleShape(render, plane->outline, NULL);
}
