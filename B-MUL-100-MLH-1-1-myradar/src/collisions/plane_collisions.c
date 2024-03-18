/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Handles the collisions for my_radar
*/

#include <SFML/Graphics.h>
#include "sim.h"
#include "collisions.h"
#include "utils.h"

plane_t *get_collided_plane(plane_t **plane, sim_t *sim)
{
    if (plane_is_in_control_area((*plane)->hitbox, sim)) {
        sfRectangleShape_setOutlineColor((*plane)->outline, sfGreen);
        return (sfFalse);
    } else {
        sfRectangleShape_setOutlineColor((*plane)->outline, sfYellow);
    }
    return (plane_check_collisions(plane, sim));
}

sfBool plane_is_in_control_area(sfFloatRect hitbox, sim_t *sim)
{
    for (unsigned int i = 0; i < sim->tower_s; i++)
        if (boundary_is_in_circle(hitbox, sim->towers[i]->pos,
            sim->towers[i]->radius)) {
            return (sfTrue);
        }
    return (sfFalse);
}

plane_t *plane_check_collisions(plane_t **plane, sim_t *sim)
{
    sfIntRect area = (sfIntRect){(*plane)->path->pos.x - 30.0,
        (*plane)->path->pos.y - 30.0, 50, 50};
    plane_t **planes = sim->planes;

    for (int i = 0; i < sim->plane_s; i++)
        if (planes[i] != NULL && planes[i]->index != (*plane)->index
            && pos_match((*plane)->path->pos, planes[i]->path->pos))
            return (planes[i]);
    return (NULL);
}
