/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Header file for collisions
*/

#ifndef COLLISIONS_H_
    #define COLLISIONS_H_

    #include <SFML/Graphics.h>
    #include "sim.h"

plane_t *get_collided_plane(plane_t **plane, sim_t *sim);
sfBool plane_is_in_control_area(sfFloatRect hitbox, sim_t *sim);
plane_t *plane_check_collisions(plane_t **plane, sim_t *sim);
#endif
