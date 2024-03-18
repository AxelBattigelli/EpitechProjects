/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Header file for airplane structure
*/

#ifndef AIRPLANE_H_
    #define AIRPLANE_H_

    #include <SFML/Graphics.h>
    #include "tower.h"
    #include "path.h"
    #include "utils.h"

typedef struct plane {
    path_t *path;
    unsigned int delay;
    float angle;
    int index;
    sfFloatRect hitbox;
    sfSprite *sprite;
    sfRectangleShape *outline;
} plane_t;

plane_t *plane_create(path_t *path, sfTexture *texture, uint delay,
    uint w_width);
plane_t *plane_init(plane_t *plane, path_t *path, uint delay, uint w_width);
void plane_rotate(plane_t *plane, uint w_width);
float plane_get_angle_by_path(plane_t *plane, uint w_width);
void plane_move_out_of_bounds(plane_t *plane, uint w_width);
void plane_destroy(plane_t *plane);
void plane_move(plane_t *plane, sfVector2f const offset, uint w_width);
void plane_reset_random(plane_t *plane, tower_t **towers,
    uint c_time, uint w_width);
#endif
