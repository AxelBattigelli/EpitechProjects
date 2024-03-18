/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Header file for tower struct
*/

#ifndef TOWER_H_
    #define TOWER_H_

    #include <SFML/Graphics.h>

typedef struct tower {
    sfVector2f pos;
    unsigned int radius;
    sfSprite *sprite;
    sfCircleShape *control_area;
} tower_t;

tower_t *tower_create(sfVector2f pos, sfTexture *texture,
    unsigned int radius);
void tower_destroy(tower_t *tower);
sfVector2f get_random_tower_pos(tower_t **towers);
sfBool vector2f_match(sfVector2f pos_1, sfVector2f pos2);
#endif
