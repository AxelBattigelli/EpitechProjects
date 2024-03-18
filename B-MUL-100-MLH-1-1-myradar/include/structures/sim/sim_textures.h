/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Header file for structure sim_textures
*/

#ifndef TEXTURES_H_
    #define TEXTURES_H_

    #include <SFML/Graphics.h>

typedef sfTexture texture_t;

typedef struct sim_textures {
    texture_t *sim_bg;
    texture_t *plane;
    texture_t *tower;
} sim_textures_t;
typedef sim_textures_t textures_t;

sim_textures_t *sim_textures_create(void);
void sim_textures_destroy(sim_textures_t *);
#endif
