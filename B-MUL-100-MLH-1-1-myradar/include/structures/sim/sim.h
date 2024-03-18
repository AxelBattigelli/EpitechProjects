/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Header file for simulation structure
*/

#ifndef SIM_H_
    #define SIM_H_

    #include <SFML/Graphics.h>
    #include "window.h"
    #include "plane.h"
    #include "tower.h"
    #include "sim_textures.h"
    #include "sim_fonts.h"
    #include "sim_texts.h"
    #include "sim_states.h"


typedef struct simulation {
    window_t *window;
    plane_t **planes;
    int plane_s;
    tower_t **towers;
    int tower_s;
    textures_t *textures;
    fonts_t *fonts;
    texts_t *texts;
    states_t *state;
    sfClock *clock;
} sim_t;

sim_t *sim_create(window_t *window);
sim_t *sim_create_from_script(window_t *window, char const *filepath);
void sim_destroy(sim_t *sim);
#endif
