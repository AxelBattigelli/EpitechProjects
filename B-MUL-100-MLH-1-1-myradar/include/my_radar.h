/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Header file for my_radar core functions
*/

#ifndef MY_RADAR_H_
    #define MY_RADAR_H_

    #include <SFML/Graphics.h>
    #include "sim.h"

int launch_simulation(window_t *window, char const *script_path);
void simulation_iteration(sim_t *sim);
void plane_loop(plane_t **plane, sim_t *sim, unsigned int c_time);
void insert_planes_in_quadtree(sim_t *sim, unsigned int c_time);
#endif
