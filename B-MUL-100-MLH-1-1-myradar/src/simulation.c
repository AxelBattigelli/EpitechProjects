/*
** EPITECH PROJECT, 2024
** simulation
** File description:
** simulation
*/

#include <stdlib.h>
#include <SFML/Graphics.h>
#include "usage.h"
#include "my_radar.h"
#include "sim.h"
#include "draw.h"
#include "events.h"
#include "utils.h"
#include "collisions.h"
#include "my.h"

int launch_simulation(window_t *window, char const *script_path)
{
    sim_t *sim = NULL;

    sim = sim_create_from_script(window, script_path);
    if (sim == NULL) {
        my_puterr("Simulation couldn't be created.\n");
        return 84;
    }
    window_set_background(sim->window, sim->textures->sim_bg);
    sfClock_restart(sim->clock);
    while (sfRenderWindow_isOpen(sim->window->render)) {
        sim_poll_events(sim->window->render, sim->state);
        sfRenderWindow_clear(sim->window->render, sfWhite);
        simulation_iteration(sim);
        sfRenderWindow_display(sim->window->render);
    }
    sim_destroy(sim);
    return 0;
}

void simulation_iteration(sim_t *sim)
{
    uint c_time = sfTime_asSeconds(sfClock_getElapsedTime(sim->clock));

    draw_background(sim->window->render, sim->window->background);
    draw_towers(sim->window->render, sim->towers, sim->state);
    for (unsigned int i = 0; i < sim->plane_s; i++) {
        if (sim->planes[i] == NULL)
            continue;
        plane_loop(&sim->planes[i], sim, c_time);
    }
    draw_timer(sim->window->render, sim->texts->timer, c_time);
}

void plane_loop(plane_t **plane, sim_t *sim, uint c_time)
{
    plane_t *coll_plane = NULL;

    if ((*plane)->delay > c_time)
        return;
    coll_plane = get_collided_plane(plane, sim);
    if (coll_plane) {
        sim->planes[(*plane)->index] = NULL;
        free(*plane);
        sim->planes[coll_plane->index] = NULL;
        free(coll_plane);
        return;
    }
    draw_plane(sim->window->render, *plane, sim->state);
    plane_move(*plane, (*plane)->path->step, sim->window->width);
    if (pos_are_near((*plane)->path->pos, (*plane)->path->end, 10.0)) {
        sim->planes[(*plane)->index] = NULL;
        free(*plane);
    }
}
