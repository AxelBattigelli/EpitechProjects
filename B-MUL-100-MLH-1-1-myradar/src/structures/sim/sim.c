/*
** EPITECH PROJECT, 2024
** sim
** File description:
** Source file for simulation structure
*/

#include <stdlib.h>
#include <SFML/System.h>
#include "sim.h"
#include "file_manipulation.h"
#include "window.h"
#include "plane.h"
#include "tower.h"

sim_t *sim_create(window_t *win)
{
    sim_t *sim = malloc(sizeof(sim_t));

    if (!sim)
        return (NULL);
    sim->window = win;
    sim->textures = sim_textures_create();
    sim->fonts = font_create();
    sim->state = sim_states_create();
    sim->clock = sfClock_create();
    sim->texts = texts_create(sim->fonts, sim->window);
    sim->planes = NULL;
    sim->towers = NULL;
    if (!(sim->textures) || !(sim->fonts) || !(sim->state) || !(sim->texts))
        return (NULL);
    return (sim);
}

sim_t *sim_create_from_script(window_t *window, char const *script_path)
{
    sim_t *sim = sim_create(window);

    if (!sim)
        return (NULL);
    if (get_entities_from_file(script_path, sim) != 0)
        return (NULL);
    return (sim);
}

void sim_destroy(sim_t *sim)
{
    window_destroy(sim->window);
    sfClock_destroy(sim->clock);
    sim_textures_destroy(sim->textures);
    font_destroy(sim->fonts);
    texts_destroy(sim->texts);
    sim_states_destroy(sim->state);
    for (unsigned int i = 0; sim->towers[i]; i++)
        tower_destroy(sim->towers[i]);
    for (unsigned int i = 0; sim->planes[i]; i++)
        plane_destroy(sim->planes[i]);
    if (sim)
        free(sim);
}
