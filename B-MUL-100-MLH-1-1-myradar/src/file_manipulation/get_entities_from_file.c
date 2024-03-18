/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Fill the planes and towers from a script file
*/

#include <stdio.h>
#include <stdlib.h>
#include "file_manipulation.h"
#include "sim.h"
#include "plane.h"
#include "path.h"
#include "tower.h"
#include "my.h"

int get_entities_from_file(char const *file_path, sim_t *sim)
{
    char *file_buffer = get_file_buffer(file_path);
    unsigned int nb_planes = 0;
    unsigned int nb_towers = 0;
    FILE *stream = NULL;

    if (!file_buffer)
        return (1);
    nb_planes = my_count_char(file_buffer, 'A');
    nb_towers = my_count_char(file_buffer, 'T');
    sim->planes = malloc(sizeof(plane_t *) * (nb_planes + 1));
    sim->towers = malloc(sizeof(tower_t *) * (nb_towers + 1));
    sim->plane_s = nb_planes;
    sim->tower_s = nb_towers;
    stream = fopen(file_path, "r");
    if (!(sim->planes) || !(sim->towers) || !(stream))
        return (1);
    if (fill_entities_by_line(stream, sim) != 0)
        return (1);
    fclose(stream);
    return 0;
}

static int process_entity_from_line(char *current_line, sim_t *sim,
    unsigned int *plane_index, unsigned int *tower_index)
{
    if (current_line[0] == 'A') {
        sim->planes[*plane_index] = add_plane_from_line(current_line,
            sim->textures->plane, sim->window->width);
        (*plane_index)++;
    } else if (current_line[0] == 'T') {
        sim->towers[*tower_index] = add_tower_from_line(current_line,
            sim->textures->tower);
        (*tower_index)++;
    } else {
        my_puterr("Unknow char in script file\n");
        return 1;
    }
    return 0;
}

int fill_entities_by_line(FILE *stream, sim_t *sim)
{
    char *current_line = NULL;
    long unsigned int size = 50;
    unsigned int plane_index = 0;
    unsigned int tower_index = 0;

    while (getline(&current_line, &size, stream) != -1) {
        if (process_entity_from_line(current_line, sim, &plane_index,
            &tower_index) != 0) {
            free(current_line);
            return 1;
        }
    }
    free(current_line);
    sim->planes[plane_index] = NULL;
    sim->towers[tower_index] = NULL;
    return 0;
}

plane_t *add_plane_from_line(char *current_line, sfTexture *plane_texture,
    unsigned int w_width)
{
    static int index = 0;
    path_t *path = NULL;
    sfVector2f departure;
    sfVector2f arrival;
    plane_t *plane;
    unsigned int speed = 0;
    unsigned int delay = 0;

    current_line++;
    departure.x = my_strtol(current_line, &current_line);
    departure.y = my_strtol(current_line, &current_line);
    arrival.x = my_strtol(current_line, &current_line);
    arrival.y = my_strtol(current_line, &current_line);
    speed = my_strtol(current_line, &current_line);
    delay = my_strtol(current_line, &current_line);
    path = path_create(departure, arrival, speed, w_width);
    if (path == NULL)
        return (NULL);
    plane = plane_create(path, plane_texture, delay, w_width);
    plane->index = index;
    index++;
    return plane;
}

tower_t *add_tower_from_line(char *current_line, sfTexture *tower_texture)
{
    sfVector2f pos;
    unsigned int radius = 0;

    current_line++;
    pos.x = my_strtol(current_line, &current_line);
    pos.y = my_strtol(current_line, &current_line);
    radius = my_strtol(current_line, &current_line);
    return (tower_create(pos, tower_texture, radius));
}
