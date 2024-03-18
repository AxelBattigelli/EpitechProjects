/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Source file for structure sim_textures
*/

#include <stdlib.h>
#include <SFML/Graphics.h>
#include "sim_textures.h"

sim_textures_t *sim_textures_create(void)
{
    sim_textures_t *tex = malloc(sizeof(sim_textures_t));

    if (!tex)
        return (NULL);
    tex->plane = sfTexture_createFromFile("assets/textures/plane.png",
        NULL);
    tex->tower = sfTexture_createFromFile("assets/textures/tower.png",
        NULL);
    tex->sim_bg = sfTexture_createFromFile("assets/backgrounds/world_map.jpg",
        NULL);
    if (!(tex->sim_bg) || !(tex->plane) || !(tex->tower))
        return (NULL);
    return (tex);
}

void sim_textures_destroy(sim_textures_t *tex)
{
    if (tex->plane)
        sfTexture_destroy(tex->plane);
    if (tex->tower)
        sfTexture_destroy(tex->tower);
    if (tex->sim_bg)
        sfTexture_destroy(tex->sim_bg);
    if (tex)
        free(tex);
}
