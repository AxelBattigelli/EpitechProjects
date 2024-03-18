/*
** EPITECH PROJECT, 2023
** window
** File description:
** window structure
*/

#include <stdlib.h>
#include <SFML/Graphics.h>
#include "window.h"

window_t *window_create(void)
{
    window_t *win = malloc(sizeof(*win));
    sfVideoMode mode = {1920, 1080, 32};

    if (!win)
        return (NULL);
    win->render = sfRenderWindow_create(mode, "my_radar", sfFullscreen, NULL);
    win->width = 1920;
    win->height = 1080;
    win->background = NULL;
    sfRenderWindow_setFramerateLimit(win->render, 60);
    return (win);
}

void window_destroy(window_t *win)
{
    sfRenderWindow_destroy(win->render);
    if (win->background)
        sfSprite_destroy(win->background);
    if (win)
        free(win);
}

void window_set_background(window_t *win, sfTexture *texture)
{
    if (!(win->background))
        win->background = sfSprite_create();
    sfSprite_setTexture(win->background, texture, sfTrue);
}
