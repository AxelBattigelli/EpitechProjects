/*
** EPITECH PROJECT, 2023
** structure
** File description:
** contains all struct necessary for project
*/

#include <SFML/Graphics.h>
#include <SFML/Audio.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "my_hunter.h"

sprite_t *init_sprite(sfRenderWindow *window, char *file)
{
    sprite_t *ret = NULL;
    sfTexture *tex = NULL;
    sfSprite *spr = NULL;

    ret = malloc(sizeof(*ret));
    tex = sfTexture_createFromFile(file, NULL);
    spr = sfSprite_create();
    if (!tex) {
        sfRenderWindow_destroy(window);
        return NULL;
    }
    if (!spr) {
        sfTexture_destroy(tex);
        sfRenderWindow_destroy(window);
        return NULL;
    }
    ret->spr = spr;
    ret->tex = tex;
    sfSprite_setTexture(spr, tex, sfTrue);
    return ret;
}

sprite_t *background(sfRenderWindow *window, char *file)
{
    sprite_t *ret = NULL;
    sfTexture *tex = NULL;
    sfSprite *spr = NULL;

    ret = malloc(sizeof(*ret));
    tex = sfTexture_createFromFile(file, NULL);
    spr = sfSprite_create();
    if (!tex) {
        sfRenderWindow_destroy(window);
        return NULL;
    }
    if (!spr) {
        sfTexture_destroy(tex);
        sfRenderWindow_destroy(window);
        return NULL;
    }
    ret->spr = spr;
    ret->tex = tex;
    sfSprite_setTexture(spr, tex, sfTrue);
    return ret;
}

sfIntRect init_rect(void)
{
    sfIntRect rect ;

    rect.top = 0;
    rect.left = 0;
    rect.width = 110;
    rect.height = 110;
    return rect;
}

sprite_t *cursor(sfRenderWindow *window)
{
    sfRenderWindow_setMouseCursorVisible(window, sfFalse);
    return init_sprite(window, "cursor3.png");
}
