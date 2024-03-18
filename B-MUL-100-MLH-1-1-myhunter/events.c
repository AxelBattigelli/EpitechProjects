/*
** EPITECH PROJECT, 2023
** events
** File description:
** events controls
*/

#include <SFML/Graphics.h>
#include <SFML/Audio.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "my_hunter.h"

int close_window(sfRenderWindow *window)
{
    sfRenderWindow_close(window);
}

float manage_mouse_click(sfMouseButtonEvent event, sprite_t *spr, float speed)
{
    effect("effect.wav");
    if (event.x >= sfSprite_getPosition(spr->spr).x &&
        event.x < sfSprite_getPosition(spr->spr).x +
        sfSprite_getTextureRect(spr->spr).width &&
        event.y >= sfSprite_getPosition(spr->spr).y &&
        event.y < sfSprite_getPosition(spr->spr).y +
        sfSprite_getTextureRect(spr->spr).height) {
        sfSprite_setPosition(spr->spr, (sfVector2f){-180, my_rand()});
        increment_digit_nb("score.txt");
        if (speed - 200 >= 400)
            speed = speed - 200;
    }
    return speed;
}

float analyse_events(sfEvent event, sfRenderWindow *window, sprite_t *spr,
    float speed)
{
    if (event.type == sfEvtClosed)
        close_window(window);
    if (event.type == sfEvtMouseButtonPressed)
        speed = manage_mouse_click(event.mouseButton, spr, speed);
    return speed;
}
