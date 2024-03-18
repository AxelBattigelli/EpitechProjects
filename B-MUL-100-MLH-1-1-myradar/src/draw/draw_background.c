/*
** EPITECH PROJECT, 2023
** draw_background
** File description:
** Source file for drawing the background
*/

#include <SFML/Graphics.h>

void draw_background(sfRenderWindow *window, sfSprite *background)
{
    sfRenderWindow_drawSprite(window, background, NULL);
}
