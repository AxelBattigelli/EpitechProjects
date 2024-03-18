/*
** EPITECH PROJECT, 2023
** sim_fonts
** File description:
** Source file for structure sim_fonts
*/

#include <stdlib.h>
#include <SFML/Graphics.h>
#include "sim_fonts.h"

sim_fonts_t *font_create(void)
{
    sim_fonts_t *fonts = malloc(sizeof(sim_fonts_t));

    if (!fonts)
        return (NULL);
    fonts->font = sfFont_createFromFile("assets/fonts/patopian-1986.ttf");
    if (!(fonts->font))
        return (NULL);
    return fonts;
}

void font_destroy(sim_fonts_t *fonts)
{
    if (fonts->font)
        sfFont_destroy(fonts->font);
    if (fonts)
        free(fonts);
}
