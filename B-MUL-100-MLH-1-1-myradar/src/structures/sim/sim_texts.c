/*
** EPITECH PROJECT, 2023
** sim_texts
** File description:
** Source file for structure sim_texts
*/

#include <stdlib.h>
#include <SFML/Graphics.h>
#include "sim_texts.h"
#include "sim_fonts.h"
#include "window.h"

sim_texts_t *texts_create(sim_fonts_t *fonts, window_t *window)
{
    sim_texts_t *texts = malloc(sizeof(sim_texts_t));

    if (!texts)
        return (NULL);
    texts->timer = init_timer_text(fonts->font, window);
    if (!(texts->timer))
        return (NULL);
    return (texts);
}

void texts_destroy(sim_texts_t *texts)
{
    if (texts->timer)
        sfText_destroy(texts->timer);
    if (texts)
        free(texts);
}

text_t *init_timer_text(font_t *font, window_t *window)
{
    sfText *timer_text = sfText_create();
    sfVector2f pos = {window->width - 250, 0};

    sfText_setCharacterSize(timer_text, 80);
    sfText_setFont(timer_text, font);
    sfText_setColor(timer_text, sfBlack);
    sfText_setPosition(timer_text, pos);
    return timer_text;
}
