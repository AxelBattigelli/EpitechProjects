/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Header file for structure sim_fonts
*/

#ifndef FONTS_H_
    #define FONTS_H_

    #include <SFML/Graphics.h>

typedef sfFont font_t;

typedef struct sim_fonts {
    font_t *font;
} sim_fonts_t;
typedef sim_fonts_t fonts_t;

sim_fonts_t *font_create(void);
void font_destroy(sim_fonts_t *);
#endif
