/*
** EPITECH PROJECT, 2023
** bootstrap
** File description:
** header file for my_hunter
*/

#ifndef MY_HUNTER_H_
    #define MY_HUNTER_H_

    #include <stdint.h>
    #include <SFML/Graphics.h>
    #include <SFML/Audio.h>

typedef struct Sprite {
    sfSprite *spr;
    sfTexture *tex;
} sprite_t;

typedef struct variables {
    float speed;
    sfRenderWindow *win;
    sfEvent event;
    sprite_t *spr;
    sprite_t *cursed;
    sprite_t *bg;
    sfIntRect rect;
    sfClock *clock;
    sfClock *clock2;
    sfTime time;
    sfVector2f pos;
    sfText *text;
} variables_t;

int my_strcmp(char const *s1, char const *s2);
int my_putstr(char const *str);
int my_putchar(char c);
sfIntRect init_rect(void);
sprite_t *init_sprite(sfRenderWindow *window, char *file);
sprite_t *background(sfRenderWindow *window, char *file);
sprite_t *cursor(sfRenderWindow *window);
int close_window(sfRenderWindow *window);
float manage_mouse_click(sfMouseButtonEvent event, sprite_t *spr, float speed);
float analyse_events(sfEvent event, sfRenderWindow *window, sprite_t *spr,
    float speed);
void music(void);
void effect(char *path_sound);
int my_rand(void);
void init_write_digit(char *nomFichier);
int read_digit(char *nomFichier);
void increment_digit(char *nomFichier);
void init_write_nb(char *nomFichier);
void increment_digit_nb(char *nomFichier);
int my_getnbr(char const *str);
void score_compare(void);
char *my_strdup(char const *src);

#endif /* MY_HUNTER_H_ */
