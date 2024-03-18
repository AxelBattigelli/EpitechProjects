/*
** EPITECH PROJECT, 2023
** main
** File description:
** win opening and display
*/

#include <SFML/Graphics.h>
#include <SFML/Audio.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "my_hunter.h"

void move_rect(sfIntRect *rect, int offset, int max_value)
{
    rect->left += offset;
    if (rect->left >= max_value)
        rect->left = 0;
}

static void duck_not_catch(sprite_t *spr)
{
    if (sfSprite_getPosition(spr->spr).x > 1500) {
        sfSprite_setPosition(spr->spr, (sfVector2f){-180, my_rand()});
        increment_digit("fail.txt");
    }
}

static void init_text(sfText *text)
{
    sfFont *font = sfFont_createFromFile("PlayfairDisplay-Regular.otf");

    sfText_setFont(text, font);
    sfText_setCharacterSize(text, 35);
    sfText_setFillColor(text, sfWhite);
    sfText_setPosition(text, (sfVector2f){1400, 10});
}

static void init_all(sfText *text)
{
    music();
    init_write_digit("fail.txt");
    init_write_nb("score.txt");
    init_text(text);
    return text;
}

static void sprites_destroy(sprite_t *bg, sprite_t *spr, sprite_t *cursed)
{
    sfSprite_destroy(bg->spr);
    sfSprite_destroy(spr->spr);
    sfSprite_destroy(cursed->spr);
}

static float events_loop(sfRenderWindow *win, sfEvent event, sprite_t *spr,
    float speed)
{
    while (sfRenderWindow_pollEvent(win, &event))
        speed = analyse_events(event, win, spr, speed);
    return speed;
}

void all_init_variables(variables_t *var)
{
    var->speed = 6000.0;
    var->win = NULL;
    var->event;
    var->spr = NULL;
    var->cursed = NULL;
    var->bg = NULL;
    var->rect = init_rect();
    var->clock = sfClock_create();
    var->clock2 = sfClock_create();
    var->time;
    var->pos;
    var->text = sfText_create();
}

int game_loop(void)
{
    sfVideoMode mode = {1500, 900, 32};
    variables_t *var = malloc(sizeof(variables_t));

    all_init_variables(var);
    var->win = sfRenderWindow_create(mode, "My_hunter", sfClose, NULL);
    sfRenderWindow_setFramerateLimit(var->win, 60);
    var->spr = init_sprite(var->win, "sprites2.png");
    var->bg = init_sprite(var->win, "bg.jpeg");
    var->cursed = cursor(var->win);
    init_all(var->text);
    while (sfRenderWindow_isOpen(var->win)) {
        display_elem(var);
        if (read_digit("fail.txt") >= 3)
            sfRenderWindow_close(var->win);
    }
    sprites_destroy(var->bg, var->spr, var->cursed);
    sfRenderWindow_destroy(var->win);
    return 0;
}

void display_elem(variables_t *var)
{
    var->time = sfClock_getElapsedTime(var->clock);
    if (var->time.microseconds > 350000) {
        sfClock_restart(var->clock);
        move_rect(&var->rect, 110, 330);
    }
    var->speed = events_loop(var->win, var->event, var->spr, var->speed);
    duck_not_catch(var->spr);
    sfText_setString(var->text, read_digit_str("score.txt"));
    sfSprite_setTextureRect(var->spr->spr, var->rect);
    var->pos = sfSprite_getPosition(var->spr->spr);
    var->pos.x += sfClock_restart(var->clock2).microseconds / var->speed;
    sfSprite_setPosition(var->spr->spr, var->pos);
    sfSprite_setPosition(var->cursed->spr, (sfVector2f){
        sfMouse_getPositionRenderWindow(var->win).x - 30,
        sfMouse_getPositionRenderWindow(var->win).y - 40});
    sfRenderWindow_drawSprite(var->win, var->bg->spr, NULL);
    sfRenderWindow_drawSprite(var->win, var->spr->spr, NULL);
    sfRenderWindow_drawSprite(var->win, var->cursed->spr, NULL);
    sfRenderWindow_drawSprite(var->win, var->text, NULL);
    sfRenderWindow_display(var->win);
}

int main(int ac, char **av)
{
    if (ac >= 2)
        if (my_strcmp(av[1], "-h") == 0) {
            my_putstr("my_hunter - play the most funny game ever\n\nSYNOPSIS\n"
            "\tmy_hunter [-h]\n\nDESCRIPTION\n\tA duck hunt game. Kill the "
                "most fish you can\n\t3 fails and you lost\n\nCONTROLS\n\t"
                "Left clic : all clics\n");
            return 0;
        }
    game_loop();
    score_compare();
    my_putstr("It's the end !\n\n  Your score is : ");
    my_putstr(read_digit_str("score.txt"));
    my_putstr("\n\n  The best score is : ");
    my_putstr(read_digit_str("hight_score.txt"));
    my_putstr("\n");
    return 0;
}
