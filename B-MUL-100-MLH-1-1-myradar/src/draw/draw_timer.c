/*
** EPITECH PROJECT, 2024
** qqc
** File description:
** Source file for drawing timer
*/

#include <stdlib.h>
#include "my.h"
#include "sim_texts.h"
#include "utils.h"

char *get_str_timer(unsigned int c_time)
{
    char *string = malloc(sizeof(char) * 6);
    unsigned int minutes = c_time / 60;
    unsigned int seconds = c_time % 60;

    string[0] = '\0';
    if (minutes < 10)
        string = my_strcat(string, "0");
    string = my_strcat(string, my_int_to_strnum(minutes));
    string = my_strcat(string, ":");
    if (seconds < 10)
        string = my_strcat(string, "0");
    string = my_strcat(string, my_int_to_strnum(seconds));
    return (string);
}

void draw_timer(sfRenderWindow *win, text_t *timer_text, unsigned int c_time)
{
    sfText_setString(timer_text, get_str_timer(c_time));
    sfRenderWindow_drawText(win, timer_text, NULL);
}
