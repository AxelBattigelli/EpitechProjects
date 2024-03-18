/*
** EPITECH PROJECT, 2024
** positions_checking
** File description:
** Boolean functions for positions, represented by sfVector2f
*/

#include <math.h>
#include <SFML/Graphics.h>

sfBool pos_match(sfVector2f pos_1, sfVector2f pos_2)
{
    return (fabs(pos_1.x - pos_2.x) < 20 && fabs(pos_1.y - pos_2.y) < 20);
}

sfBool pos_are_near(sfVector2f pos_1, sfVector2f pos_2, float const threshold)
{
    float x_diff = fabs(pos_2.x - pos_1.x);
    float y_diff = fabs(pos_2.y - pos_1.y);

    return (x_diff <= threshold && y_diff <= threshold);
}
