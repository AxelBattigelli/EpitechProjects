/*
** EPITECH PROJECT, 2023
** countisland
** File description:
** count the number of islands
*/

#include <stddef.h>

int width_of_world(char **world)
{
    int width = 0;

    while (world[0][width] != '\0')
        width++;
    return width;
}

int height_of_world(char **world)
{
    int height = 0;

    while (world[height] != NULL)
        height++;
    return height;
}

int recursive(int x, int y, int count, char **world)
{
    if (world[x][y] != 'X')
        return 0;
    if (world[x][y] == 'X') {
        world[x][y] = count + 48;
        if ((y + 1) < width_of_world(world))
            recursive(x, y + 1, count, world);
        if ((y - 1) > 0)
            recursive(x, y - 1, count, world);
        if ((x - 1) > 0)
            recursive(x - 1, y, count, world);
        if ((x + 1) < height_of_world(world))
            recursive(x + 1, y, count, world);
        return 1;
    }
}

int count_island(char **world)
{
    int x = 0;
    int y = 0;
    int count = 0;

    while (x < height_of_world(world)) {
        if (y < width_of_world(world)) {
            count += recursive(x, y, count, world);
            y++;
        } else {
            x++;
            y = 0;
        }
    }
    return count;
}
