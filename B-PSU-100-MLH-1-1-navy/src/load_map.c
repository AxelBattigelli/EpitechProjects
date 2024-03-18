/*
** EPITECH PROJECT, 2023
** load map
** File description:
** fonctions who load the map into array
*/

#include "../include/my.h"
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

int vertical_len(char *map_file)
{
    int i = 0;
    int vertical_len = 0;

    for (; map_file[i + vertical_len] != '\n'; vertical_len++);
    return vertical_len;
}

int horizontal_len(char *map_file)
{
    int horizontal_len = 0;

    for (int i = 0; map_file[i]; i++) {
        if (map_file[i] == '\n')
            horizontal_len++;
    }
    return horizontal_len;
}

char **map_into_array(char *map_file, int j, int i)
{
    int y = 0;
    char *str;
    int len_v = vertical_len(map_file);
    int len_h = horizontal_len(map_file);
    char **array = malloc(sizeof(char *) * (len_h + 2));

    for (; j < len_h; j++) {
        str = malloc(sizeof(char) * len_v + 2);
        for (; map_file[i] != '\n' && map_file[i] && y < len_v + 2; i++) {
            str[y] = map_file[i];
            y++;
        }
        str[y] = 0;
        array[j] = str;
        y = 0;
        if (map_file[i] != 0)
            i++;
    }
    array[j] = 0;
    return array;
}

char **read_into_array(char *map_file)
{
    char *buff = malloc(sizeof(char) * (200));
    int fd = open(map_file, O_RDONLY);
    long len = read(fd, buff, 199);
    int i = 0;

    close(fd);
    buff[len] = '\0';
    return map_into_array(buff, 0, 0);
}
