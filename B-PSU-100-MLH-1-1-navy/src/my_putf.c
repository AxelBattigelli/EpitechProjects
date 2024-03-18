/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

int my_putf(char *str, char *path)
{
    int fd = open(path, O_WRONLY);

    write(fd, str, my_strlen(str));
}
