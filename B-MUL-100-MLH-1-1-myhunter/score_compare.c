/*
** EPITECH PROJECT, 2023
** score_compare
** File description:
** score_compare
*/

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include "include/my_hunter.h"

static int read_score(char *nomFichier)
{
    FILE *fd;
    char buffer[5];

    fd = fopen(nomFichier, "r+");
    fread(buffer, 4, 1, fd);
    fclose(fd);
    return my_getnbr(buffer);
}

static void write_score(char *nomFichier, int nb)
{
    int fd = open(nomFichier, O_WRONLY);
    char buffer[4];

    if (nb >= 1000) {
        buffer[0] = (nb / 1000 % 10) + 48;
        buffer[1] = (nb / 100 % 10) + 48;
        buffer[2] = (nb / 10 % 10) + 48;
    } else if (nb >= 100 && nb < 1000) {
        buffer[0] = '0';
        buffer[1] = (nb / 100 % 10) + 48;
        buffer[2] = (nb / 10 % 10) + 48;
    }
    if (nb >= 10 && nb < 100) {
        buffer[0] = '0';
        buffer[1] = '0';
        buffer[2] = (nb / 10 % 10) + 48;
    }
    buffer[3] = '0';
    write(fd, buffer, 4);
    close(fd);
}

void score_compare(void)
{
    if (read_score("score.txt") > read_score("hight_score.txt"))
        write_score("hight_score.txt", read_score("score.txt"));
}
