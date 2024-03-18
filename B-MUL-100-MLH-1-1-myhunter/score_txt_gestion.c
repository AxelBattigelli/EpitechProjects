/*
** EPITECH PROJECT, 2023
** score_txt_gestion
** File description:
** score gestion
*/

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include "include/my_hunter.h"

void init_write_nb(char *nomFichier)
{
    int fd = open(nomFichier, O_WRONLY);
    char buffer[] = "0000\n";

    write(fd, buffer, 4);
    close(fd);
}

static int read_nb(char *nomFichier)
{
    FILE *fd;
    char buffer[5];

    fd = fopen(nomFichier, "r+");
    fread(buffer, 4, 1, fd);
    fclose(fd);
    return my_getnbr(buffer);
}

char *read_digit_str(char *nomFichier)
{
    FILE *fd;
    char *buffer = malloc(sizeof(char) *5);
    char *buffer2;

    fd = fopen(nomFichier, "r+");
    fread(buffer, 4, 1, fd);
    fclose(fd);
    buffer2 = my_strdup(buffer);
    return buffer2;
}

static void write_nb(char *nomFichier, int nb)
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

void increment_digit_nb(char *nomFichier)
{
    int nb = read_nb(nomFichier);

    nb += 10;
    write_nb(nomFichier, nb);
}
