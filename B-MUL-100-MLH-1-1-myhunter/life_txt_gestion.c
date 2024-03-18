/*
** EPITECH PROJECT, 2023
** life_txt_gestion
** File description:
** fail gestion
*/

#include <unistd.h>
#include <fcntl.h>
#include "include/my_hunter.h"

void init_write_digit(char *nomFichier)
{
    int fd = open(nomFichier, O_WRONLY);
    char buffer[2];

    buffer[0] = '0';
    write(fd, buffer, 1);
    close(fd);
}

int read_digit(char *nomFichier)
{
    int fd = open(nomFichier, O_RDONLY);
    char buffer[2];

    read(fd, buffer, 2);
    close(fd);
    return buffer[0] - '0';
}

static void write_digit(char *nomFichier, int chiffre)
{
    int fd = open(nomFichier, O_WRONLY);
    char buffer[2];

    buffer[0] = chiffre + '0';
    write(fd, buffer, 1);
    close(fd);
}

void increment_digit(char *nomFichier)
{
    int chiffre = read_digit(nomFichier);

    chiffre = (chiffre + 1) % 10;
    write_digit(nomFichier, chiffre);
}
