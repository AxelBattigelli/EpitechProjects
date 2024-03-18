/*
** EPITECH PROJECT, 2023
** next_id_txt
** File description:
** next id gestion
** because coding style, any fd secure test
**    if (fd == -1) {
**        my_printf("Cann't open file");
**        return;
**    }
*/

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include "include/my.h"

void init_write_nb(char *filename)
{
    int fd = open(filename, O_WRONLY | O_TRUNC);
    char buffer[] = "0";

    write(fd, buffer, my_strlen(buffer));
    close(fd);
}

int read_nb(char *filename)
{
    int fd = open(filename, O_RDONLY);
    char buffer[10];
    int value;
    ssize_t bytesRead;

    if (fd == -1) {
        my_printf("Cann't open file");
        return -1;
    }
    bytesRead = read(fd, buffer, sizeof(buffer) - 1);
    if (bytesRead == -1) {
        my_printf("Cann't read file");
        close(fd);
        return -1;
    }
    buffer[bytesRead] = '\0';
    value = my_getnbr(buffer);
    close(fd);
    return value;
}

static void write_nb(char *filename, int nb)
{
    int fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    char buffer[20];
    int digits = 0;
    int temp = nb;
    ssize_t res;

    do {
        ++digits;
        temp /= 10;
    } while (temp != 0);
    for (int i = digits - 1; i >= 0; --i) {
        buffer[i] = '0' + (nb % 10);
        nb /= 10;
    }
    res = write(fd, buffer, my_strlen(buffer));
    if (res < 0 || res != my_strlen(buffer))
        my_printf("Cann't write in file");
    close(fd);
}

void increment_digit_nb(char *filename)
{
    int nb = read_nb(filename);

    nb++;
    write_nb(filename, nb);
}
