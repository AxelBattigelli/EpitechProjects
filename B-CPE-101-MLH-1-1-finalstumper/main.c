/*
** EPITECH PROJECT, 2023
** main
** File description:
** Contains the main function
*/

#include "final_stumper.h"

int main(void)
{
    int buff_size = 30000;
    char buff[buff_size + 1];
    int offset = 0;
    int len = read(0, buff + offset, buff_size - offset);

    for (; len > 0; len = read(0, buff + offset, buff_size - offset)) {
        offset = offset + len;
    }
    buff[offset] = '\0';
    if (len < 0)
        return (84);
    rush3(buff);
    return (0);
}
