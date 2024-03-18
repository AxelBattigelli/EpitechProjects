/*
** EPITECH PROJECT, 2023
** my_intlen
** File description:
** len of nb
*/

int my_intlen(int nb)
{
    int len = 0;

    if (nb == 0)
        return (1);
    while (nb != 0) {
        len++;
        nb /= 10;
    }
    return len;
}
