/*
** EPITECH PROJECT, 2023
** my_put_unsigned_nbr
** File description:
** display the number from 0 to 4mld~
*/

#include <unistd.h>
#include "include/my.h"

int my_putchar_r(char c)
{
    write(1, &c, 1);
    return 1;
}

int my_put_unsigned_nbr(unsigned int nb)
{
    int len = 0;
    int rst = 0;

    if (nb >= 0 && nb <= 9){
        my_putchar(nb + 48);
        len++;
    } else if (nb > 9) {
        len += my_put_unsigned_nbr(nb / 10);
        len += my_putchar_r(nb % 10 + 48);
    }
    return len;
}
