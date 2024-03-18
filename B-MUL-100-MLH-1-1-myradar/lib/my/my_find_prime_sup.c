/*
** EPITECH PROJECT, 2023
** my_find_prime_sup
** File description:
** check if number is prime
** --> division by 1 and himself only
*/

#include "my.h"

int print_is_prime_result2(int count)
{
    if (count == 2) {
        return 1;
    } else {
        return 0;
    }
}

int my_is_prime2(int nb)
{
    int count = 0;
    int i = 1;

    if (nb < 0) {
        nb = nb * (-1);
    }
    while (i <= nb) {
        if (nb % i == 0) {
            count++;
            i++;
        } else {
            i++;
        }
    }
    return print_is_prime_result2(count);
}

int my_find_prime_sup(int nb)
{
    if (nb < 0) {
        nb = 0;
    }
    while (my_is_prime2(nb) == 0) {
        my_is_prime2(nb + 1);
        nb++;
    }
    return nb;
}
