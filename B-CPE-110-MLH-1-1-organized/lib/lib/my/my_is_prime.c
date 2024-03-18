/*
** EPITECH PROJECT, 2023
** my_is_prime
** File description:
** check if number is prime
** --> division by 1 and himself only
*/

#include "my.h"

int print_is_prime_result(int count)
{
    if (count == 2) {
        return 1;
    } else {
        return 0;
    }
}

int my_is_prime(int nb)
{
    int count = 0;
    int i = 1;
    int division;

    if (nb < 0) {
        nb = nb * (-1);
    }
    while (i <= nb) {
        division = nb % i;
        if (division == 0) {
            count++;
            i++;
        } else {
            i++;
        }
    }
    print_is_prime_result(count);
}
