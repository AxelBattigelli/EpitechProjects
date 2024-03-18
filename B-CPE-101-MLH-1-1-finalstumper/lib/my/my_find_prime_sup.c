/*
** EPITECH PROJECT, 2023
** my_find_prime_sup
** File description:
** Returns the smallest prime number bigger
** or equal to a given number
*/

#include "my.h"

int my_find_prime_sup(int nb)
{
    if (my_is_prime(nb))
        return nb;
    else
        my_find_prime_sup(nb + 1);
}
