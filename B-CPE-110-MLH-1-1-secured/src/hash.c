/*
** EPITECH PROJECT, 2024
** secured
** File description:
** hash fonction
*/

#include <stdio.h>
#include "../include/my.h"
#include <stdint.h>
#include <unistd.h>

static int error_handling(char *key, int len)
{
    if (len <= 0) {
        write(2, "invalid size\n", 13);
        return 1;
    }
    if (my_strlen(key) == 0) {
        write(2, "key empty\n", 10);
        return 1;
    }
    return 0;
}

int mix(int a, int b, int c)
{
    a -= b;
    a -= c;
    a ^= (c >> 13);
    b -= c;
    b -= a;
    b ^= (a << 8);
    c -= a;
    c -= b;
    c ^= (b >> 13);
    a -= b;
    a -= c;
    a ^= (c >> 12);
    b -= c;
    b -= a;
    b ^= (a << 16);
    c -= a;
    c -= b;
    c ^= (b >> 5);
    return c;
}

int hash(char *key, int len)
{
    long int seed_ref = 325245365401;
    int modulo_prime = 9981001;
    unsigned int sum = 0;
    int result;
    int tmp;
    int tmp2;

    if (error_handling(key, len) == 1)
        return 84;
    for (int i = 0; i < my_strlen(key); i++) {
        tmp = key[i];
        tmp2 = tmp;
        sum = mix(sum, seed_ref, tmp + 48);
    }
    result = sum % modulo_prime;
    if (result < 0)
        result *= -1;
    return result;
}
