/*
** EPITECH PROJECT, 2023
** my_is_prime
** File description:
** Returns 1 if the given number is a prime, 0 otherwise
*/

int my_is_prime(int nb)
{
    int div = 3;

    if (nb < 2)
        return 0;
    if (nb == 2 || nb == 3 || nb == 5)
        return 1;
    if (nb % 2 == 0)
        return 0;
    while (div <= nb / 2) {
        if (nb % div == 0)
            return 0;
        div += 2;
    }
    return 1;
}
