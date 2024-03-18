/*
** EPITECH PROJECT, 2023
** my_compute_square_root
** File description:
** Returns the square root of the given number
*/

int my_compute_square_root_rec(int nb, int root)
{
    if (nb / root > root)
        my_compute_square_root_rec(nb, root + 1);
    else if (root * root == nb)
        return root;
    else
        return 0;
}

int my_compute_square_root(int nb)
{
    if (nb < 0)
        return 0;
    else
        return my_compute_square_root_rec(nb, 1);
}
