/*
** EPITECH PROJECT, 2023
** my_swap
** File description:
** swap the content of two integers
*/

void my_swap(int *a, int *b)
{
    int tmp1 = *a;
    int tmp2 = *b;

    *b = tmp1;
    *a = tmp2;
}
