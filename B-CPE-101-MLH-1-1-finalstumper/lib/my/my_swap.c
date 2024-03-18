/*
** EPITECH PROJECT, 2023
** my_swap
** File description:
** swaps the content of two integer
*/

void my_swap(int *a, int *b)
{
    int temp = *a;

    *a = *b;
    *b = temp;
}
