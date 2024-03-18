/*
** EPITECH PROJECT, 2023
** my_sort_int_array
** File description:
** Sorts an array
*/

#include "my.h"

void my_sort_int_array(int *array, int size)
{
    int i = 0;
    int j = 1;

    while (i < size - 1) {
        if (array[i] > array[j])
            my_swap(&array[i], &array[j]);
        j++;
        if (j == size) {
            i++;
            j = i + 1;
        }
    }
}
