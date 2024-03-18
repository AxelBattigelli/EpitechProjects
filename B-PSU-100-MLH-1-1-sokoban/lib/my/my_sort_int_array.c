/*
** EPITECH PROJECT, 2023
** my_sort_in_array
** File description:
** sort an array in ascending order
*/

void my_sort_int_array(int *array, int size)
{
    int done = 0;
    int i = 1;
    int tmp1;
    int tmp2;

    while (done < (size - 1) ) {
        if (array[done] > array[i]) {
            tmp1 = array[done];
            tmp2 = array[i];
            array[done] = tmp2;
            array[i] = tmp1;
        }
        if (i == (size - 1)) {
            done++;
            i = size - 1 - done;
        } else {
            i++;
        }
    }
}
