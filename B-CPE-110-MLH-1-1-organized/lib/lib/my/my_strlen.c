/*
** EPITECH PROJECT, 2023
** my_strlen
** File description:
** return the len of a string
*/

int my_strlen(char const *str)
{
    int count = 0;
    int i = 0;

    while (str[i] != '\0') {
        count++;
        i++;
    }
    return count;
}
