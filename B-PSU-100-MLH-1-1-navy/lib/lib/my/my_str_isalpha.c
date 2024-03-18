/*
** EPITECH PROJECT, 2023
** my_str_isalpha
** File description:
** return 1 if string contains only alpha characters, else 0
*/

int my_str_isalpha(char const *str)
{
    int counter = 0;
    int i = 0;
    int len_str = 0;

    while (str[i] != '\0') {
        if (str[i] > 64 && str[i] < 91 || str[i] > 96 && str[i] < 123) {
            counter++;
        }
        i++;
        len_str++;
    }
    if (counter == len_str) {
        return 1;
    } else {
        return 0;
    }
}
