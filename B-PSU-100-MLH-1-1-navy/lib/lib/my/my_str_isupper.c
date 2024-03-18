/*
** EPITECH PROJECT, 2023
** my_str_isupper
** File description:
** return 1 if string contains only upper alpha, else 0
*/

int my_str_isupper(char const *str)
{
    int counter = 0;
    int i = 0;
    int len_str = 0;

    while (str[i] != '\0') {
        if (str[i] > 64 && str[i] < 91) {
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
