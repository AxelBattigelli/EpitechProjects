/*
** EPITECH PROJECT, 2023
** my_str_isnum
** File description:
** return 1 if string contains only digits characters, else 0
*/

int my_str_isnum(char const *str)
{
    int counter = 0;
    int i = 0;
    int len_str = 0;

    while (str[i] != '\0') {
        if (str[i] > 47 && str[i] < 58) {
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
