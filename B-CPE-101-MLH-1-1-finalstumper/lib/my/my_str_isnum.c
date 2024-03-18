/*
** EPITECH PROJECT, 2023
** my_str_isnum
** File description:
** Returns 1 if the string passed is a number
*/

int my_str_isnum(char const *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if (str[i] < '0' || '9' < str[i])
            return 0;
        i++;
    }
    return 1;
}
