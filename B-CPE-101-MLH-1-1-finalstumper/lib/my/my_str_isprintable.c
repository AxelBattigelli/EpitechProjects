/*
** EPITECH PROJECT, 2023
** my_str_isprintable
** File description:
** Returns 1 if the string passed as parameter
** only contains printable characters
*/

int my_str_isprintable(char const *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if (str[i] < 32 || 126 < str[i])
            return 0;
        i++;
    }
    return 1;
}
