/*
** EPITECH PROJECT, 2023
** my_strcapitalize
** File description:
** put a upper case to the first char of each word
*/

char *my_strcapitalize_second(char *str)
{
    int i = 1;

    while (str[i] != '\0') {
        if (str[i - 1] == ' ' && str[i] > 96 && str[i] < 123) {
            str[i] = str[i] - 32;
        }
        if (str[i - 1] == 43 && str[i] > 96 && str[i] < 123
            || str[i - 1] == 45 && str[i] > 96 && str[i] < 123) {
            str[i] = str[i] - 32;
        }
        i++;
    }
    return str;
}

char *my_strcapitalize(char *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if (str[i] > 64 && str[i] < 91) {
            str[i] = str[i] + 32;
        }
        i++;
    }
    if (str[0] > 96 && str[0] < 123) {
        str[0] = str[0] - 32;
    }
    my_strcapitalize_second(str);
}
