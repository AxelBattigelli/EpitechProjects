/*
** EPITECH PROJECT, 2023
** my_strlowcase
** File description:
** put all letters in lowercase
*/

char *my_strlowcase(char *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if (str[i] > 64 && str[i] < 91) {
            str[i] = str[i] + 32;
        }
        i++;
    }
    return str;
}
