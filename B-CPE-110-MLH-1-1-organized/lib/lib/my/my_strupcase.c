/*
** EPITECH PROJECT, 2023
** my_strupcase
** File description:
** put all letters in upper
*/

char *my_strupcase(char *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if (str[i] > 96 && str[i] < 124) {
            str[i] = str[i] - 32;
        }
        i++;
    }
    return str;
}
