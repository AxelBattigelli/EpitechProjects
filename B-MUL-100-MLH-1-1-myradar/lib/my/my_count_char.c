/*
** EPITECH PROJECT, 2023
** my_count_char
** File description:
** nb char appear in a string
*/

int my_count_char(char const *str, char const to_count)
{
    unsigned int count = 0;

    for (unsigned int i = 0; str[i]; i++)
        if (str[i] == to_count)
            count++;
    return (count);
}
