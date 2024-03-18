/*
** EPITECH PROJECT, 2023
** my_str_to_word_array_custom
** File description:
** return array where each cell point to a word
*/

#include <stdlib.h>
#include <stddef.h>
#include "../include/my.h"

// test if chara is alpha or num return 0 else 1
int my_chara_isalphanum(char c)
{
    if ((c > 64 && c < 91) ||
        (c > 96 && c < 123) ||
        (c > 47 && c < 58) ||
        (c == 47 || c == 92 || c == 61 || c == 95 || c == 34 || c == 64) ||
        (c > 41 && c < 47))
        return 0;
    return 1;
}

// return the len of a word by his position
int len_of_word(char const *str, int position)
{
    int len_this_word = 0;

    while (my_chara_isalphanum(str[position]) == 0) {
        len_this_word++;
        position++;
    }
    return len_this_word;
}

// return the number of word to create the tab
int number_of_words(char const *str)
{
    int compt = 0;
    int i = 0;

    while (str[i] != '\0') {
        if ((my_chara_isalphanum(str[i]) == 1) &&
            (my_chara_isalphanum(str[i - 1]) == 0))
            compt++;
        i++;
    }
    compt++;
    return compt;
}

// strcpy plus simple
char *my_s_t_w_a_loop(char const *str, int line, char **string, char *intern)
{
    int i = 0;
    int j = 0;

    while (line < number_of_words(str) && str[i] != '\0') {
        intern = malloc(sizeof(char) * (len_of_word(str, i) + 1));
        while (my_chara_isalphanum(str[i]) == 0) {
            intern[j] = str[i];
            j++;
            i++;
        }
        if (j > 0) {
            intern[j] = '\0';
            string[line] = intern;
            line++;
        }
        j = 0;
        i++;
    }
    string[line] = NULL;
    return string;
}

char **my_str_to_word_array_custom(char const *str)
{
    int line = 0;
    char **string;
    char *intern;

    if (number_of_words(str) == 0) {
        my_put_nbr(9);
        return str;
    }
    string = malloc(sizeof(char *) * (number_of_words(str) + 1));
    my_s_t_w_a_loop(str, line, string, intern);
}
