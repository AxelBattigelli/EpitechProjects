/*
** EPITECH PROJECT, 2023
** my_str_to_word_array
** File description:
** Splits a string into words
*/

#include "my.h"

static int is_alphanum(char c)
{
    if ('a' <= c && c <= 'z' || '0' <= c && c <= '9' || 'A' <= c && c <= 'Z')
        return 1;
    return 0;
}

static int count_words(char const *str)
{
    int word_counter = 0;
    int i = 1;

    if (str[0] == '\0')
        return 0;
    if (is_alphanum(str[0]))
        word_counter++;
    while (str[i] != '\0') {
        if (!is_alphanum(str[i - 1]) && is_alphanum(str[i]))
            word_counter++;
        i++;
    }
    return word_counter;
}

static void split(char const *str, char **arr, int word_counter)
{
    int i = 0;
    int j = 0;
    int n = 0;

    while (n < word_counter) {
        while (!is_alphanum(str[i]))
            i++;
        j = i + 1;
        while (is_alphanum(str[j]) && str[j] != '\0')
            j++;
        arr[n] = malloc(sizeof(char) * (j - i + 1));
        my_strncpy(arr[n], &str[i], j - i);
        i = j;
        n++;
    }
}

char **my_str_to_word_array(char const *str)
{
    char **arr;
    int j = 0;
    int n = 0;
    int word_counter = 0;

    word_counter = count_words(str);
    arr = malloc(sizeof(char *) * (word_counter + 1));
    split(str, arr, word_counter);
    arr[word_counter] = 0;
    return arr;
}
