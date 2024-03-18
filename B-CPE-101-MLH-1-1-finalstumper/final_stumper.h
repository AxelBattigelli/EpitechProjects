/*
** EPITECH PROJECT, 2023
** final_stumper
** File description:
** All functions proto for final_stumper
*/

#ifndef FINAL_STUMPER
    #define FINAL_STUMPER
    #include "./include/my.h"
    #include <stdlib.h>
    #include <unistd.h>

typedef struct p_fct {
    char *(*fct)(int, int);
} p_fct;

int lenght(char *str);
int height(char *str);
char *line(int length, char beg, char mid, char end);
void rush3(char *buff);
char *rush1_1(int x, int y);
char *rush1_2(int x, int y);
char *rush1_3(int x, int y);
char *rush1_4(int x, int y);
char *rush1_5(int x, int y);

#endif
