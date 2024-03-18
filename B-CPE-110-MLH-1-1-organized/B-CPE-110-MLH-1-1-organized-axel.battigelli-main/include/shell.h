/*
** EPITECH PROJECT, 2023
** B-CPE-110 : Setting Up Shell
** File description:
** shell.h
*/

#ifndef SHELL_H
    #define SHELL_H

typedef struct linked_list_s {
    struct linked_list_s *next;
    char *type;
    char *name;
    int id;
} linked_list_t;

struct Node {
    struct Node* next;
    char *name;
    char *type;
    int index;
};

struct data {
    struct Node *first;
    int next_index;
};

int add(void *data, char **args);
int del(void *data, char **args);
int sort(void *data, char **args);
int disp(void *data, char **args);

int workshop_shell(void *data);

void init_write_nb(char *filename);
int read_nb(char *filename);
void increment_digit_nb(char *filename);
int nb_elem_args(char **args);

#endif /* SHELL_H */
