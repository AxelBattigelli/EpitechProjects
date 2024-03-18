/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#ifndef MY_NAVY_
    #define MY_NAVY_

    #include <sys/types.h>
    #include <sys/stat.h>
    #include <fcntl.h>
    #include <string.h>
    #include <stdio.h>
    #include <unistd.h>
    #include <signal.h>
    #include <stdlib.h>
    #include <unistd.h>

struct win_s {
    int win;
};

extern struct win_s win;
char **make_me_boats(char *str, char **tab);
char **dump_map(void);
int *dump_who(int *who);
void aff_screen(void);
void connect(int i, siginfo_t *sig, void *s);
int client_one(char **av);
int client_two(char **av);
char **ennemy_tab(char *str, char **user1);
void connect_usr1(int i, siginfo_t *sig, void *s);
char **complete_sentence(char **tab, int *arr, int pid);
char **know_case(int fd, char **tab, int pid, int *arr);
int let_play(int pid, char **av, char **tab);
char *my_intostr(int nb);
int my_putf(char *str, char *path);
void my_show_tab(char **tab);
char **so_boat(char **tab, char *str, int pos, int length);
char **linear_boat(char **tab, char *str, int pos, int length);
char **good_boat(char *str, char **tab);
char **make_me_boats(char *str, char **tab);
char **ennemy_tab(char *str, char **user1);
void hit(int i, siginfo_t *sig, void *s);
int check_coordinate(char *str, char **user1);
void show_status(char **tab, char **user1);
int navy(int ac, char **av, int i);
int win_or_not(char **tab, char **user1);
int loop_sig(char *str, int pid);
char *check_sig(void);
int play(int *who, char **tab, char **user1, struct sigaction *signal);
int check_boat(int c, char *str);
int loop_wait(int *who, char **tab, char **user1, struct sigaction *signal);
char **for_play(char *str, int pid, struct sigaction *signal, char **user1);
char **read_into_array(char *map_file);
char *read_into_str(char *map_file);

#endif /* !MY_NAVY_ */
