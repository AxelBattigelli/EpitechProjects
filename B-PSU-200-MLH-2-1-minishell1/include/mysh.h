/*
** EPITECH PROJECT, 2024
** mysh
** File description:
** contains all the prototypes of mysh
*/

#ifndef MY_SH_
    #define MY_SH_

typedef struct cp_env_s {
    char **cp_env;
} cp_env_t;

char **my_str_to_word_array_custom(char const *str);
void my_exit(char **arg_cmd);
int my_setenv(char **arg_cmd, cp_env_t *cp_env);
int my_unsetenv(char **arg_cmd, cp_env_t *cp_env);
int disp_env(char **arg_cmd, cp_env_t *cp_env);
int my_cd(char **arg_cmd, cp_env_t *cp_env);
int installed_program(char **arg_cmd, cp_env_t *cp_env);

#endif /* MY_SH_ */
