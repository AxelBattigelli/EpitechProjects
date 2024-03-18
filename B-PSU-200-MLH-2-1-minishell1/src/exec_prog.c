/*
** EPITECH PROJECT, 2024
** mysh
** File description:
** main file for mysh
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdint.h>
#include <sys/wait.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "../include/my.h"
#include "../include/mysh.h"

static void error_getion(int status)
{
    if (WIFEXITED(status))
        if (WEXITSTATUS(status) != 0)
            return (1);
    if (WIFSIGNALED(status))
        my_printf("%s%s\n", strsignal(WTERMSIG(status)),
        WCOREDUMP(status) ? " (core dumped)" : "");
}

static int res_status(pid_t pid, int status)
{
    pid_t dead;

    do {
        dead = waitpid(pid, &status, 0);
    } while (dead == 0);
    if (dead == -1) {
        perror("waitpid");
        exit(EXIT_FAILURE);
    }
    error_getion(status);
    if (status != 0)
        return 1;
    return 0;
}

// enum ENOEXEC ==> errno
int run_installed_program(char *progname, char **arg_cmd, cp_env_t *cp_env)
{
    int status;
    pid_t pid = fork();

    if (pid == -1) {
        perror("fork");
        exit(EXIT_FAILURE);
    }
    if (pid == 0) {
        execve(progname, arg_cmd, cp_env->cp_env);
        _exit(EXIT_FAILURE);
    }
    return res_status(pid, status);
}

static int find_path_index(cp_env_t *cp_env)
{
    for (int i = 0; cp_env->cp_env[i] != NULL; i++)
        if (my_strncmp(cp_env->cp_env[i], "PATH=", 5) == 0)
            return i;
    return (-1);
}

static int count_paths(const char *line)
{
    int path_count = 1;

    for (int j = 5; line[j]; j++)
        if (line[j] == ':')
            path_count++;
    return (path_count);
}

char *conditionnal_parse_path_env(char *line)
{
    int path_count = count_paths(line);
    char **path_list = malloc(sizeof(char *) * (path_count + 1));
    int k = 0;
    int l = 0;

    path_list[k] = malloc(sizeof(char) * 100);
    for (int j = 5; line[j]; j++) {
        if (line[j] == ':') {
            path_list[k][l] = '\0';
            k++;
            l = 0;
            path_list[k] = malloc(sizeof(char) * 100);
        } else {
            path_list[k][l + 1] = '\0';
            path_list[k][l] = line[j];
            l++;
        }
    }
    path_list[k + 1] = NULL;
    return path_list;
}

char **parse_path_env(cp_env_t *cp_env)
{
    char *line = NULL;
    int path_index = find_path_index(cp_env);

    if (path_index != -1) {
        line = cp_env->cp_env[path_index];
        return (conditionnal_parse_path_env(line));
    }
    return NULL;
}

static char *my_strchr(const char *str, int cara)
{
    int i = 0;

    while (str[i] && str[i] != '\0') {
        if (str[i] == cara) {
            return (char *)str;
        }
        i++;
    }
    return NULL;
}

void second_part_installed_program(char **arg_cmd, cp_env_t *cp_env)
{
    char **sarah = parse_path_env(cp_env);
    int size = 0;
    int check = 0;
    char *tmp;

    while (sarah[size])
        size++;
    for (int i = 0; i < size; i++)
        if (sarah[i] != NULL) {
            tmp = malloc(sizeof(*tmp) * (my_strlen(sarah[i]) + 1 +
            my_strlen(arg_cmd[0]) + 1));
            my_strcpy(tmp, sarah[i]);
            my_strcat(tmp, "/");
            my_strcat(tmp, arg_cmd[0]);
            check += run_installed_program(my_strdup(tmp), arg_cmd, cp_env);
            free(tmp);
        }
    if (check >= size)
        my_printf("%s: Command not found.\n", arg_cmd[0]);
}

// check if file exist
int installed_program(char **arg_cmd, cp_env_t *cp_env)
{
    char *position_slash = my_strchr(arg_cmd[0], '/');
    int block_status = 1;

    if (position_slash != NULL) {
        block_status = run_installed_program(arg_cmd[0], arg_cmd, cp_env);
        if (block_status == 1)
            my_printf("%s: Permission denied.\n", arg_cmd[0]);
    } else
        second_part_installed_program(arg_cmd, cp_env);
    return (0);
}
