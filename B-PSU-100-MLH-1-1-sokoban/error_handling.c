/*
** EPITECH PROJECT, 2023
** error_handling
** File description:
** if en error
*/

#include "include/my_sokoban.h"

static long len_file(char const *filepath)
{
    struct stat fstat;

    stat(filepath, &fstat);
    return (fstat.st_size);
}

char *open_and_read_file(char const *filepath)
{
    int fd = open(filepath, O_RDONLY);
    int size = len_file(filepath);
    char *buffer = malloc(sizeof(char) * (size + 1));
    int res;

    if (fd == -1) {
        write(2, "Failed to open\n", 16);
        return (NULL);
    }
    res = read(fd, buffer, size);
    if (res == -1) {
        write(2, "Cannot read the file\n", 22);
        return (NULL);
    }
    buffer[size] = '\0';
    close(fd);
    return buffer;
}

static int nb_args_err(int ac)
{
    if (ac != 2) {
        write(2, "Error : Invalid number of argument\n", 36);
        return 84;
    }
    return 0;
}

static int is_not_file(char const *filepath)
{
    if (open(filepath, O_RDONLY) == -1) {
        write(2, "No such file or directory !\n", 29);
        return 84;
    }
    return 0;
}

static int open_read_close_error(char const *filepath)
{
    if (open_and_read_file(filepath) == NULL)
        return 84;
    return 0;
}

static int invalid_map_error(char const *filepath)
{
    char *buffer;
    int i = 0;
    int nb_p = 0;

    buffer = open_and_read_file(filepath);
    for (i = 0; buffer[i] != '\0'; i++)
        if (buffer[i] != ' ' && buffer[i] != '\n' && buffer[i] != '#' &&
            buffer[i] != 'X' && buffer[i] != 'P' && buffer[i] != 'O') {
            write(2, "Invalid map\n", 13);
            return 84;
        }
    for (i = 0; buffer[i] != '\0'; i++)
        if (buffer[i] == 'P')
            nb_p++;
    if (nb_p != 1) {
        write(2, "There are any or some player\n", 30);
        return 84;
    }
    return 0;
}

int invalid_nb_box_empl(char *av)
{
    int nb_o = 0;
    int nb_x = 0;
    char *buffer = open_and_read_file(av);

    for (int i = 0; buffer[i] != '\0'; i++) {
        if (buffer[i] == 'O')
            nb_o++;
        if (buffer[i] == 'X')
            nb_x++;
    }
    if (nb_o != nb_x) {
        write(2, "Emplacements and boxes are not equal\n", 38);
        return 84;
    }
    return 0;
}

int error_management(int ac, char **av)
{
    if (nb_args_err(ac) == 84)
        return 84;
    if (is_not_file(av[1]) == 84)
        return 84;
    if (open_read_close_error(av[1]) == 84)
        return 84;
    if (invalid_map_error(av[1]) == 84)
        return 84;
    if (invalid_nb_box_empl(av[1]) == 84)
        return 84;
    return 0;
}
