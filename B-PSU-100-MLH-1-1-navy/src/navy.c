/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

char **dump_map(void)
{
    return read_into_array("./log/map.txt");
}

char **good_boat(char *str, char **tab)
{
    int c = str[0] - 48;
    int j = 2;
    int i = 0;
    char *list = "ABCDEFGH";
    int pos = 2;

    if (check_boat(c, str) == -1 || my_strlen(str) != 7)
        return (NULL);
    while (str[2] != list[i]) {
        pos = pos + 2;
        i++;
    }
    if (((str[2] == str[5]) && (str[3] != str[6])) || (c == 1))
        tab = so_boat(tab, str, pos, c);
    else if ((str[2] != str[5]) && (str[3] == str[6]))
        tab = linear_boat(tab, str, pos, c);
    else
        return (NULL);
    return (tab);
}

static int return_presence(char buffer[], ssize_t bytesRead)
{
    int newline_count = 0;

    for (ssize_t i = 0; i < bytesRead; i++)
        if (buffer[i] == '\n')
            newline_count++;
    return newline_count;
}

static int check_count_line(int fd)
{
    char buffer[40];
    ssize_t bytesRead = read(fd, buffer, sizeof(buffer));
    int newline_count = 0;

    if (bytesRead > 33)
        return -1;
    while (bytesRead > 0) {
        newline_count += return_presence(buffer, bytesRead);
        bytesRead = read(fd, buffer, sizeof(buffer));
    }
    return newline_count;
}

static int conditional_boats_len_check(int res_tt[], int i)
{
    for (int j = i + 1; j < 4; j++)
        if (res_tt[i] == res_tt[j])
            return -1;
    return 0;
}

int boats_len_check(char **s)
{
    int res_tt[4];
    int res;
    int origin;
    char string_tmp[2];

    for (int i = 0; i < 4; i++) {
        res = my_compute_square_root((s[i][5] - s[i][2]) * (s[i][5] - s[i][2])
        + (s[i][6] - s[i][3]) * (s[i][6] - s[i][3])) + 1;
        string_tmp[0] = s[i][0];
        string_tmp[1] = '\0';
        origin = my_getnbr(string_tmp);
        res_tt[i] = res;
        if (res != origin)
            return (-1);
    }
    for (int i = 0; i < 4 - 1; i++)
        if (conditional_boats_len_check(res_tt, i) == -1)
            return (-1);
    return (0);
}

char **make_me_boats(char *str, char **tab)
{
    int fd = open(str, O_RDONLY);
    char **s;
    int newline_count = 0;

    if (fd == -1)
        return (NULL);
    newline_count = check_count_line(fd);
    if (newline_count != 4)
        return (NULL);
    s = read_into_array(str);
    if (boats_len_check(s) == -1)
        return (NULL);
    for (int i = 0; s[i] != 0; i++) {
        tab = good_boat(s[i], tab);
        if (tab == NULL)
            return (NULL);
    }
    return (tab);
}

int navy(int ac, char **av, int i)
{
    if (ac == 2)
        if (client_one(av) == -1) {
            my_putstr("Invalid coordinate or file\n");
            return (84);
    }
    if (ac == 3)
        if (client_two(av) == -1) {
            my_putstr("Invalid coordinate, file or PID connexion\n");
            return (84);
        }
}
