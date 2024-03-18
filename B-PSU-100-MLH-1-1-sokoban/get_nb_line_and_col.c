/*
** EPITECH PROJECT, 2023
** get_nb_line_and_col
** File description:
** get
*/

int cmp_for_max_col_of_line(int line1, int line2)
{
    int res;

    if (line2 > line1)
        res = line2;
    else
        res = line1;
    return res;
}

int get_nb_col(char *buffer)
{
    int i = 0;
    int line1 = 0;
    int line2 = 0;

    for (int i = 0; buffer[i] != '\n'; i++)
        line1++;
    i++;
    while (buffer[i - 1] && buffer[i]) {
        if (buffer[i] == '\n') {
            line1 = cmp_for_max_col_of_line(line1, line2);
            line2 = 0;
            i++;
        }
        line2++;
        i++;
    }
    return line1;
}

int get_nb_line(char *buffer)
{
    int count = 0;

    for (int i = 0; buffer[i] != '\0'; i++)
        if (buffer[i] == '\n')
            count++;
    return count;
}
