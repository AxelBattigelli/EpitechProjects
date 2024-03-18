/*
** EPITECH PROJECT, 2023
** my_getnbr
** File description:
** Converts a string to an int
*/

static int is_digit(char c)
{
    if ('0' <= c && c <= '9')
        return 1;
    return 0;
}

static int long_to_int(long li)
{
    if (-2147483648 <= li && li < 2147483648)
        return li;
    return 0;
}

static long extract_long(char *str)
{
    int i = 0;
    long nbr = 0;

    while (!is_digit(str[i]) && str[i] != '\0')
        i++;
    while (is_digit(str[i]) && nbr <= 2147483648 && str[i] != '\0') {
        nbr = 10 * nbr + (str[i] - '0');
        i++;
    }
    return nbr;
}

int my_getnbr(char const *str)
{
    int i = 0;
    long nbr = 0;
    int minus_counter = 0;

    while (!is_digit(str[i]) && str[i] != '\0') {
        if (str[i] == '-')
            minus_counter++;
        if (str[i] != '-' && str[i] != '+')
            return 0;
        i++;
    }
    nbr = extract_long(str);
    if (minus_counter % 2 == 1)
            nbr *= -1;
    return long_to_int(nbr);
}
