/*
** EPITECH PROJECT, 2023
** my
** File description:
** contains all the prototypes of libmy
*/

#ifndef MY_H_
    #define MY_H_

int my_putchar(char c);
int my_isneg(int nb);
int my_put_nbr(int nb);
int my_swap(int *a, int *b);
int my_putstr(char const *str);
__attribute__((const))
int my_strlen(char const *str);
int my_getnbr(char const *str);
int my_sort_int_array(int *tab, int size);
int my_compute_power_rec(int nb, int power);
int my_compute_square_root(int nb);
int my_is_prime(int nb);
int my_find_prime_sup(int nb);
char *my_strcpy(char *dest, char const *src);
char *my_strncpy(char *dest, char const *src, int n);
char *my_revstr(char *str);
char *my_strstr(char *str, char const *to_find);
int my_strcmp(char const *s1, char const *s2);
int my_strncmp(char const *s1, char const *s2, int n);
char *my_strupcase(char *str);
char *my_strlowcase(char *str);
char *my_strcapitalize(char *str);
int my_str_isalpha(char const *str);
int my_str_isnum(char const *str);
int my_str_islower(char const *str);
int my_str_isupper(char const *str);
int my_isprintable(char const *str);
int my_showstr(char const *str);
int my_showmem(char const *str, int size);
char *my_strcat(char *dest, char const *src);
char *my_strncat(char *dest, char const *src, int nb);
int my_show_word_array(char *const *tab);
char **my_str_to_word_array(char const *str);
char *my_strdup(char const *src);

int calc(int nb, int i);
int my_is_prime2(int nb);
int print_is_prime_result2(int count);
int breaker_function(int i, int brk, char const *str);
int logic_my_getnbr(int brk, int mfn, long rst, char const *str);
int print_is_prime_result(int count);
int negative_to_positive(int nb);
int print_extrem_negative(int);
int print_positive_uni(int nb);
char my_strcapitalize_second(char *str);
int my_strlen_strcat(char const *str);
int my_str_isprintable(char const *str);
int my_strlen_strncat(char const *str);

int my_printf(const char *format, ...);
int decimal_into_binary(int nb);
int decimal_into_hexadecimal_min(long divide);
int decimal_into_hexadecimal_up(int nb);
int decimal_into_octal(int nb);
int my_len_nbr(int nb);
int my_put_unsigned_nbr(unsigned int nb);
int my_putstr_len(char *str);
int hashtag(const char *format);
int my_putchar_percent(void);
int my_char_isnum(char c);
int my_putchar_r(char c);
char **my_str_to_array_printf(char const *str);
int my_put_float(double nb);
int pointer_into_hexadecimal(long *nb);
int exponent_max(double nb);
int exponent_min(double nb);
int my_put_float_prec(double nb, int prec);
int my_printf(const char *format, ...);

#endif /* MY_H_ */
