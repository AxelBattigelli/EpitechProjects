/*
** EPITECH PROJECT, 2023
** my_getnbr
** File description:
** return number contains in a string
** Memo breaker : False : 0 ; True : 1
** Memo help : each '-' multiply the number by -1
**             mfn ==> multiplier_for_negative
**             brk ==> breaker
**             rst ==> result
*/

#include "my.h"

int my_getnbr(char const *str)
{
    int brk = 0;
    int mfn = 1;
    long rst = 0;
    int final_result;

    final_result = logic_my_getnbr(brk, mfn, rst, str);
    return final_result;
}

int breaker_function(int i, int brk, char const *str)
{
    if (str[i + 1] < 48 || str[i + 1] >= 58) {
        brk = 1;
    }
    return brk;
}

int logic_my_getnbr(int brk, int mfn, long rst, char const *str)
{
    int i = 0;

    while (brk != 1 && str[i] != '\0') {
        if (str[i] == 45) {
            mfn = mfn * (-1);
        }
        if (str[i] >= 48 && str[i] < 58) {
            rst = 10 * rst + (str[i] - 48);
            brk = breaker_function(i, brk, str);
        }
        i++;
        rst = mfn * rst;
        if (rst < -2147483648 || rst > 2147483647) {
            return 0;
        }
        rst = mfn * rst;
    }
    return mfn * rst;
}
