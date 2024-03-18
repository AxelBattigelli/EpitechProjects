#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "../include/my_sokoban.h"

Test(open_and_read_file, test_file_reading)
{
    char const *filepath = "tests/map1";
    char *result = open_and_read_file(filepath);

    cr_assert_not_null(result, "Reading file failed");
    free(result);
}

Test(open_and_read_file2, test_file_reading)
{
    char const *filepath = "tests/map_inexist";
    char *result = open_and_read_file(filepath);

    cr_assert_null(result, "Reading file failed");
    free(result);
}

Test(invalid_nb_box_empl, test_invalid_boxes_and_empl)
{
    char *test_filepath = "tests/map1";
    int result = invalid_nb_box_empl(test_filepath);

    cr_assert_eq(result, 0, "Mismatch between emplacements and boxes");
}

Test(invalid_nb_box_empl2, test_invalid_boxes_and_empl)
{
    char *test_filepath = "tests/map2";
    int result = invalid_nb_box_empl(test_filepath);

    cr_assert_eq(result, 84, "Not mismatch between emplacements and boxes");
}

Test(error_management, test_error_handling)
{
    char *av1[] = {"my_sokoban", "tests/map1"};
    char *av2[] = {"my_sokoban", "not_exist/map1"};
    char *av3[] = {"my_sokoban", "tests/map2"};
    char *av4[] = {"my_sokoban", "tests/map2"};

    cr_assert_eq(error_management(2, av1), 0, "Argument number error not handled");
    cr_assert_eq(error_management(3, av1), 84, "Argument number error not handled");
    cr_assert_eq(error_management(2, av2), 84, "Invalid file path not handled");
    cr_assert_eq(error_management(2, av3), 84, "Invalid number of boxes and emplacements error not handled");
    cr_assert_eq(error_management(2, av4), 84, "Error with content");
}

Test(get_nb_x, test_count_x_characters)
{
    char *test_map[] = {
        "########\n",
        "# X    #\n",
        "#    X #\n",
        "########\n",
        NULL
    };
    int nb_lines = 4;
    int result = get_nb_x(test_map, nb_lines);

    cr_assert_eq(result, 2, "Incorrect count of 'X' characters");
}

Test(get_nb_emplacement, test_count_o_characters)
{
    char *test_map[] = {
        "########\n",
        "# O   O#\n",
        "#    O #\n",
        "########\n",
        NULL
    };
    int nb_lines = 4;
    int result = get_nb_emplacement(test_map, nb_lines);

    cr_assert_eq(result, 3, "Incorrect count of 'X' characters");
}

// if block return 1
Test(check_stuck, test_stuck_positions)
{
    char *test_map[] = {
        "#####",
        "#X  #",
        "#  X#",
        "#   #",
        "#####",
        NULL
    };
    int result1 = check_stuck(test_map, 2, 2);
    int result2 = check_stuck(test_map, 1, 1);

    cr_assert_eq(result1, 0, "Character should be stuck at position (2, 2)");
    cr_assert_eq(result2, 1, "Character should not be stuck at position (1, 1)");
}

Test(check_end_game_defeat, test_defeat_conditions)
{
    char *test_buffer = "#####\n#   #\n#X O#\n# P #\n#####\n";
    char *test_map[] = {
        "#####",
        "#   #",
        "#X O#",
        "# P #",
        "#####",
        NULL
    };
    int result = check_end_game_defeat(test_buffer, test_map);

    cr_assert_eq(result, 0, "Game should be lost");
}

Test(check_end_game_defeat2, test_defeat_conditions)                        //  TO DO !
{
    char *test_buffer = "#####\n#   #\n#X O#\n# P #\n#####\n";
    char *test_map[] = {
        "#####",
        "#   #",
        "# PX#",
        "#   #",
        "#####",
        NULL
    };
//     int result = check_end_game_defeat(test_buffer, test_map);

//     cr_assert_eq(result, 1, "Game should be win");
}

Test(get_nb_line_and_col, get_nb_line_and_col) {
    char *test_buffer = "#####\n#   #\n#X O#\n# P #\n#####\n";
    int result = get_nb_col(test_buffer);

    cr_assert_eq(result, 5, "incorrect size col");
}

Test(get_nb_line_and_col2, get_nb_line_and_col) {
    char *test_buffer = "#####\n#   #\n#X O#\n# P #\n#####\n";
    int result = get_nb_line(test_buffer);

    cr_assert_eq(result, 5, "incorrect size col");
}

Test(get_nb_line_and_col3, get_nb_line_and_col) {
    int result = cmp_for_max_col_of_line(3, 3);
    int result2 = cmp_for_max_col_of_line(3, 1);
    int result3 = cmp_for_max_col_of_line(1, 3);

    cr_assert_eq(result, 3, "compare size col error");
    cr_assert_eq(result, 3, "compare size col error");
    cr_assert_eq(result, 3, "compare size col error");
}

Test(create_map, create_map)
{
    char *test_buffer = "#####\n";//#   #\n#X O#\n# P #\n#####\n";
    char *attept_result[] = {"#####"};//, "#   #", "#X O#", "# P #", "#####"};
    char **result = create_map(test_buffer);

    // cr_assert_eq(result, attept_result, "Invalid tab\n");
}

Test(test_move_management, test_move_management)
{
    int input_key = 65;
    char *test_map[] = {
        "#####",
        "#   #",
        "# PX#",
        "#   #",
        "#####",
        NULL
    };
    int nb_line = 5;
    char *test_save_map[] = {
        "#####",
        "#   #",
        "# XO#",
        "#P  #",
        "#####",
        NULL
    };
    char **result;
    // result = move_management(input_key, test_map, nb_line, test_save_map);

    // cr_assert_eq(result, attept_result, "Invalid\n");
}

Test(test_fill_tab, test_fill_tab)
{
    char *test_buffer = "#####\n#   #\n#X O#\n# P #\n#####\n";
    char **tab;
    int i = 0;
    char **result;
    char *attempt_result[] = {
        "#####",
        "#   #",
        "# XO#",
        "#P  #",
        "#####",
        NULL
    };

    tab = malloc(sizeof(char *) * (5 + 1));
    for (i = 0; i != 5; i++)
        tab[i] = malloc(sizeof(char) * (5 + 1));
    tab[i] = NULL;
    result = fill_tab(tab, test_buffer);

    // cr_assert_eq(result, attempt_result, "Invalid\n");
}

Test(test_malloc_2d_array, test_malloc_2d_array)
{
    char *test_buffer = "#####\n#   #\n#X O#\n# P #\n#####\n";
    char **result;
    char *attempt_result[] = {
        "#####",
        "#   #",
        "# XO#",
        "#P  #",
        "#####",
        NULL
    };

    result = malloc_2d_array(test_buffer);

    // cr_assert_eq(result, attempt_result, "Invalid\n");
}

Test(test_reset_game, test_reset_game)
{
    char *test_map[] = {
        "#####",
        "#   #",
        "#PXO#",
        "#   #",
        "#####",
        NULL
    };
    char *test_save_map[] = {
        "#####",
        "#   #",
        "# XO#",
        "#P  #",
        "#####",
        NULL
    };
    int nb_line = 5;
    
    // reset_game(test_map, test_save_map, nb_line);
}
