/*
** EPITECH PROJECT, 2025
** tests
** File description:
** unit_test
*/

#include <criterion/criterion.h>
#include <criterion/internal/assert.h>
#include <criterion/redirect.h>

#include "parse_args.h"

#include <bits/getopt_core.h>

// Test(init_server_defaults, valid_server)
// {
//     server_t server;
//     int res = init_server_defaults(&server);

//     cr_assert_eq(res, 0);
//     cr_assert_eq(server.port, 0);
//     cr_assert_eq(server.team_count, 0);
//     cr_assert_eq(server.width, 0);
//     cr_assert_eq(server.height, 0);
// }

// Test(init_server_defaults, null_pointer)
// {
//     int res = init_server_defaults(NULL);
//     cr_assert_eq(res, 84);
// }

// Test(parse_team_names, valid_teams)
// {
//     server_t server = {0};
//     char *argv[] = {"./zappy", "-n", "Red", "Blue", "Green", "-c", "2"};
//     int argc = sizeof(argv) / sizeof(argv[0]);

//     optind = 2;
//     optarg = argv[2];

//     int res = parse_team_names(argc, argv, &server);

//     cr_assert_eq(res, 0);
//     cr_assert_eq(server.team_count, 3);
//     cr_assert_str_eq(server.teams[0], "Red");
//     cr_assert_str_eq(server.teams[1], "Blue");
//     cr_assert_str_eq(server.teams[2], "Green");
// }

// Test(parse_team_names, too_many_teams)
// {
//     server_t server = {0};
//     char *argv[] = {"./zappy", "-n",
//         "A","B","C","D","E","F","G","H","I","J","K"};
//     int argc = sizeof(argv) / sizeof(argv[0]);

//     optind = 2;
//     optarg = argv[2];

//     int res = parse_team_names(argc, argv, &server);

//     cr_assert_eq(res, 84);
// }

// Test(validate_server_config, valid_config)
// {
//     server_t server = {
//         .team_count = 2,
//         .port = 4242,
//         .width = 10,
//         .height = 10,
//         .clients_per_team = 5,
//         .freq = 100
//     };

//     cr_assert_eq(validate_server_config(&server), 0);
// }

// Test(validate_server_config, missing_teams)
// {
//     server_t server = {
//         .team_count = 0,
//         .port = 4242,
//         .width = 10,
//         .height = 10,
//         .clients_per_team = 5,
//         .freq = 100
//     };

//     cr_assert_eq(validate_server_config(&server), 84);
// }

// Test(handle_map_options, set_port)
// {
//     server_t server = {0};
//     optarg = "1234";

//     int res = handle_map_options('p', &server);
//     cr_assert_eq(res, 0);
//     cr_assert_eq(server.port, 1234);
// }

// Test(handle_map_options, unknown_opt)
// {
//     server_t server = {0};
//     int res = handle_map_options('z', &server);
//     cr_assert_eq(res, -1);
// }

// Test(handle_option, valid_option)
// {
//     server_t server = {0};
//     optarg = "9999";

//     int res = handle_option('p', NULL, 0, &server);
//     cr_assert_eq(res, 0);
//     cr_assert_eq(server.port, 9999);
// }

// Test(handle_option, invalid_option)
// {
//     server_t server = {0};
//     int res = handle_option('z', NULL, 0, &server);
//     cr_assert_eq(res, 84);
// }

