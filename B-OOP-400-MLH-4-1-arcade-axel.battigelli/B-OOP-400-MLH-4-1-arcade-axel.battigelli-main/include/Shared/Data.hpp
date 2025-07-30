/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Data.hpp
*/

#pragma once

#include <array>
#include <cstdint>
#include <string>
#include <variant>
#include <vector>

namespace Arcade
{
    namespace Shared
    {
        namespace Data
        {
            /**
             * @struct Vector2
             * @brief A struct representing a 2D vector
             *
             * A 2D vector has an X and a Y variable.
             */
            template<typename T> struct Vector2 {
                T x; /*!< X position */
                T y; /*!< Y position */
            };

            /**
             * @struct Color
             * @brief A struct representing a color
             *
             * A color has an R, a G and a B variable.
             */
            struct Color {
                std::uint8_t r; /*!< Red component */
                std::uint8_t g; /*!< Green component */
                std::uint8_t b; /*!< Blue component */
            };

            /**
             * @struct Tile
             * @brief A struct representing a tile, which is a part of a map
             */
            struct Tile {
                std::array<char, 2> text; /*!< The character to display */
                std::string asset;        /*!< Path to the asset */
                Color fg;                 /*!< Foreground color */
                Color bg;                 /*!< Background color */
            };

            /**
             * @enum Key
             * @brief An enum representing a key on the keyboard
             */
            enum Key {
                KB_SPACE = 0,     /*!< Space key */
                KB_ENTER = 1,     /*!< Enter key */
                KB_ESCAPE = 2,    /*!< Escape key */
                KB_BACKSPACE = 3, /*!< Backspace key */
                KB_DELETE = 4,    /*!< Delete key */
                KB_TAB = 5,       /*!< Tab key */

                KB_UP = 6,    /*!< Up key */
                KB_DOWN = 7,  /*!< Down key */
                KB_LEFT = 8,  /*!< Left key */
                KB_RIGHT = 9, /*!< Right key */

                KB_UPPER_A = 'A', /*!< A key in uppercase */
                KB_LOWER_A = 'a', /*!< A key in lowercase */

                KB_UPPER_B = 'B', /*!< B key in uppercase */
                KB_LOWER_B = 'b', /*!< B key in lowercase */

                KB_UPPER_C = 'C', /*!< C key in uppercase */
                KB_LOWER_C = 'c', /*!< C key in lowercase */

                KB_UPPER_D = 'D', /*!< D key in uppercase */
                KB_LOWER_D = 'd', /*!< D key in lowercase */

                KB_UPPER_E = 'E', /*!< E key in uppercase */
                KB_LOWER_E = 'e', /*!< E key in lowercase */

                KB_UPPER_F = 'F', /*!< F key in uppercase */
                KB_LOWER_F = 'f', /*!< F key in lowercase */

                KB_UPPER_G = 'G', /*!< G key in uppercase */
                KB_LOWER_G = 'g', /*!< G key in lowercase */

                KB_UPPER_H = 'H', /*!< H key in uppercase */
                KB_LOWER_H = 'h', /*!< H key in lowercase */

                KB_UPPER_I = 'I', /*!< I key in uppercase */
                KB_LOWER_I = 'i', /*!< I key in lowercase */

                KB_UPPER_J = 'J', /*!< J key in uppercase */
                KB_LOWER_J = 'j', /*!< J key in lowercase */

                KB_UPPER_K = 'K', /*!< K key in uppercase */
                KB_LOWER_K = 'k', /*!< K key in lowercase */

                KB_UPPER_L = 'L', /*!< L key in uppercase */
                KB_LOWER_L = 'l', /*!< L key in lowercase */

                KB_UPPER_M = 'M', /*!< M key in uppercase */
                KB_LOWER_M = 'm', /*!< M key in lowercase */

                KB_UPPER_N = 'N', /*!< N key in uppercase */
                KB_LOWER_N = 'n', /*!< N key in lowercase */

                KB_UPPER_O = 'O', /*!< O key in uppercase */
                KB_LOWER_O = 'o', /*!< O key in lowercase */

                KB_UPPER_P = 'P', /*!< P key in uppercase */
                KB_LOWER_P = 'p', /*!< P key in lowercase */

                KB_UPPER_Q = 'Q', /*!< Q key in uppercase */
                KB_LOWER_Q = 'q', /*!< Q key in lowercase */

                KB_UPPER_R = 'R', /*!< R key in uppercase */
                KB_LOWER_R = 'r', /*!< R key in lowercase */

                KB_UPPER_S = 'S', /*!< S key in uppercase */
                KB_LOWER_S = 's', /*!< S key in lowercase */

                KB_UPPER_T = 'T', /*!< T key in uppercase */
                KB_LOWER_T = 't', /*!< T key in lowercase */

                KB_UPPER_U = 'U', /*!< U key in uppercase */
                KB_LOWER_U = 'u', /*!< U key in lowercase */

                KB_UPPER_V = 'V', /*!< V key in uppercase */
                KB_LOWER_V = 'v', /*!< V key in lowercase */

                KB_UPPER_W = 'W', /*!< W key in uppercase */
                KB_LOWER_W = 'w', /*!< W key in lowercase */

                KB_UPPER_X = 'X', /*!< X key in uppercase */
                KB_LOWER_X = 'x', /*!< X key in lowercase */

                KB_UPPER_Y = 'Y', /*!< Y key in uppercase */
                KB_LOWER_Y = 'y', /*!< Y key in lowercase */

                KB_UPPER_Z = 'Z', /*!< Z key in uppercase */
                KB_LOWER_Z = 'z', /*!< Z key in lowercase */

                KB_NUM_0 = '0', /*!< 0 numpad key */
                KB_NUM_1 = '1', /*!< 1 numpad key */
                KB_NUM_2 = '2', /*!< 2 numpad key */
                KB_NUM_3 = '3', /*!< 3 numpad key */
                KB_NUM_4 = '4', /*!< 4 numpad key */
                KB_NUM_5 = '5', /*!< 5 numpad key */
                KB_NUM_6 = '6', /*!< 6 numpad key */
                KB_NUM_7 = '7', /*!< 7 numpad key */
                KB_NUM_8 = '8', /*!< 8 numpad key */
                KB_NUM_9 = '9', /*!< 9 numpad key */

                UNKNOWN_KB = -1 /*!< Unknown key */
            };

            /**
             * @enum MouseButton
             * @brief An enum representing a mouse
             */
            enum MouseButton {
                MB_LEFT,   /*!< Principal click */
                MB_RIGHT,  /*!< Secondary click */
                MB_CENTER, /*!< Middle click */
            };

            /**
             * @typedef Event
             * @brief A variant representing an event
             *
             * An event can be a position or a key code.
             */
            using Event =
                std::variant<std::monostate, std::pair<MouseButton, Vector2<std::int32_t>>, Key>;

            /**
             * @typedef Map
             * @brief A pair representing a map with its tiles
             */
            using Map = std::vector<std::vector<Tile>>;
        }; // namespace Data
    } // namespace Shared
} // namespace Arcade
