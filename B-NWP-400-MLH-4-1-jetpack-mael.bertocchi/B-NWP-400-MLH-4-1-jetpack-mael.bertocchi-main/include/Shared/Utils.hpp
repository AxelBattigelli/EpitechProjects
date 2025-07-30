/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Utils.hpp
*/

#pragma once

#include "Shared/Data.hpp"

#include <vector>

namespace Jetpack::Shared
{
    /**
    * @class Utils
    * @brief Utility functions
    */
    class Utils
    {
        public:
            /**
             * @brief Splits a string into a vector of substrings based on a delimiter
             *
             * @param str The input string.
             * @param delimiter The character used to split the string (default is space).
             * @return A vector containing the split substrings.
             */
            static std::vector<std::string> SplitStr(const std::string& str, char delimiter = ' ');

            /**
             * @brief Get the value of an option from a list of arguments
             *
             * @param begin The beginning of the list of arguments
             * @param end The end of the list of arguments
             * @param option The option to search for
             * @return The value of the option, or an empty string if not found
             */
            static const std::string GetTextOption(char **begin, char **end, const std::string& option);

            /**
             * @brief Check if an option exists in a list of arguments
             *
             * @param begin The beginning of the list of arguments
             * @param end The end of the list of arguments
             * @param option The option to search for
             * @return True if the option exists, false otherwise
             */
            static bool DoesOptionExist(char **begin, char **end, const std::string& option);

            /**
             * @brief Convert an event to a hexadecimal string
             *
             * @param event The event to convert
             * @return The hexadecimal string representation of the event
             */
            static const std::string SerializeEvent(const Shared::Data::Event& event);

            /**
             * @brief Convert a hexadecimal string to an event
             *
             * @param packet The hexadecimal string to convert
             * @return The event represented by the hexadecimal string
             */
            static const Shared::Data::Event DeserializeEvent(const std::string& packet);

            /**
             * @brief Convert a string to a hexadecimal string
             *
             * @param str The string to convert
             * @return The hexadecimal string representation of the input string
             */
            static const std::string StringAsHex(const std::string& str);

            /**
             * @brief Convert a character to a hexadecimal string
             *
             * @param value The character to convert
             * @return The hexadecimal string representation of the input character
             */
            static const std::string CharAsHex(const std::uint8_t value);
    };
}
