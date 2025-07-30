/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Logger.hpp
*/

#pragma once

#include <string>

namespace Jetpack::Shared
{
    class Logger
    {
        public:
            /**
             * @brief Logs an error message with the current time
             *
             * @param str The message to log
             * @param print If true, the message will be printed
             */
            static void Error(const std::string& str, bool print);

            /**
             * @brief Logs a warning message with the current time
             *
             * @param str The message to log
             * @param print If true, the message will be printed
             */
            static void Info(const std::string& str, bool print);

        private:
            /**
             * @brief Retrieves the current time and formats it as a string
             */
            static const std::string GetCurrentTime();
    };
}
