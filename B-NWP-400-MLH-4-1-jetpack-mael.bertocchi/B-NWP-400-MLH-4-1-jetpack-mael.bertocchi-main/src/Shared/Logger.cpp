/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Logger.cpp
*/

#include "Shared/Logger.hpp"

#include <iostream>
#include <ctime>

const std::string Jetpack::Shared::Logger::GetCurrentTime()
{
    std::time_t now = std::time(nullptr);
    std::tm *localTime = std::localtime(&now);
    char buffer[80] = {0};

    std::strftime(buffer, sizeof(buffer), "%Y-%m-%d %H:%M:%S", localTime);
    return std::string(buffer);
}

void Jetpack::Shared::Logger::Info(const std::string& message, bool print)
{
    if (print) {
        std::cout << "\r[" << GetCurrentTime() << "] \033[32m>\033[0m " << message << std::endl;
    }
}

void Jetpack::Shared::Logger::Error(const std::string& message, bool print)
{
    if (print) {
        std::cerr << "\r[" << GetCurrentTime() << "] \033[31m>\033[0m " << message << std::endl;
    }
}
