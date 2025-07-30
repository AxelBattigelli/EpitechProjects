/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Exception.cpp
*/

#include "Shared/Exception.hpp"

Jetpack::Shared::Exception::Exception(const std::string& what, const std::source_location& where) : _what(what), _where(where) {}

const char *Jetpack::Shared::Exception::what() const noexcept
{
    return _what.c_str();
}

const std::source_location& Jetpack::Shared::Exception::where() const noexcept
{
    return _where;
}
