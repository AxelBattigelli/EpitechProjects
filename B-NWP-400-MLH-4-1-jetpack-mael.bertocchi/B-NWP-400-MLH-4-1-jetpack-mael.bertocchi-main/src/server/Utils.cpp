#include "server/Utils.hpp"
#include <iostream>
#include <sstream>
#include <iomanip>

std::string Utils::string_to_hex(const std::string &str)
{
    std::ostringstream oss;
    for (unsigned char c : str) {
        oss << std::setw(2) << std::setfill('0') << std::hex << (int)c;
    }
    return oss.str();
}

std::string Utils::int_to_hex(uint8_t val)
{
    std::ostringstream oss;
    oss << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(val);
    return oss.str();
}
