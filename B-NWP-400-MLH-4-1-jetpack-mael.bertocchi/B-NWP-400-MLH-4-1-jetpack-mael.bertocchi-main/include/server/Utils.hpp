#pragma once

#include <string>
#include <cstdint>

class Utils {
    public:
        static std::string string_to_hex(const std::string &str);
        static std::string int_to_hex(uint8_t val);
};
