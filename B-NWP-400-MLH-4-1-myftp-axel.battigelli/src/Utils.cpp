#include "Utils.hpp"
#include <cctype>
#include <cstdio>
#include <cstring>
#include <memory>
#include <string>

std::string Utils::ReplaceAll(std::string str, const std::string &from, const std::string &to)
{
    size_t start_pos = 0;
    while((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length();
    }
    return str;
}

std::string Utils::toUpperString(const std::string &command)
{
    std::string tmpCommand;
    for (size_t x = 0; x < std::strlen(command.c_str()); ++x)
        tmpCommand += (toupper(command[x]));
    return std::string(tmpCommand);
}

std::string Utils::Exec(const char *cmd)
{
    std::shared_ptr<FILE> const pipe(popen(cmd, "r"), pclose);
    if (!pipe)
        return "ERROR";
    char buffer[128];
    std::string result;
    while (feof(pipe.get()) == 0) {
        if (fgets(buffer, 128, pipe.get()) != nullptr)
            result += buffer;
    }
    return result;
}
