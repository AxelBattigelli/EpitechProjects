#pragma once

#include <cstdlib>
#include <cstring>
#include <string>

class Utils
{
private:
public:
    static std::string ReplaceAll(std::string str, const std::string &from, const std::string &to);
    static std::string toUpperString(const std::string &command);
    static std::string Exec(const char *cmd);
};

