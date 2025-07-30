#pragma once

#include "ffi.hpp"
#include <cstring>
#include <dlfcn.h>
#include <filesystem>
#include <format>
#include <string>

std::string getLibraryPath(void *handle);

void *getLibInfo(const std::filesystem::path &path, lib_info_t &out);

template<typename T> T *dlSym(void *handle, const std::string &name)
{
    auto fun = (T *) dlsym(handle, name.c_str());
    if (fun == nullptr) {
        std::string error = dlerror();
        throw arcade::shared::Exception(
            std::format("Error loading shared library: {}\nThe library is likely not a "
                        "compatible library",
                error));
    }
    return fun;
}
