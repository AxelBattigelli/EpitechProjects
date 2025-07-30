#include "Shared/Exception.hpp"
#include "ffi.hpp"
#include "utils.hpp"
#include <cstring>
#include <dlfcn.h>
#include <filesystem>
#include <format>
#include <string>

std::string getLibraryPath(void *handle)
{
    Dl_info info;
    if (dladdr(dlsym(handle, "TYPE"), &info) == 0) {
        std::string error = dlerror();
        throw arcade::shared::Exception(
            std::format("Error loading shared library: {}\nThe library is likely not a "
                        "compatible library",
                error));
    }
    return std::string(info.dli_fname);
}

void *getLibInfo(const std::filesystem::path &path, lib_info_t &out)
{
    void *handle = dlopen(path.c_str(), RTLD_LAZY | RTLD_LOCAL);

    if (handle == nullptr) {
        throw arcade::shared::Exception(
            std::format("Error loading shared library: {}", dlerror()));
    }
    dlerror();

    lib_info_t *info = (lib_info_t *) dlsym(handle, "ARCADE_AACMMN_LIB_INFO");
    if (info == nullptr) {
        char **type = (char **) dlsym(handle, "TYPE");
        if (type == nullptr) {
            std::string error = dlerror();
            dlclose(handle);
            throw arcade::shared::Exception(std::format(
                "Error loading shared library: {}\nThe library is likely not a compatible library",
                error));
        }
        out.magic = LIB_MAGIC_VALUE;
        out.api_version = 0;
        if (std::string("Graphical") == *type)
            std::memcpy(&out.type, "FRONTEND", 9);
        else if (std::string("Game") == *type)
            std::memcpy(&out.type, "GAME", 5);
        else
            std::memcpy(&out.type, "UNKNOWN", 8);
        out.name.ptr = nullptr;
        out.name.len = 0;
        out.id.ptr = nullptr;
        out.id.len = 0;
        return handle;
    }

    if (info->magic != LIB_MAGIC_VALUE) {
        dlclose(handle);
        throw arcade::shared::Exception(std::format(
            "Error loading shared library: Magic is wrong\nThe library is likely corrupted"));
    }

    if (info->api_version != LIB_API_VERSION) {
        dlclose(handle);
        throw arcade::shared::Exception(
            std::format("Error loading shared library: Got version {}, but expected {}\nThe "
                        "library is either outdated or too new for this runner",
                info->api_version, LIB_API_VERSION));
    }

    out = *info;
    return handle;
}
