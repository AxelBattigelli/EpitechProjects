#include "main.hpp"
#include "Shared/Exception.hpp"
#include "ffi.hpp"
#include <cerrno>
#include <iostream>
#include <ncurses.h>

struct frontend_handle {
    NcursesFrontend inner;
    std::string lastString;
};

NcursesFrontend::NcursesFrontend()
{
    std::clog << "Creating the Logger frontend!\n";
}

void NcursesFrontend::init()
{
    std::clog << "Initing the Logger frontend!\n";
}

void NcursesFrontend::deinit()
{
    std::clog << "Deiniting the Logger frontend!\n";
}

NcursesFrontend::~NcursesFrontend()
{
    std::clog << "Deleting the Logger frontend!\n";
}

int color(uint8_t color)
{
    return (int) (((double) color) * 1000.0 / 256.0);
}

void NcursesFrontend::renderMap(tile_t **data, uint64_t length, const uint64_t *lengths)
{
    int pair = 0;
    std::clog << "RENDER\n";
    for (uint64_t y = 0; y < length; ++y) {
        std::clog << "LINE\n";
        for (uint64_t x = 0; x < lengths[y]; ++x) {
            auto &tile = data[y][x];
            std::clog << "TILE " << x << " " << y << ": '"
                      << std::string(tile.image_path.ptr, tile.image_path.len) << " ,"
                      << tile.image_path.len << "\n";
        }
    }
}

event_t NcursesFrontend::getEvent()
{
    int cha = '\n';
    if (cha == ERR)
        return event_t{.other = event_other_t{.type = EVT_OTHER}};
    if (cha == 0) {
        std::clog << "TODO: Mouse\n";
        return event_t{.other = event_other_t{.type = EVT_OTHER}};
    }
    switch (cha) {
        case 0:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_UP}};
        case 1:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_DOWN}};
        case 2:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_LEFT}};
        case 3:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_RIGHT}};
        case 27:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_ESCAPE}};
        case '\r':
        case '\n':
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_ENTER}};
        case ' ':
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_SPACE}};
        default:
            if (cha >= 33 && cha <= 126)
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = (key_e) cha}};
            else
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_UNKNOWN}};
    }
}

extern "C"
{
    extern const lib_info_t ARCADE_AACMMN_LIB_INFO = {.magic = LIB_MAGIC_VALUE,
        .api_version = LIB_API_VERSION,
        .type = "FRONTEND",
        .name = {.ptr = "Logger (AA)", .len = 11},
        .id = {.ptr = "eu.epitech.anicetaxel.logger", .len = 28}};

    frontend_handle_t *frontend_getInstance()
    {
        try {
            auto *handle = new frontend_handle{
                .inner = NcursesFrontend(),
            };
            return handle;
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    }

    void frontend_destroyInstance(frontend_handle_t *hdl)
    {
        try {
            delete hdl;
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    }

    void frontend_init(frontend_handle_t *handle)
    {
        try {
            handle->inner.init();
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    };

    void frontend_deinit(frontend_handle_t *handle)
    {
        try {
            handle->inner.deinit();
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    };

    void frontend_renderMap(
        frontend_handle_t *hdl, tile_t **data, uint64_t length, const uint64_t *lengths)
    {
        try {
            hdl->inner.renderMap(data, length, lengths);
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    }

    event_t frontend_getEvent(frontend_handle_t *hdl)
    {
        try {
            return hdl->inner.getEvent();
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    }
}
