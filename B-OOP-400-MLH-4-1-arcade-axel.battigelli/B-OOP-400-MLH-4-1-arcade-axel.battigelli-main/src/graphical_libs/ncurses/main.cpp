#include "main.hpp"
#include "Shared/Exception.hpp"
#include "ffi.hpp"
#include <cerrno>
#include <iostream>
#include <map>
#include <ncurses.h>
#include <utility>
#include <vector>

struct frontend_handle {
    NcursesFrontend inner;
    std::string lastString;
};

NcursesFrontend::NcursesFrontend()
{
    std::clog << "Creating the NCURSES frontend!\n";
}

void NcursesFrontend::init()
{
    std::clog << "Initing the NCURSES frontend!\n";
    this->_window = initscr();
    clear();
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    mousemask(ALL_MOUSE_EVENTS, NULL);
    mouseinterval(0);
    timeout(1);
    idlok(stdscr, TRUE);
    leaveok(stdscr, TRUE);
    curs_set(0);

    start_color();
    refresh();
}

void NcursesFrontend::deinit()
{
    endwin();
    std::clog << "Deiniting the NCURSES frontend!\n";
}

NcursesFrontend::~NcursesFrontend()
{
    std::clog << "Deleting the NCURSES frontend!\n";
}

int color(uint8_t color)
{
    return (int) (((double) color) * 1000.0 / 256.0);
}

int getColorPair(
    std::vector<std::pair<std::pair<color_t, color_t>, std::pair<int, std::pair<int, int>>>> &map,
    color_t front, color_t back)
{
    int first_pair = 0;
    int first_color = 0;
    for (const auto &element : map) {
        if (element.first.first.r == front.r && element.first.first.g == front.g
            && element.first.first.b == front.b && element.first.second.r == back.r
            && element.first.second.g == back.g && element.first.second.b == back.b)
            return element.second.first;
        first_pair = std::max(element.second.first, first_pair);
        first_color = std::max(element.second.second.first, first_color);
        first_color = std::max(element.second.second.second, first_color);
    }
    first_pair++;
    first_color++;
    init_extended_color(first_color, color(front.r), color(front.g), color(front.b));
    init_extended_color(first_color + 1, color(back.r), color(back.g), color(back.b));
    init_extended_pair(first_pair, first_color, first_color + 1);
    map.emplace_back(std::make_pair(front, back),
        std::make_pair(first_pair, std::make_pair(first_color, first_color + 1)));
    return first_pair;
}

void NcursesFrontend::renderMap(tile_t **data, uint64_t length, const uint64_t *lengths)
{
    std::vector<std::pair<std::pair<color_t, color_t>, std::pair<int, std::pair<int, int>>>>
        color_map = {};
    for (uint64_t y = 0; y < length; ++y) {
        for (uint64_t x = 0; x < lengths[y]; ++x) {
            auto tile = data[y][x];
            int colorIdx = getColorPair(color_map, tile.front, tile.back);
            wattr_set(this->_window, A_NORMAL, colorIdx, &colorIdx);
            mvwaddch(this->_window, y, x * 2, tile.characters[0]);
            mvwaddch(this->_window, y, (x * 2) + 1, tile.characters[1]);
        }
    }
    refresh();
}

event_t NcursesFrontend::getEvent()
{
    refresh();
    halfdelay(1);

    int cha = getch();
    if (cha == ERR)
        return event_t{.other = event_other_t{.type = EVT_OTHER}};
    if (cha == KEY_MOUSE) {
        MEVENT event;
        if (getmouse(&event) == OK) {
            if (event.bstate & BUTTON1_RELEASED) {
                return event_t{.mouse = event_mouse_t{
                                   .type = EVT_MOUSE,
                                   .position{.x = (uint64_t) event.x / 2, .y = (uint64_t) event.y},
                                   .button = MOUSE_LEFT,
                               }};
            } else if (event.bstate & BUTTON3_RELEASED) {
                return event_t{.mouse = event_mouse_t{
                                   .type = EVT_MOUSE,
                                   .position{.x = (uint64_t) event.x / 2, .y = (uint64_t) event.y},
                                   .button = MOUSE_RIGHT,
                               }};
            } else if (event.bstate & BUTTON2_RELEASED) {
                return event_t{.mouse = event_mouse_t{
                                   .type = EVT_MOUSE,
                                   .position{.x = (uint64_t) event.x / 2, .y = (uint64_t) event.y},
                                   .button = MOUSE_CENTER,
                               }};
            }
        }
    }
    switch (cha) {
        case KEY_UP:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_UP}};
        case KEY_DOWN:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_DOWN}};
        case KEY_LEFT:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_LEFT}};
        case KEY_RIGHT:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_RIGHT}};
        case 27:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_ESCAPE}};
        case '\r':
        case '\n':
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_ENTER}};
        case ' ':
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_SPACE}};
        case '\t':
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_TAB}};
        case KEY_BACKSPACE:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_BACKSPACE}};
        case KEY_DC:
            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_DELETE}};
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
        .name = {.ptr = "Ncurses (AA)", .len = 12},
        .id = {.ptr = "eu.epitech.anicetaxel.ncurses", .len = 29}};

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
