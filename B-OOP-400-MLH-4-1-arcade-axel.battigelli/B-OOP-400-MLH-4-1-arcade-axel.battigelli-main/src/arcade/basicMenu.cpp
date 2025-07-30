#include "basicMenu.hpp"
#include "Shared/Exception.hpp"
#include "utils.hpp"
#include <dlfcn.h>
#include <format>
#include <ranges>

std::vector<tile_t> createLine(std::string value, color_t fg, color_t bg)
{
    std::vector<tile_t> ret = {};

    if (value.size() % 2 != 0)
        value.push_back(' ');
    for (const auto &values : value | std::views::chunk(2)) {
        ret.push_back(tile_t{{.ptr = nullptr, .len = 0}, {values[0], values[1]}, fg, bg});
    }
    return ret;
}

std::string BasicMenu::run(std::string &frontend, std::string &game, Frontend &display, bool &run)
{
    size_t gameIdx = 0, frontendIdx = 0;

    int field = 1;
    std::string name = "NICK";
    while (field != 0) {
        auto map = std::vector<std::vector<tile_t>>();
        auto selected_color = (field == 1) ? color_t{127, 127, 255} : color_t{255, 255, 255};
        map.push_back(std::move(createLine(
            "Available graphical libraries:", color_t{255, 255, 255}, color_t{0, 0, 0})));
        for (std::tuple<void *, const lib_info_t &, std::size_t> frontend :
            std::ranges::views::zip(
                this->frontends, this->frontend_infos, std::views::iota(0))) {
            color_t fg = (std::get<2>(frontend) == frontendIdx) ? color_t{0, 0, 0}
                                                                : color_t{255, 255, 255};
            color_t bg =
                (std::get<2>(frontend) == frontendIdx) ? selected_color : color_t{0, 0, 0};
            if (std::get<1>(frontend).name.ptr == nullptr
                || std::get<1>(frontend).name.len == 0) {
                map.push_back(
                    std::move(createLine(getLibraryPath(std::get<0>(frontend)), fg, bg)));
            } else {
                map.push_back(std::move(createLine(std::string(std::get<1>(frontend).name.ptr,
                                                        std::get<1>(frontend).name.len),
                    fg, bg)));
            }
        }

        map.emplace_back();
        selected_color = (field == 2) ? color_t{127, 127, 255} : color_t{255, 255, 255};
        map.push_back(std::move(
            createLine("Available games:", color_t{255, 255, 255}, color_t{0, 0, 0})));
        for (std::tuple<void *, const lib_info_t &, std::size_t> game :
            std::ranges::views::zip(this->games, this->game_infos, std::views::iota(0))) {
            color_t fg =
                (std::get<2>(game) == gameIdx) ? color_t{0, 0, 0} : color_t{255, 255, 255};
            color_t bg = (std::get<2>(game) == gameIdx) ? selected_color : color_t{0, 0, 0};
            if (std::get<1>(game).name.ptr == nullptr || std::get<1>(game).name.len == 0) {
                map.push_back(
                    std::move(createLine(getLibraryPath(std::get<0>(game)), fg, bg)));
            } else {
                map.push_back(std::move(createLine(
                    std::string(std::get<1>(game).name.ptr, std::get<1>(game).name.len), fg,
                    bg)));
            }
        }

        map.emplace_back();
        selected_color = (field == 3) ? color_t{127, 127, 255} : color_t{0, 0, 0};
        map.push_back(std::move(
            createLine("Scores for this game:", color_t{255, 255, 255}, selected_color)));
        map.emplace_back();
        selected_color = (field == 4) ? color_t{127, 127, 255} : color_t{0, 0, 0};
        std::vector<tile_t> line =
            createLine("Player name:", color_t{255, 255, 255}, selected_color);
#if __cpp_lib_containers_ranges
        line.append_range(createLine("  ", color_t{255, 255, 255}, color_t{0, 0, 0}));
#else
        {
            auto tmp = createLine("  ", color_t{255, 255, 255}, color_t{0, 0, 0});
            line.insert(line.end(), tmp.cbegin(), tmp.cend());
        }
#endif
#if __cpp_lib_containers_ranges
        line.append_range(createLine(name, color_t{255, 255, 255}, color_t{0, 0, 0}));
#else
        {
            auto tmp = createLine(name, color_t{255, 255, 255}, color_t{0, 0, 0});
            line.insert(line.end(), tmp.cbegin(), tmp.cend());
        }
#endif
        map.push_back(line);
        map.emplace_back();
        selected_color = (field == 5) ? color_t{127, 127, 255} : color_t{0, 0, 0};
        map.push_back(
            std::move(createLine("Launch Game!", color_t{255, 255, 255}, selected_color)));
        display.renderMap(map);
        auto evt = display.getEvent();

        if (evt.type == EVT_KB) {
            if (field == 4
                && ((evt.keyboard.key >= 'A' && evt.keyboard.key <= 'Z')
                    || (evt.keyboard.key >= 'a' && evt.keyboard.key <= 'z')
                    || (evt.keyboard.key >= '0' && evt.keyboard.key <= '9'))) {
                name.push_back((char) evt.keyboard.key);
            } else if (field == 4 && evt.keyboard.key == KB_BACKSPACE) {
                name.pop_back();
            } else if (evt.keyboard.key == KB_UP && field == 2) {
                gameIdx = (gameIdx > 0) ? gameIdx - 1 : this->games.size() - 1;
            } else if (evt.keyboard.key == KB_DOWN && field == 2) {
                gameIdx = (gameIdx + 1) % this->games.size();
            } else if (evt.keyboard.key == KB_UP && field == 1) {
                frontendIdx = (frontendIdx > 0) ? frontendIdx - 1 : this->frontends.size() - 1;
            } else if (evt.keyboard.key == KB_DOWN && field == 1) {
                frontendIdx = (frontendIdx + 1) % this->frontends.size();
            } else if (evt.keyboard.key == KB_TAB) {
                field++;
                if (field > 5)
                    field = 1;
            } else if (evt.keyboard.key == KB_ENTER && field == 5) {
                field = 0;
            } else if (evt.keyboard.key == KB_LOWER_Q || evt.keyboard.key == KB_UPPER_Q) {
                run = false;
                return "QUITTING";
            }
        }
    }

    Dl_info info;
    if (dladdr(dlsym(this->games[gameIdx], "TYPE"), &info) == 0) {
        std::string error = dlerror();
        throw arcade::shared::Exception(
            std::format("Error loading shared library: {}\nThe library is likely not a "
                        "compatible library",
                error));
    }
    game = std::string(info.dli_fname);
    if (dladdr(dlsym(this->frontends[frontendIdx], "TYPE"), &info) == 0) {
        std::string error = dlerror();
        throw arcade::shared::Exception(
            std::format("Error loading shared library: {}\nThe library is likely not a "
                        "compatible library",
                error));
    }
    frontend = std::string(info.dli_fname);
    return "NONAME";
}
