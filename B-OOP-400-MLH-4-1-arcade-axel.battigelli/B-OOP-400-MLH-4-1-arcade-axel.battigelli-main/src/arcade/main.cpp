#include "Game/IGame.hpp"
#include "Graphical/IGraphical.hpp"
#include "Shared/Data.hpp"
#include "Shared/Exception.hpp"
#include "common.hpp"
#include "ffi.hpp"
#include <cstdio>
#include <cstring>
#include <format>
#include <iostream>
#include <istream>
#include <ranges>
#include <sstream>
#include <streambuf>
#include <string>
#include <vector>
#include <filesystem>
#include <dlfcn.h>
#include "basicMenu.hpp"
#include "game.hpp"
#include "frontend.hpp"
#include "utils.hpp"

int main(int argc, char *argv[])
{
    for (int i = 0; i < argc; ++i) {
        if (std::strcmp(argv[i], "-h") == 0 || std::strcmp(argv[i], "--help") == 0) {
            std::cout << "Man arcade\n"
                      << "\tRun many games on many interfaces \n"
                      << "Usage :\n"
                      << "\t" << argv[0] << " [lib/lib_graphic.so]\n";
            return 0;
        }
    }
    try {
        if (argc < 2 || argc > 2)
            throw arcade::shared::Exception("Bad usage");
        bool run = true;
        while (run) {
            std::vector<void *> games;
            std::vector<lib_info_t> game_infos;
            std::vector<void *> frontends;
            std::vector<lib_info_t> frontend_infos;
            std::vector<void *> handles;

            for (const auto &dir_entry : std::filesystem::directory_iterator("./lib/")) {
                if (!dir_entry.is_regular_file())
                    continue;
                try {
                    lib_info_t libInfo;
                    handles.push_back(getLibInfo(dir_entry.path(), libInfo));
                    std::string type(libInfo.type);
                    if (type == "FRONTEND") {
                        frontends.push_back(handles.back());
                        frontend_infos.push_back(libInfo);
                    }
                    if (type == "GAME") {
                        games.push_back(handles.back());
                        game_infos.push_back(libInfo);
                    }
                } catch (const arcade::shared::Exception &e) {
                    std::cerr << "Warning: " << e << '\n';
                }
            }

            std::string frontendPath = "./lib/arcade_sdl2.so";
            std::string gamePath = "./lib/arcade_minesweeper.so";
            std::string playerName = "";

            {
                auto menu_frontend = Frontend();
                menu_frontend.load(argv[1]);
                auto menu = BasicMenu(games, game_infos, frontends, frontend_infos);
                menu.run(frontendPath, gamePath, menu_frontend, run);
            }

            if (!run)
                continue;

            {
                auto game = Game();
                auto frontend = Frontend();
                game.load(gamePath);
                frontend.load(frontendPath);
                game.reset();
                bool run_game = true;
                while (run_game) {
                    auto map = game.getMap();
                    frontend.renderMap(map);
                    auto evt = frontend.getEvent();
                    if (evt.type == EVT_KB && evt.keyboard.key == KB_LOWER_Q) {
                        run_game = false;
                    }
                    if (evt.type == EVT_KB && evt.keyboard.key == KB_LOWER_L) {
                        {
                            auto old_frontend = std::move(frontend);
                        }
                        frontend = Frontend();
                        frontend.load(getLibraryPath(frontends[std::rand() % frontends.size()]));
                    }
                    game.updateMap(evt);
                }
            }

            for (auto h : handles) {
                dlclose(h);
            }
        }
    } catch (const arcade::shared::Exception &e) {
        std::cerr << "Error: " << e << '\n';
        return 84;
    } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what()
                  << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
        return 84;
    } catch (...) {
        std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                  << std::current_exception().__cxa_exception_type()->name() << '\n';
        return 84;
    }
}
