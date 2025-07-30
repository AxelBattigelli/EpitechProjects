#include "ffi.hpp"
#include "frontend.hpp"
#include <string>
#include <vector>

#pragma once

class BasicMenu {
  private:
    std::vector<void *> games;
    std::vector<lib_info_t> game_infos;
    std::vector<void *> frontends;
    std::vector<lib_info_t> frontend_infos;

  public:
    BasicMenu(std::vector<void *> games, std::vector<lib_info_t> game_infos,
        std::vector<void *> frontends, std::vector<lib_info_t> frontend_infos)
        : games(games), game_infos(game_infos), frontends(frontends),
          frontend_infos(frontend_infos) {};

    std::string run(std::string &frontend, std::string &game, Frontend &display, bool &run);
};