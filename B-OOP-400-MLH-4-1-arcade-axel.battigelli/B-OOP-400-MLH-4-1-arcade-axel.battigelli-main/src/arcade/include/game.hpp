#include "Game/IGame.hpp"
#include <common.hpp>
#include <ffi.hpp>
#include <vector>

#pragma once

class Game {
  private:
    void *dlHandle = nullptr;
    game_handle_t *frHandle = nullptr;
    Arcade::Game::IGame *grHandle = nullptr;

    decltype(game_destroyInstance) *frDestroyInstance = nullptr;
    decltype(game_reset) *frReset = nullptr;
    decltype(game_getMap) *frGetMap = nullptr;
    decltype(game_updateMap) *frUpdateMap = nullptr;
    decltype(game_getScore) *frGetScore = nullptr;

    std::vector<std::vector<tile_t>> getMapGr();
    std::vector<std::vector<tile_t>> getMapFr();

    void updateMapGr(event_t event) { this->grHandle->UpdateMap(convertFrToGrEvent(event)); }
    void updateMapFr(event_t event) { this->frUpdateMap(this->frHandle, event); }

    void resetGr() { this->grHandle->Reset(); }
    void resetFr() { this->frReset(this->frHandle); }

  public:
    void load(std::string path);
    ~Game();
    std::vector<std::vector<tile_t>> getMap();
    void updateMap(event_t event);
    void reset();
};
