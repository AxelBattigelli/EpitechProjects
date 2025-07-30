#include "Shared/Exception.hpp"
#include "ffi.hpp"
#include "game.hpp"
#include "utils.hpp"
#include <cstring>
#include <dlfcn.h>
#include <filesystem>
#include <string>
#include <format>

void Game::load(std::string path)
{
    lib_info_t info;

    this->dlHandle = getLibInfo(path, info);
    dlclose(this->dlHandle);
    this->dlHandle = dlopen(path.c_str(), RTLD_NOW | RTLD_LAZY);
    if (info.api_version == 0) {
        using fn = Arcade::Game::IGame *();
        auto fun = dlSym<fn>(this->dlHandle, "GetInstance");
        this->grHandle = fun();
        return;
    }
    this->frDestroyInstance =
        dlSym<decltype(game_destroyInstance)>(this->dlHandle, "game_destroyInstance");
    this->frReset = dlSym<decltype(game_reset)>(this->dlHandle, "game_reset");
    this->frGetMap = dlSym<decltype(game_getMap)>(this->dlHandle, "game_getMap");
    this->frUpdateMap = dlSym<decltype(game_updateMap)>(this->dlHandle, "game_updateMap");
    this->frGetScore = dlSym<decltype(game_getScore)>(this->dlHandle, "game_getScore");
    auto fun = dlSym<decltype(game_getInstance)>(this->dlHandle, "game_getInstance");
    this->frHandle = fun();
}

Game::~Game()
{
    if (this->grHandle) {
        this->grHandle->Reset();
        delete this->grHandle;
        this->grHandle = nullptr;
    }
    if (this->frHandle) {
        this->frDestroyInstance(this->frHandle);
        this->frHandle = nullptr;
    }
    if (this->dlHandle) {
        dlclose(this->dlHandle);
        this->dlHandle = nullptr;
    }
}

std::vector<std::vector<tile_t>> Game::getMapGr()
{
    const auto &inMap = this->grHandle->GetMap();
    std::vector<std::vector<tile_t>> outMap;
    for (const auto &inLine : inMap) {
        std::vector<tile_t> outLine;
        for (const auto &inTile : inLine) {
            outLine.push_back(convertGrToFrTile(inTile));
        }
        outMap.push_back(std::move(outLine));
    }
    return std::move(outMap);
}

std::vector<std::vector<tile_t>> Game::getMapFr()
{
    uint64_t length;
    uint64_t *lengths;
    auto inMap = this->frGetMap(this->frHandle, &length, &lengths);
    std::vector<std::vector<tile_t>> outMap;

    for (uint64_t i = 0; i < length; ++i) {
        std::vector<tile_t> outLine;
        for (uint64_t j = 0; j < lengths[i]; ++j) {
            outLine.push_back(inMap[i][j]);
        }
        outMap.push_back(outLine);
    }
    return outMap;
}

std::vector<std::vector<tile_t>> Game::getMap()
{
    if (this->grHandle)
        return this->getMapGr();
    else if (this->frHandle)
        return this->getMapFr();
    return {};
}

void Game::updateMap(event_t event)
{
    if (this->grHandle)
        this->updateMapGr(event);
    else if (this->frHandle)
        this->updateMapFr(event);
}

void Game::reset()
{
    if (this->grHandle)
        this->resetGr();
    else if (this->frHandle)
        this->resetFr();
}
