#include "GameLayer.hpp"
#include "common.hpp"
#include "iostream"

GameLayer::GameLayer()
{
    this->_hdl = game_getInstance();
}

GameLayer::~GameLayer()
{
    if (this->_hdl)
        game_destroyInstance(this->_hdl);
    this->_hdl = nullptr;
}

const Arcade::Shared::Data::Map &GameLayer::GetMap() const
{
    auto self = const_cast<GameLayer *>(this);
    self->_map.clear();
    uint64_t length;
    uint64_t *lengths;
    auto inMap = game_getMap(this->_hdl, &length, &lengths);
    std::vector<std::vector<tile_t>> interMap;

    for (uint64_t i = 0; i < length; ++i) {
        std::vector<tile_t> outLine;
        for (uint64_t j = 0; j < lengths[i]; ++j) {
            outLine.push_back(inMap[i][j]);
        }
        interMap.push_back(outLine);
    }

    for (const auto &inLine : interMap) {
        Arcade::Shared::Data::Map::value_type outLine;
        for (const auto &inTile : inLine) {
            outLine.push_back(std::move(convertFrToGrTile(inTile)));
        }
        self->_map.push_back(std::move(outLine));
    }
    return self->_map;
}

void GameLayer::UpdateMap(const Arcade::Shared::Data::Event event)
{
    game_updateMap(this->_hdl, convertGrToFrEvent(event));
}

void GameLayer::Reset()
{
    game_reset(this->_hdl);
}

std::size_t GameLayer::GetScore() const
{
    return game_getScore(this->_hdl);
}

extern "C"
{
    const char *TYPE = "Game";
    const char *NAME = "Game x";

    Arcade::Game::IGame *GetInstance()
    {
        return new GameLayer();
    }
}
