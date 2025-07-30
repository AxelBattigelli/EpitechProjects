#pragma once

#include "Game/IGame.hpp"
#include "ffi.hpp"

class GameLayer : public Arcade::Game::IGame {
  private:
    game_handle_t *_hdl = nullptr;
    Arcade::Shared::Data::Map _map = {};

  public:
    GameLayer();
    virtual ~GameLayer() override;
    virtual const Arcade::Shared::Data::Map &GetMap() const override;
    virtual void UpdateMap(const Arcade::Shared::Data::Event event) override;
    virtual void Reset() override;
    virtual std::size_t GetScore() const override;
};
