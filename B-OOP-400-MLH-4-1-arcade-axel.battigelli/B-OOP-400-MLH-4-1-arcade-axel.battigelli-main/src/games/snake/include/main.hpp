#pragma once

#include "ffi.hpp"
#include <cerrno>
#include <chrono>
#include <deque>
#include <iostream>
#include <vector>

class SnakeGame {
  private:
    uint64_t _score = 0;
    std::deque<std::pair<int, int>> _snake = {};
    std::pair<int, int> _snakeVelocity;
    std::pair<int, int> _food;
    std::vector<std::vector<tile_t>> _tiles = {};
    std::vector<uint64_t> _lengths = {};
    std::vector<tile_t *> _tilesLines = {};
    std::chrono::steady_clock::time_point _nextUpdate =
        std::chrono::steady_clock::time_point::min();

    void updateTiles();
    void placeFood();
    void displayGameOver();
    void displayScore();

  public:
    SnakeGame();
    ~SnakeGame();

    tile_t **getMap(uint64_t *length, uint64_t **lengths);
    void reset();
    void updateMap(event_t evt);
    uint64_t getScore();
};

struct game_handle {
    SnakeGame inner;
};
