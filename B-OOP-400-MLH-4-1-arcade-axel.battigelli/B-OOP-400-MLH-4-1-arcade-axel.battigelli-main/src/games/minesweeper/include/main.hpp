#pragma once

#include "ffi.hpp"
#include <cerrno>
#include <chrono>
#include <deque>
#include <iostream>
#include <vector>

class MinesweeperGame {
  private:
    uint64_t _score = 0;
    bool _isFirstType = true;
    int _countdownTime = 200;
    bool _gameOver = false;
    std::vector<std::pair<int, int>> _minesPos = {};
    std::array<std::array<int, 15>, 15> _numbers;
    std::array<std::array<bool, 15>, 15> _isRevealed {{}};

    std::vector<std::vector<tile_t>> _tiles = {};
    std::vector<uint64_t> _lengths = {};
    std::vector<tile_t *> _tilesLines = {};
    std::vector<std::pair<int, int>> _flags = {};
    std::chrono::steady_clock::time_point _lastUpdateTime;

    void updateTiles();
    void displayGameOver();
    void displayScore();
    bool isFlagged(int x, int y) const;
    void placeMines(int firstClickX, int firstClickY);
    void calculateNumbers();
    void discoverCell(int x, int y);
    void displayCountdown();

  public:
    MinesweeperGame();
    ~MinesweeperGame();

    tile_t **getMap(uint64_t *length, uint64_t **lengths);
    void reset();
    void updateMap(event_t evt);
    uint64_t getScore();
};

struct game_handle {
    MinesweeperGame inner;
};
