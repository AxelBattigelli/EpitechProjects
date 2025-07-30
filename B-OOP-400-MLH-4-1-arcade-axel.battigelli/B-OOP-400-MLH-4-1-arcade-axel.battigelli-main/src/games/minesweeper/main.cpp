#include "main.hpp"
#include "Shared/Exception.hpp"
#include "ffi.hpp"
#include <cstring>
#include <iostream>
#include <set>

MinesweeperGame::MinesweeperGame()
{
    std::clog << "Initing Minesweeper\n";
}
MinesweeperGame::~MinesweeperGame()
{
    std::clog << "Deiniting Minesweeper\n";
}

void MinesweeperGame::updateTiles()
{
    this->_lengths.clear();
    this->_tilesLines.clear();

    for (auto &line : this->_tiles) {
        this->_lengths.push_back(line.size());
        this->_tilesLines.push_back(line.data());
    }
}

tile_t **MinesweeperGame::getMap(uint64_t *length, uint64_t **lengths)
{
    this->updateTiles();
    *length = this->_tilesLines.size();
    *lengths = this->_lengths.data();
    return this->_tilesLines.data();
}

void MinesweeperGame::reset()
{
    this->_countdownTime = 200;
    this->_gameOver = false;
    this->_score = 0;
    this->_isFirstType = true;
    this->_flags.clear();
    this->_minesPos.clear();

    this->_lastUpdateTime = std::chrono::steady_clock::now();

    for (auto &row : this->_numbers)
        row.fill(0);

    for (auto &row : this->_isRevealed)
        row.fill(false);

    this->_tiles.clear();
    this->_tiles.resize(21,
        std::vector<tile_t>(19,
            tile_t{
                .image_path = {.ptr = nullptr, .len = 0},
                // .image_path = {.ptr = "assets/minesweeper/case.png", .len = 27},
                .characters = {'[', ']'},
                .front = {255, 255, 255},
                .back = {0, 0, 0},
            }));
    const tile_t wall = {
        .image_path = {.ptr = "assets/minesweeper/tile_border.png", .len = 34},
        .characters = {'#', '#'},
        .front = {255, 255, 255},
        .back = {0, 0, 0},
    };

    for (int y = 0; y < 2; ++y)
        for (int x = 0; x < 19; ++x)
            this->_tiles[y][x] = wall;
    for (int y = 17; y < 19; ++y)
        for (int x = 0; x < 19; ++x)
            this->_tiles[y][x] = wall;
    for (int y = 2; y < 17; ++y) {
        this->_tiles[y][0] = wall;
        this->_tiles[y][1] = wall;
        this->_tiles[y][17] = wall;
        this->_tiles[y][18] = wall;
    }

    const tile_t empty = {
        .image_path = {.ptr = "assets/snake/bg.png", .len = 19},
        .characters = {' ', ' '},
        .front = {255, 255, 255},
        .back = {0, 0, 0},
    };

    for (int y = 19; y < 21; ++y) {
        for (int x = 0; x < 19; ++x) {
            this->_tiles[y][x] = empty;
        }
    }
}

void MinesweeperGame::displayGameOver()
{
    const std::string message = " GAME  OVER ";
    int centerY = this->_tiles.size() / 2;
    int centerX = (this->_tiles[0].size() - message.size()) / 2;

    for (size_t i = 0; i < message.size(); ++i) {
        this->_tiles[centerY][centerX + i] = tile_t{.image_path = {.ptr = nullptr, .len = 0},
            .characters = {message[i], ' '},
            .front = {255, 0, 0},
            .back = {200, 200, 200}};
    }
}

void MinesweeperGame::displayScore()
{
    const std::string message = "Score: " + std::to_string(this->_score);

    for (size_t i = 0, tileX = 0; i < message.size() && tileX < this->_tiles[19].size(); ++tileX) {
        char first = message[i];
        char second = (i + 1 < message.size()) ? message[i + 1] : ' ';

        this->_tiles[19][tileX] = tile_t{
            .image_path = {.ptr = nullptr, .len = 0},
            .characters = {first, second},
            .front = {255, 255, 255},
            .back = {0, 0, 0}
        };
        i += 2;
    }
}

void MinesweeperGame::displayCountdown()
{
    int minutes = this->_countdownTime / 60;
    int seconds = this->_countdownTime % 60;
    std::string message = "Time: " + std::to_string(minutes) + ":" + (seconds < 10 ? "0" : "") + std::to_string(seconds);

    for (size_t i = 0, tileX = 0; i < message.size() && tileX < this->_tiles[20].size(); ++tileX) {
        char first = message[i];
        char second = (i + 1 < message.size()) ? message[i + 1] : ' ';

        this->_tiles[20][tileX] = tile_t{
            .image_path = {.ptr = nullptr, .len = 0},
            .characters = {first, second},
            .front = {255, 255, 255},
            .back = {0, 0, 0}
        };
        i += 2;
    }
}
void MinesweeperGame::placeMines(int firstClickX, int firstClickY)
{
    srand(time(0));
    this->_minesPos.clear();

    std::set<std::pair<int, int>> used;

    while (this->_minesPos.size() < 10) {
        int x = rand() % 15 + 2;
        int y = rand() % 15 + 2;

        if ((x == firstClickX && y == firstClickY) || used.count({x, y}) > 0)
            continue;

        this->_minesPos.emplace_back(x, y);
        used.insert({x, y});
    }
}

void MinesweeperGame::calculateNumbers()
{
    for (auto& row : this->_numbers)
        row.fill(0);

    for (const auto& [mx, my] : this->_minesPos) {
        for (int dy = -1; dy <= 1; ++dy) {
            for (int dx = -1; dx <= 1; ++dx) {
                int nx = mx + dx;
                int ny = my + dy;

                if (nx < 0 || ny < 0 || nx >= 15 || ny >= 15 || (nx == mx && ny == my))
                    continue;

                this->_numbers[ny][nx] += 1;
            }
        }
    }
}

bool MinesweeperGame::isFlagged(int x, int y) const
{
    return std::find(this->_flags.begin(), this->_flags.end(), std::make_pair(x, y)) != this->_flags.end();
}

void MinesweeperGame::discoverCell(int x, int y)
{
    if (x < 2 || y < 2 || x >= 17 || y >= 17)
        return;

    int nx = x - 2;
    int ny = y - 2;

    if (_isRevealed[ny][nx])
        return;
    if (isFlagged(x, y))
        return;

    _isRevealed[ny][nx] = true;

    for (const auto &[mx, my] : _minesPos) {
        if (mx == x && my == y) {
            displayGameOver();
            return;
        }
    }

    int number = _numbers[ny][nx];

    std::string image_path_str = "assets/minesweeper/tile_" + std::to_string(number) + ".png";
    if (number > 0) {
        _tiles[y][x] = tile_t{
            .image_path = {.ptr = new char[image_path_str.size()], .len = 29},
            .characters = {static_cast<char>('0' + number), ' '},
            .front = {255, 255, 255},
            .back = {0, 0, 0}
        };
        std::strcpy(_tiles[y][x].image_path.ptr, image_path_str.c_str());
    } else {
        _tiles[y][x] = tile_t{
            .image_path = {.ptr = nullptr, .len = 0},
            .characters = {' ', ' '},
            .front = {255, 255, 255},
            .back = {0, 0, 0}
        };

        for (int dy = -1; dy <= 1; ++dy) {
            for (int dx = -1; dx <= 1; ++dx) {
                if (dx == 0 && dy == 0)
                    continue;
                discoverCell(x + dx, y + dy);
            }
        }
    }
    this->_score += 1;
}


void MinesweeperGame::updateMap(event_t evt)
{
    auto now = std::chrono::steady_clock::now();
    std::chrono::duration<float> elapsed = now - _lastUpdateTime;
    _lastUpdateTime = now;

    switch (evt.keyboard.key) {
        case KB_LOWER_W:
            this->reset();
            return;
    }

    if (this->_gameOver)
        return;

    int x = evt.mouse.position.x;
    int y = evt.mouse.position.y;
    auto it = std::find(this->_flags.begin(), this->_flags.end(), std::make_pair(x, y));

    switch (evt.mouse.button) {
        case MOUSE_LEFT:
            std::clog << "left " << evt.mouse.position.x << " : " << evt.mouse.position.y << "\n";
            if (it != this->_flags.end())
                break;
            if (this->_isFirstType) {
                this->_isFirstType = false;
                this->_isRevealed[x][y] = true;
                this->placeMines(x, y);
                this->calculateNumbers();
            }
            this->discoverCell(x, y);
            break;
        case MOUSE_RIGHT:
            std::clog << "right " << evt.mouse.position.x << " : " << evt.mouse.position.y << "\n";
            if (it == this->_flags.end()) {
                this->_flags.emplace_back(x, y);
                this->_tiles[y][x] = tile_t{
                    .image_path = {.ptr = nullptr, .len = 0},
                    .characters = {'F', 'F'},
                    .front = {255, 255, 255},
                    .back = {0, 0, 0}
                };
            } else {
                this->_flags.erase(it);
                this->_tiles[y][x] = tile_t{
                    .image_path = {.ptr = nullptr, .len = 0},
                    .characters = {'F', 'F'},
                    .front = {255, 255, 255},
                    .back = {0, 0, 0}
                };
            }
            break;
        default:
            break;
    }

    if (!_gameOver) {
        if (elapsed.count() >= 1.0f) {
            if (this->_countdownTime > 0) {
                this->_countdownTime--;
            }
        }
    }

    if (this->_countdownTime <= 0) {
        this->_gameOver = true;
        displayGameOver();
        return;
    }

    this->displayScore();
    this->displayCountdown();
}

uint64_t MinesweeperGame::getScore()
{
    return this->_score;
}

extern "C"
{
    extern const lib_info_t ARCADE_AACMMN_LIB_INFO = {.magic = LIB_MAGIC_VALUE,
        .api_version = LIB_API_VERSION,
        .type = "GAME",
        .name = {.ptr = "Minesweeper (AA)", .len = 16},
        .id = {.ptr = "eu.epitech.anicetaxel.minesweeper", .len = 33}};

    game_handle_t *game_getInstance(void)
    {
        try {
            return new game_handle_t{MinesweeperGame()};
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

    void game_destroyInstance(game_handle_t *hdl)
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

    tile_t **game_getMap(game_handle_t *hdl, uint64_t *length, uint64_t **lengths)
    {
        try {
            return hdl->inner.getMap(length, lengths);
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

    void game_reset(game_handle_t *hdl)
    {
        try {
            hdl->inner.reset();
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

    void game_updateMap(game_handle_t *hdl, event_t evt)
    {
        try {
            hdl->inner.updateMap(evt);
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

    uint64_t game_getScore(game_handle_t *hdl)
    {
        try {
            return hdl->inner.getScore();
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
