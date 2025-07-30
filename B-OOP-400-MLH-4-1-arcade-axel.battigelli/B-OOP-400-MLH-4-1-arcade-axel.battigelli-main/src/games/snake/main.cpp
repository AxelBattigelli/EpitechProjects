#include "main.hpp"
#include "Shared/Exception.hpp"
#include "ffi.hpp"
#include <iostream>

SnakeGame::SnakeGame()
{
    std::clog << "Initing Snake\n";
}

SnakeGame::~SnakeGame()
{
    std::clog << "Deiniting Snake\n";
}

void SnakeGame::updateTiles()
{
    this->_lengths.clear();
    this->_tilesLines.clear();

    for (auto &line : this->_tiles) {
        this->_lengths.push_back(line.size());
        this->_tilesLines.push_back(line.data());
    }
}

tile_t **SnakeGame::getMap(uint64_t *length, uint64_t **lengths)
{
    this->updateTiles();
    *length = this->_tilesLines.size();
    *lengths = this->_lengths.data();
    return this->_tilesLines.data();
}

void SnakeGame::placeFood()
{
    bool ok = false;

    while (!ok) {
        int xRand = rand() % 24 + 3;
        int yRand = rand() % 24 + 3;

        ok = true;
        for (const auto &segment : this->_snake) {
            if (segment.first == xRand && segment.second == yRand) {
                ok = false;
                break;
            }
        }
        if (ok) {
            this->_food = {xRand, yRand};
            const tile_t food = {
                .image_path = {.ptr = "assets/snake/food.png", .len = 21},
                .characters = {'P', 'P'},
                .front = {255, 255, 255},
                .back = {0, 0, 0},
            };
            this->_tiles[this->_food.second][this->_food.first] = food;
        }
    }
}

void SnakeGame::reset()
{
    using std::literals::chrono_literals::operator""s;

    this->_score = 0;
    this->_snake.clear();
    int startX = 13;
    int startY = 13;

    this->_snake.push_back({startX, startY});
    this->_snake.push_back({startX - 1, startY});
    this->_snake.push_back({startX - 2, startY});
    this->_snake.push_back({startX - 3, startY});

    this->_snakeVelocity = {1, 0};

    this->_tiles.clear();
    this->_tiles.resize(30,
        std::vector<tile_t>(30,
            tile_t{
                .image_path = {.ptr = "assets/snake/bg.png", .len = 19},
                .characters = {' ', ' '},
                .front = {255, 255, 255},
                .back = {0, 0, 0},
            }));
    const tile_t wall = {
        .image_path = {.ptr = "assets/snake/tile_red.png", .len = 25},
        .characters = {'#', '#'},
        .front = {255, 255, 255},
        .back = {0, 0, 0},
    };
    for (int i = 2; i < 28; ++i) {
        this->_tiles[i][2] = wall;
        this->_tiles[i][27] = wall;
        this->_tiles[2][i] = wall;
        this->_tiles[27][i] = wall;
    }
    this->placeFood();
    this->_nextUpdate = std::chrono::steady_clock::now() + 1s;
}

void SnakeGame::displayGameOver()
{
    const std::string message = " GAME  OVER ";
    int centerY = this->_tiles.size() / 2;
    int centerX = (this->_tiles[0].size() - message.size()) / 2;

    for (const auto &segment : this->_snake) {
        this->_tiles[segment.second][segment.first] =
            tile_t{.image_path = {.ptr = "assets/snake/bg.png", .len = 19},
                .characters = {' ', ' '},
                .front = {255, 255, 255},
                .back = {0, 0, 0}};
    }
    for (size_t i = 0; i < message.size(); ++i) {
        this->_tiles[centerY][centerX + i] = tile_t{.image_path = {.ptr = nullptr, .len = 0},
            .characters = {message[i], ' '},
            .front = {255, 0, 0},
            .back = {200, 200, 200}};
    }
    this->_snakeVelocity = {0, 0};
    this->_snake.push_back({13, 13});
    this->_snake.clear();
}

void SnakeGame::displayScore()
{
    const std::string message = "Score: " + std::to_string(this->_score);

    for (size_t i = 0, tileX = 0; i < message.size() && tileX < this->_tiles[29].size(); ++tileX) {
        char first = message[i];
        char second = (i + 1 < message.size()) ? message[i + 1] : ' ';

        this->_tiles[29][tileX] = tile_t{
            .image_path = {.ptr = nullptr, .len = 0},
            .characters = {first, second},
            .front = {255, 255, 255},
            .back = {0, 0, 0}
        };
        i += 2;
    }
}


void SnakeGame::updateMap(event_t evt)
{
    using std::literals::chrono_literals::operator""ms;

    switch (evt.keyboard.key) {
        case KB_UP:
            if (this->_snakeVelocity.second == 0)
                this->_snakeVelocity = {0, -1};
            break;
        case KB_DOWN:
            if (this->_snakeVelocity.second == 0)
                this->_snakeVelocity = {0, 1};
            break;
        case KB_LEFT:
            if (this->_snakeVelocity.first == 0)
                this->_snakeVelocity = {-1, 0};
            break;
        case KB_RIGHT:
            if (this->_snakeVelocity.first == 0)
                this->_snakeVelocity = {1, 0};
            break;
        case KB_LOWER_W:
            this->reset();
            return;
    }
    if (this->_snake.empty())
        return;
    if (this->_nextUpdate > std::chrono::steady_clock::now())
        return;
    this->_nextUpdate += 200ms;
    auto newPos = this->_snake.front();
    newPos.first += this->_snakeVelocity.first;
    newPos.second += this->_snakeVelocity.second;
    if (newPos.first <= 2 || newPos.first >= 27 || newPos.second <= 2 || newPos.second >= 27) {
        newPos = this->_snake.front();
    }

    for (auto it = std::next(this->_snake.begin()); it != this->_snake.end(); ++it) {
        if (*it == newPos) {
            this->displayGameOver();
            return;
        }
    }

    this->_snake.push_front(newPos);
    if (this->_food.first != newPos.first || this->_food.second != newPos.second) {
        auto oldPos = this->_snake.back();
        this->_tiles[oldPos.second][oldPos.first] =
            tile_t{.image_path = {.ptr = "assets/snake/bg.png", .len = 19},
                .characters = {' ', ' '},
                .front = {255, 255, 255},
                .back = {0, 0, 0}};
        this->_snake.pop_back();
    } else {
        this->_score += 10;
        this->placeFood();
    }
    std::array<char, 2> headChar = {'8', '>'};
    char *headImage = "assets/snake/head_right.png";
    if (this->_snakeVelocity == std::pair<int, int>{-1, 0}) {
        headChar = {'<', '8'};
        headImage = "assets/snake/head_left_.png";
    } else if (this->_snakeVelocity == std::pair<int, int>{0, -1}) {
        headChar = {'/', '\\'};
        headImage = "assets/snake/head_up___.png";
    } else if (this->_snakeVelocity == std::pair<int, int>{0, 1}) {
        headChar = {'\\', '/'};
        headImage = "assets/snake/head_down_.png";
    }

    this->_tiles[newPos.second][newPos.first] = tile_t{.image_path = {.ptr = headImage, .len = 27},
        .characters = {headChar[0], headChar[1]},
        .front = {0, 255, 0},
        .back = {0, 0, 0}};
    bool isFirst = true;
    for (const auto &segment : this->_snake) {
        if (isFirst) {
            isFirst = false;
            continue;
        }
        this->_tiles[segment.second][segment.first] =
            tile_t{.image_path = {.ptr = "assets/snake/body.png", .len = 21},
                .characters = {'(', ')'},
                .front = {0, 255, 0},
                .back = {0, 0, 0}};
    }
    this->displayScore();
}

uint64_t SnakeGame::getScore()
{
    return this->_score;
}

extern "C"
{
    extern const lib_info_t ARCADE_AACMMN_LIB_INFO = {.magic = LIB_MAGIC_VALUE,
        .api_version = LIB_API_VERSION,
        .type = "GAME",
        .name = {.ptr = "Snake (AA)", .len = 10},
        .id = {.ptr = "eu.epitech.anicetaxel.snake", .len = 27}};

    game_handle_t *game_getInstance(void)
    {
        try {
            return new game_handle_t{SnakeGame()};
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
