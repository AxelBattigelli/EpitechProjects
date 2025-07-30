/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Play.cpp
*/

#include "Client/Scene/Play.hpp"
#include "Shared/Logger.hpp"

constexpr float TILE_HEIGHT = 72; /*!< The height of the tile in pixels */
constexpr float TILE_WIDTH = 45; /*!< The width of the tile in pixels */

Jetpack::Client::Play::Play(std::shared_ptr<Database> database) : AScene(database), _lastPositions({{0.0, 0.0}, {0.0, 0.0}}), _width(0), _isJumping(false)
{
    Shared::Logger::Info("Play scene created", _database->IsDebugEnabled());
}

void Jetpack::Client::Play::Load()
{
    float playercale = std::min(TILE_WIDTH / 121.0f, TILE_HEIGHT / 138.0f);
    const std::vector<std::string>& map = _database->GetMap();

    _width = map[0].size() * TILE_WIDTH;

    CreateMapElements(map);

    _players[0] = _database->CreateSprite("level-2-play-player-2", "assets/images/player.png");
    _players[0]->setScale({playercale, playercale});

    _players[1] = _database->CreateSprite("level-2-play-player-1", "assets/images/player.png");
    _players[1]->setScale({playercale, playercale});
    _players[1]->setColor(sf::Color(160, 160, 160, 120));

    _database->CreateText("level-1-play-coins", "Player 1: 0\nPlayer 2: 0", {80, 40}, 30);

    _backgrounds[0] = _database->CreateSprite("level-0-play-background-1", "assets/images/play-background.jpeg");
    _backgrounds[0]->setScale({1.1f, 1.1f});

    _backgrounds[1] = _database->CreateSprite("level-0-play-background-2", "assets/images/play-background.jpeg");
    _backgrounds[1]->setScale({1.1f, 1.1f});
    _backgrounds[1]->setPosition(_backgrounds[0]->getPosition().x + _backgrounds[0]->getGlobalBounds().width, _backgrounds[0]->getPosition().y);

    Shared::Logger::Info("Play scene loaded", _database->IsDebugEnabled());
}

void Jetpack::Client::Play::Unload()
{
    _database->DestroySprite("level-0-play-background-1");
    _database->DestroySprite("level-0-play-background-2");
    _database->DestroySprite("level-2-play-player-1");
    _database->DestroySprite("level-2-play-player-2");
    _database->DestroyText("level-1-play-coins");

    for (const auto& elementId : _mapElements) {
        try {
            _database->DestroySprite(elementId);
        } catch (...) {
            continue;
        }
    }
    _mapElements.clear();

    Shared::Logger::Info("Play scene unloaded", _database->IsDebugEnabled());
}

void Jetpack::Client::Play::Update()
{
    static float targetOffset = 0.0f;
    static float currentOffset = 0.0f;
    float rawSpeed = MovePlayer();

    targetOffset += rawSpeed;

    float deltaOffset = targetOffset - currentOffset;
    float smoothSpeed = deltaOffset * 0.1f;

    currentOffset += smoothSpeed;
    for (auto& background : _backgrounds) {
        background->move({smoothSpeed, 0});
        if (background->getPosition().x + background->getGlobalBounds().width <= 0) {
            background->setPosition(background->getPosition().x + background->getGlobalBounds().width - 5, background->getPosition().y);
        }
    }
    for (const auto& elementId : _mapElements) {
        try {
            auto element = _database->GetSprite(elementId);
            element->move({smoothSpeed, 0});
        } catch (...) {
            _mapElements.erase(std::remove(_mapElements.begin(), _mapElements.end(), elementId), _mapElements.end());
        }
    }
}

void Jetpack::Client::Play::AnalyzeEvent(const sf::Event& event)
{
    if (event.type == sf::Event::KeyPressed) {
        if (event.key.code == sf::Keyboard::Space && !_isJumping) {
            _database->PushEvent(Shared::Data::Header::PLAYER_ACTION, "\x01");
            _isJumping = true;
        } else if (event.key.code == sf::Keyboard::Escape) {
            _database->PushEvent(Shared::Data::Header::WANT_TO_QUIT);
            _database->SetScene("quit");
        }
    } else if (event.type == sf::Event::KeyReleased) {
        if (event.key.code == sf::Keyboard::Space) {
            _database->PushEvent(Shared::Data::Header::PLAYER_ACTION, "\x00");
            _isJumping = false;
        }
    }
}

void Jetpack::Client::Play::CreateMapElements(const std::vector<std::string>& map)
{
    for (size_t y = 0; y < map.size(); y++) {
        for (size_t x = 0; x < map[y].size(); x++) {
            char elementType = map[y][x];

            if (elementType == '_') {
                continue;
            }

            std::shared_ptr<sf::Sprite> sprite = nullptr;
            std::string id {};

            if (elementType == 'e') {
                float zapperScale = std::min(TILE_WIDTH / 104.0f, TILE_HEIGHT / 122.0f);

                id = "level-1-play-zapper-at-" + std::to_string(x) + "-" + std::to_string(y);
                sprite = _database->CreateSprite(id, "assets/images/zapper.png");
                sprite->setScale(zapperScale, zapperScale);
            } else if (elementType == 'c') {
                float coinScale = std::min(TILE_WIDTH / 199.0f, TILE_HEIGHT / 171.0f);

                id = "level-1-play-coin-at-" + std::to_string(x) + "-" + std::to_string(y);
                sprite = _database->CreateSprite(id, "assets/images/coin.png");
                sprite->setScale(coinScale, coinScale);
            }
            if (sprite) {
                sf::FloatRect bounds = sprite->getLocalBounds();
                float posX = x * TILE_WIDTH + (TILE_WIDTH - (bounds.width * sprite->getScale().x)) / 2.0f;
                float posY = y * TILE_HEIGHT + (TILE_HEIGHT - (bounds.height * sprite->getScale().y)) / 2.0f;

                sprite->setPosition(posX, posY);
                _mapElements.push_back(id);
            }
        }
    }
}

float Jetpack::Client::Play::MovePlayer()
{
    const std::array<sf::Vector2f, 2>& positions = _database->GetPositions();
    std::array<sf::Vector2f, 2> deltas {};
    const float playerFixedPosX = 200.0f;
    static float playerWorldPosX = 0.0f;
    float speed = 0.0f;

    for (std::uint8_t i = 0; i < 2; i++) {
        deltas[i] = positions[i] - _lastPositions[i];
        _lastPositions[i] = positions[i];
    }

    playerWorldPosX += deltas[0].x;

    if (playerWorldPosX < playerFixedPosX || playerWorldPosX > _width - (1080.0f - playerFixedPosX)) {
        for (std::uint8_t i = 0; i < 2; i++) {
            _players[i]->setPosition(_players[i]->getPosition().x + deltas[i].x, positions[i].y);
        }
    } else {
        speed = -deltas[0].x;
        for (std::uint8_t i = 0; i < 2; i++) {
            _players[i]->setPosition(playerFixedPosX, positions[i].y);
        }
    }
    return speed;
}
