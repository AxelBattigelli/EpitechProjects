/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Game.cpp
*/

#include "Client/Scene/Wait.hpp"
#include "Client/Scene/Play.hpp"
#include "Client/Logic/Game.hpp"
#include "Shared/Exception.hpp"
#include "Shared/Logger.hpp"
#include "Shared/Utils.hpp"

#include <format>

Jetpack::Client::Game::Game(std::shared_ptr<Database> database) : _window(std::make_unique<sf::RenderWindow>(sf::VideoMode(1080, 720), "Jetpack")), _database(database), _currentScene("wait")
{
    if (_database->IsDebugEnabled()) {
        sf::err().rdbuf(nullptr);
    }
    _window->setFramerateLimit(60);
    _scenes.emplace("wait", std::make_unique<Wait>(_database));
    _scenes.emplace("play", std::make_unique<Play>(_database));
}

void Jetpack::Client::Game::Run()
{
    _scenes[_currentScene]->Load();
    while (_window->isOpen()) {
        while (!_database->IsEventQueueEmpty(Shared::Data::Category::FROM_SERVER)) {
            AnalyzeEvent(_database->PopEvent(Shared::Data::Category::FROM_SERVER));
        }

        std::string scene = _database->GetScene();
        sf::Event event {};

        if (scene != _currentScene) {
            if (scene != "quit") {
                ChangeScene(scene);
            } else {
                Quit();
            }
        }
        _scenes[_currentScene]->Update();
        while (_window->pollEvent(event)) {
            if (event.type != sf::Event::Closed) {
                _scenes[_currentScene]->AnalyzeEvent(event);
            } else {
                Quit();
            }
        }
        _window->clear();
        for (const auto& [name, sprite] : _database->GetSprites()) {
            _window->draw(*sprite);
        }
        for (const auto& [name, button] : _database->GetButtons()) {
            button->Render(*_window);
        }
        for (const auto& [name, text] : _database->GetTexts()) {
            _window->draw(*text);
        }
        _window->display();
    }
    _scenes[_currentScene]->Unload();
}

void Jetpack::Client::Game::UpdateGameFinish(const std::string& data)
{
    std::string id = _database->GetId() != Shared::Data::Id::FST_CLIENT ? "02" : "01";

    Shared::Logger::Info(std::format("Game finished. Player {} won.", data), _database->IsDebugEnabled());
    if (data != id) {
        _database->SetWaitText("You lost!");
    } else {
        _database->SetWaitText("You won!");
    }
    _database->SetScene("wait");
}

void Jetpack::Client::Game::UpdatePositions(const std::string& data)
{
    std::vector<std::string> coordinates = Shared::Utils::SplitStr(data);

    if (coordinates.size() != 4) {
        throw Shared::Exception("Invalid player position data");
    }
    try {
        sf::Vector2f me = {std::stof(coordinates[0]), std::stof(coordinates[1])};
        sf::Vector2f other = {std::stof(coordinates[2]), std::stof(coordinates[3])};

        _database->SetPositions(me, other);
        Shared::Logger::Info(std::format("Player position updated: ({}, {}), ({}, {})", me.x, me.y, other.x, other.y), _database->IsDebugEnabled());
    } catch (const std::exception& ex) {
        throw Shared::Exception("Invalid player position data");
    }
}

void Jetpack::Client::Game::UpdateCoins(const std::string& data)
{
    std::vector<std::string> parts = Shared::Utils::SplitStr(data);

    if (parts.size() != 4) {
        throw Shared::Exception("Invalid coin data");
    }
    try {
        sf::Vector2f position = {std::stof(parts[0]), std::stof(parts[1])};
        std::uint32_t me = std::stoul(parts[2]);
        std::uint32_t other = std::stoul(parts[3]);

        _database->CollectCoin(position, me, other);
        Shared::Logger::Info(std::format("Coins collected: ({}, {}), {} coins for me, {} coins for other", position.x, position.y, me, other), _database->IsDebugEnabled());
    } catch (const std::exception& ex) {
        throw Shared::Exception("Invalid coin data");
    }
}

void Jetpack::Client::Game::AnalyzeEvent(const Shared::Data::Event& event)
{
    switch (event.header) {
        case Shared::Data::Header::CON_ACCEPTED:
            _database->SetId(event.receiver);
            _database->SetWaitText("Waiting for another player...");
            _database->PushEvent(Shared::Data::Header::WANT_TO_PLAY);
            break;
        case Shared::Data::Header::CON_REJECTED: case Shared::Data::Header::CON_CLOSED:
            _database->SetScene("quit");
            break;
        case Shared::Data::Header::GAME_START:
            _database->SetScene("play");
            break;
        case Shared::Data::Header::GAME_FINISH:
            UpdateGameFinish(event.data);
            break;
        case Shared::Data::Header::MAP_CONTENT:
            _database->LoadMap(event.data);
            break;
        case Shared::Data::Header::COIN_COLLECTED:
            UpdateCoins(event.data);
            break;
        case Shared::Data::Header::PLAYER_POSITION:
            UpdatePositions(event.data);
            break;
        case Shared::Data::Header::INTERNAL_ERROR:
            Shared::Logger::Error(std::format("Internal error: {}", event.data), _database->IsDebugEnabled());
            break;
        default:
            throw Shared::Exception(std::format("Unknown or unhandled header: {}", std::to_string(event.header)));
    }
}

void Jetpack::Client::Game::ChangeScene(const std::string& scene)
{
    if (_scenes.find(scene) != _scenes.end()) {
        Shared::Logger::Info(std::format("Switching to scene: {}...", scene), _database->IsDebugEnabled());
        _scenes[_currentScene]->Unload();
        _currentScene = scene;
        _scenes[_currentScene]->Load();
    } else {
        Shared::Logger::Error(std::format("Failed to switch to non-existing scene: {}", scene), _database->IsDebugEnabled());
        _database->SetScene(_currentScene);
    }
}

void Jetpack::Client::Game::Quit()
{
    if (!_window) {
        throw Shared::Exception("Unable to quit: Window is not initialized");
    }
    if (!_window->isOpen()) {
        throw Shared::Exception("Unable to quit: Window is already closed");
    }
    Shared::Logger::Info("Quitting game...", _database->IsDebugEnabled());
    _database->PushEvent(Shared::Data::Header::CON_CLOSED);
    _window->close();
}
