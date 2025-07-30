/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Database.cpp
*/

#include "Client/Logic/Database.hpp"
#include "Shared/Exception.hpp"
#include "Shared/Logger.hpp"

#include <sstream>
#include <format>
#include <limits>
#include <cmath>

Jetpack::Client::Database::Database(bool isDebugEnabled) : _positions({{0.0, 0.0}, {0.0, 0.0}}), _waitText("Connection to the server..."), _scene("wait"), _id(Shared::Data::Id::NONE), _isDebugEnabled(isDebugEnabled)
{
    LoadFont("assets/fonts/jetpack.ttf");
}

std::shared_ptr<sf::Sprite>& Jetpack::Client::Database::CreateSprite(const std::string& name, const std::string& path)
{
    if (!DoesTextureExist(path)) {
        LoadTexture(path);
    }
    if (DoesSpriteExist(name)) {
        throw Jetpack::Shared::Exception(std::format("Sprite \"{}\" already exists.", name));
    }
    _sprites.emplace(name, std::make_shared<sf::Sprite>(*_textures[path]));
    return _sprites[name];
}

void Jetpack::Client::Database::DestroySprite(const std::string& name)
{
    if (!DoesSpriteExist(name)) {
        throw Jetpack::Shared::Exception(std::format("Sprite \"{}\" does not exist.", name));
    }
    _sprites.erase(name);
}

std::map<std::string, std::shared_ptr<sf::Sprite>>& Jetpack::Client::Database::GetSprites()
{
    return _sprites;
}

std::shared_ptr<sf::Sprite>& Jetpack::Client::Database::GetSprite(const std::string& name)
{
    if (DoesSpriteExist(name)) {
        return _sprites[name];
    }
    throw Jetpack::Shared::Exception(std::format("Sprite \"{}\" does not exist.", name));
}

std::shared_ptr<Jetpack::Client::Button>& Jetpack::Client::Database::CreateButton(const std::string& name, const std::string& str, const sf::Vector2f& position, const std::size_t size)
{
    if (DoesButtonExist(name)) {
        throw Jetpack::Shared::Exception(std::format("Button \"{}\" already exists.", name));
    }
    _buttons.emplace(name, std::make_shared<Jetpack::Client::Button>(_font, str, position, size));
    return _buttons[name];
}

void Jetpack::Client::Database::DestroyButton(const std::string& name)
{
    if (!DoesButtonExist(name)) {
        throw Jetpack::Shared::Exception(std::format("Button \"{}\" does not exist.", name));
    }
    _buttons.erase(name);
}

std::map<std::string, std::shared_ptr<Jetpack::Client::Button>>& Jetpack::Client::Database::GetButtons()
{
    return _buttons;
}

std::shared_ptr<Jetpack::Client::Button>& Jetpack::Client::Database::GetButton(const std::string& name)
{
    if (DoesButtonExist(name)) {
        return _buttons[name];
    }
    throw Jetpack::Shared::Exception(std::format("Button \"{}\" does not exist.", name));
}

std::shared_ptr<sf::Text>& Jetpack::Client::Database::CreateText(const std::string& name, const std::string& str, const sf::Vector2f& position, const std::size_t size)
{
    if (DoesTextExist(name)) {
        throw Jetpack::Shared::Exception(std::format("Text \"{}\" already exists.", name));
    }
    _texts.emplace(name, std::make_shared<sf::Text>(str, *_font, size));
    _texts[name]->setOrigin(_texts[name]->getGlobalBounds().width / 2, _texts[name]->getGlobalBounds().height / 2);
    _texts[name]->setOutlineColor(sf::Color::Black);
    _texts[name]->setOutlineThickness(2);

    sf::Vector2u windowSize = sf::Vector2u(1080, 720);
    float xPos = (position.x < 0) ? windowSize.x / 2 : position.x;
    float yPos = (position.y < 0) ? windowSize.y / 2 : position.y;

    _texts[name]->setPosition(xPos, yPos);

    return _texts[name];
}

void Jetpack::Client::Database::DestroyText(const std::string& name)
{
    if (!DoesTextExist(name)) {
        throw Jetpack::Shared::Exception(std::format("Text \"{}\" does not exist.", name));
    }
    _texts.erase(name);
}

std::map<std::string, std::shared_ptr<sf::Text>>& Jetpack::Client::Database::GetTexts()
{
    return _texts;
}

std::shared_ptr<sf::Text>& Jetpack::Client::Database::GetText(const std::string& name)
{
    if (DoesTextExist(name)) {
        return _texts[name];
    }
    throw Jetpack::Shared::Exception(std::format("Text \"{}\" does not exist.", name));
}

bool Jetpack::Client::Database::IsDebugEnabled() const
{
    return _isDebugEnabled;
}

void Jetpack::Client::Database::PushEvent(const Shared::Data::Header header, const std::string& data)
{
    Jetpack::Shared::Data::Event event = {
        .sender = _id,
        .receiver = Shared::Data::Id::SERVER,
        .header = header,
        .data = data
    };

    _eventsToServer.push(event);
}

void Jetpack::Client::Database::PushEvent(const Shared::Data::Event& event)
{
    if (_id != -1 && event.receiver != _id) {
        throw Jetpack::Shared::Exception(std::format("Invalid receiver: got {}, expected {}", std::to_string(event.sender), std::to_string(_id)));
    }
    _eventsFromServer.push(event);
}

Jetpack::Shared::Data::Event Jetpack::Client::Database::PopEvent(Shared::Data::Category category)
{
    switch (category) {
        case Shared::Data::Category::FROM_SERVER: {
            if (_eventsFromServer.empty()) {
                throw Jetpack::Shared::Exception("No events in the queue.");
            }
            auto event = _eventsFromServer.front();

            _eventsFromServer.pop();
            return event;
        }
        case Shared::Data::Category::TO_SERVER: {
            if (_eventsToServer.empty()) {
                throw Jetpack::Shared::Exception("No events in the queue.");
            }
            auto event = _eventsToServer.front();

            _eventsToServer.pop();
            return event;
        }
        default:
            throw Jetpack::Shared::Exception("Invalid event category.");
    }
}

bool Jetpack::Client::Database::IsEventQueueEmpty(Shared::Data::Category category) const
{
    switch (category) {
        case Shared::Data::Category::FROM_SERVER:
            return _eventsFromServer.empty();
        case Shared::Data::Category::TO_SERVER:
            return _eventsToServer.empty();
        default:
            throw Jetpack::Shared::Exception("Invalid event category.");
    }
}

void Jetpack::Client::Database::ClearEventQueue(Shared::Data::Category category)
{
    switch (category) {
        case Shared::Data::Category::FROM_SERVER:
            while (!_eventsFromServer.empty()) {
                _eventsToServer.pop();
            }
            break;
        case Shared::Data::Category::TO_SERVER:
            while (!_eventsToServer.empty()) {
                _eventsToServer.pop();
            }
            break;
        default:
            throw Jetpack::Shared::Exception("Invalid event category.");
    }
}

void Jetpack::Client::Database::SetId(Shared::Data::Id id)
{
    Shared::Logger::Info(std::format("Setting client id to {}.", std::to_string(id)), _isDebugEnabled);
    _id = id;
}

Jetpack::Shared::Data::Id Jetpack::Client::Database::GetId() const
{
    return _id;
}

void Jetpack::Client::Database::LoadMap(const std::string& raw)
{
    std::istringstream stream(raw);
    std::string line {};

    for (const auto& c : raw) {
        if (c != 'e' && c != '_' && c != 'c' && c != '\n') {
            throw Jetpack::Shared::Exception("Invalid character in map data.");
        }
    }
    while (std::getline(stream, line)) {
        _map.push_back(line);
    }
    if (_map.empty()) {
        throw Jetpack::Shared::Exception("Empty map data.");
    }
    for (const auto& line : _map) {
        if (line.size() != _map[0].size()) {
            throw Jetpack::Shared::Exception("Invalid map data: inconsistent line lengths.");
        }
    }
}

const std::vector<std::string>& Jetpack::Client::Database::GetMap() const
{
    return _map;
}

void Jetpack::Client::Database::SetScene(const std::string& scene)
{
    _scene = scene;
}

const std::string& Jetpack::Client::Database::GetScene() const
{
    return _scene;
}

void Jetpack::Client::Database::SetWaitText(const std::string& text)
{
    _waitText = text;
}

const std::string& Jetpack::Client::Database::GetWaitText() const
{
    return _waitText;
}

void Jetpack::Client::Database::SetPositions(const sf::Vector2f& me, const sf::Vector2f& other)
{
    _positions[0] = me;
    _positions[1] = other;
}

const std::array<sf::Vector2f, 2>& Jetpack::Client::Database::GetPositions() const
{
    return _positions;
}

void Jetpack::Client::Database::CollectCoin(const sf::Vector2f& position, std::uint32_t me, std::uint32_t other)
{
    std::string id = std::format("level-1-play-coin-at-{}-{}", position.x, position.y);

    if (DoesTextExist("level-1-play-coins")) {
        _texts["level-1-play-coins"]->setString(std::format("Player 1: {}\nPlayer 2: {}", me, other));
    }
    if (DoesSpriteExist(id)) {
        DestroySprite(id);
    }
}

bool Jetpack::Client::Database::DoesSpriteExist(const std::string& name) const
{
    return _sprites.find(name) != _sprites.end();
}

bool Jetpack::Client::Database::DoesButtonExist(const std::string& name) const
{
    return _buttons.find(name) != _buttons.end();
}

bool Jetpack::Client::Database::DoesTextureExist(const std::string& name) const
{
    return _textures.find(name) != _textures.end();
}

void Jetpack::Client::Database::LoadTexture(const std::string& name)
{
    auto texture = std::make_unique<sf::Texture>();

    if (!texture->loadFromFile(name)) {
        throw Jetpack::Shared::Exception(std::format("Failed to load texture: \"{}\".", name));
    }
    _textures.emplace(name, std::move(texture));
}

void Jetpack::Client::Database::LoadFont(const std::string& name)
{
    sf::Font font = sf::Font();

    if (!font.loadFromFile(name)) {
        throw Jetpack::Shared::Exception(std::format("Failed to load font: \"{}\".", name));
    }
    _font = std::make_shared<sf::Font>(font);
}

bool Jetpack::Client::Database::DoesTextExist(const std::string& name) const
{
    return _texts.find(name) != _texts.end();
}
