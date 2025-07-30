/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Button.cpp
*/

#include "Client/Object/Button.hpp"
#include "Shared/Exception.hpp"

#include <format>

Jetpack::Client::Button::Button(const std::shared_ptr<sf::Font> font, const std::string& str, const sf::Vector2f& position, const std::size_t size)
{
    _text = std::make_shared<sf::Text>(str, *font, size);

    if (!_text) {
        throw Shared::Exception(std::format("Failed to create text (\"{}\")", str));
    }
    sf::FloatRect bounds = _text->getGlobalBounds();

    _text->setOrigin(bounds.width / 2, bounds.height / 2);
    _text->setPosition({ position.x - bounds.left, position.y - bounds.top });
    _text->setOutlineColor(sf::Color::Black);
    _text->setFillColor(sf::Color::White);
    _text->setOutlineThickness(1);
}

void Jetpack::Client::Button::Render(sf::RenderWindow& window) const
{
    if (_text) {
        window.draw(*_text);
    }
}

bool Jetpack::Client::Button::Update(const std::int32_t x, const std::int32_t y)
{
    if (!_text) {
        return false;
    }
    bool isHovered = _text->getGlobalBounds().contains({ static_cast<float>(x), static_cast<float>(y) });

    if (isHovered) {
        _text->setFillColor(sf::Color(250, 220, 95));
    } else {
        _text->setFillColor(sf::Color::White);
    }
    return isHovered;
}
