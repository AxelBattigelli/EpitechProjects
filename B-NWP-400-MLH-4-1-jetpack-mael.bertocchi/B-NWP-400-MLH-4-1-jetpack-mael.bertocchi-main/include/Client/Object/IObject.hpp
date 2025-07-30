/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Button.hpp
*/

#pragma once

#include <SFML/Graphics.hpp>
#include <cstdint>
#include <memory>

namespace Jetpack::Client
{
    /**
     * @class IObject
     * @brief Interface for all objects in the game.
     */
    class IObject
    {
        public:
            /**
             * @brief Destroy the object
             */
            virtual ~IObject() = default;

            /**
             * @brief Draw the object on the screen
             *
             * @param window The window to draw the object on
             */
            virtual void Render(sf::RenderWindow& window) const = 0;

            /**
             * @brief Update the object depending of the mouse position
             *
             * @param x The x coordinate of the mouse position
             * @param y The y coordinate of the mouse position
             * @return Wether the position is within the bounds of the object
             */
            virtual bool Update(const std::int32_t x, const std::int32_t y) = 0;
    };
}
