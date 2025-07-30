/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Button.hpp
*/

#pragma once

#include "Client/Object/IObject.hpp"

namespace Jetpack::Client
{
    /**
     * @class Button
     * @brief Button class to create a button in the game
     */
    class Button : public IObject
    {
        public:
            /**
             * @brief Create a new button
             *
             * @param font The font to be used for the text.
             * @param str The text to be displayed on the button.
             * @param position The position of the button on the screen.
             * @param size The size of the button.
             */
            Button(const std::shared_ptr<sf::Font> font, const std::string& str, const sf::Vector2f& position, const std::size_t size);

            /**
             * @brief Draw the object on the screen
             *
             * @param window The window to draw the object on
             */
            void Render(sf::RenderWindow& window) const override;

            /**
             * @brief Update the object depending of the mouse position
             *
             * @param x The x coordinate of the mouse position
             * @param y The y coordinate of the mouse position
             * @return Wether the position is within the bounds of the object
             */
            bool Update(const std::int32_t x, const std::int32_t y) override;

        private:
            std::shared_ptr<sf::Text> _text; /*!< The text to be displayed on the button */
    };
}
