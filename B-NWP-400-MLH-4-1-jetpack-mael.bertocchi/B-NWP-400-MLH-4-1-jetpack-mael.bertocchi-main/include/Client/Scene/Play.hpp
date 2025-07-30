/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Play.hpp
*/

#pragma once

#include "Client/Scene/AScene.hpp"

namespace Jetpack::Client
{
    /**
     * @class Play
     * @brief Play scene class
     */
    class Play : public AScene
    {
        public:
            /**
            * @brief Create new play scene object
            *
            * @param database The database to use
            */
            Play(std::shared_ptr<Database> database);

            /**
             * @brief Load the scene
             */
            void Load() override;

            /**
             * @brief Unload the scene
             */
            void Unload() override;

            /**
             * @brief Update the scene
             */
            void Update() override;

            /**
             * @brief Analyze events for the scene
             *
             * @param event The event to analyze
             */
            void AnalyzeEvent(const sf::Event& event) override;

        private:
            /**
             * @brief Create map elements
             *
             * @param mapData The map data to use
             */
            void CreateMapElements(const std::vector<std::string>& mapData);

            /**
             * @brief Move the player
             *
             * @return The speed to move the player
             */
            float MovePlayer();

            std::array<std::shared_ptr<sf::Sprite>, 2> _backgrounds; /*!< Background sprites */
            std::array<std::shared_ptr<sf::Sprite>, 2> _players; /*!< Player sprites */
            std::array<sf::Vector2f, 2> _lastPositions; /*!< Last player position */
            std::vector<std::string> _mapElements; /*!< Map elements */
            std::size_t _width; /*!< Width of the map */
            bool _isJumping; /*!< Is the player jumping */
    };
}
