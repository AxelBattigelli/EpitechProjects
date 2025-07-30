/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Game.hpp
*/

#pragma once

#include "Client/Logic/Database.hpp"
#include "Client/Scene/IScene.hpp"

#include <SFML/Graphics.hpp>
#include <unordered_map>
#include <cstdint>
#include <memory>

namespace Jetpack::Client
{
    /**
     * @class Game
     * @brief The Game class represents the main game loop and window.
     * It handles the rendering and game logic.
     */
    class Game
    {
        public:
            /**
             * @brief Create a new Game instance.
             *
             * @param database A shared pointer to the database instance.
             */
            Game(std::shared_ptr<Database> database);

            /**
             * @brief Run the game loop.
             */
            void Run();

        private:
            /**
             * @brief Analyze the event and push it into the queue
             *
             * @param event The event to analyze
             */
            void AnalyzeEvent(const Shared::Data::Event& event);

            /**
             * @brief Update the game start in the database
             *
             * @param data The data containing the game start information
             */
            void UpdateGameFinish(const std::string& data);

            /**
             * @brief Update the player position in the database
             *
             * @param data The data containing the player position
             */
            void UpdatePositions(const std::string& data);

            /**
             * @brief Update the player coins in the database
             *
             * @param data The data containing the player coins
             */
            void UpdateCoins(const std::string& data);

            /**
             * @brief Change the current scene
             *
             * @param scene The name of the new scene
             */
            void ChangeScene(const std::string& scene);

            /**
             * @brief Quit the game
             */
            void Quit();

            std::unordered_map<std::string, std::unique_ptr<IScene>> _scenes; /*!< A map of scenes in the game */
            std::unique_ptr<sf::RenderWindow> _window; /*!< A pointer to the window */
            std::shared_ptr<Database> _database; /*!< A pointer to the database */
            std::string _currentScene; /*!< The name of the current scene */
    };
}
