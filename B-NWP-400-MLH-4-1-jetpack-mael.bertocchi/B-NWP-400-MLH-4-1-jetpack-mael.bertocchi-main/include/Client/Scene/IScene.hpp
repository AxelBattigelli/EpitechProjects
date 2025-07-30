/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** IScene.hpp
*/

#pragma once

#include <SFML/Graphics.hpp>
#include <memory>

namespace Jetpack::Client
{
    /**
     * @class IScene
     * @brief Interface for all scenes
     */
    class IScene
    {
        public:
            /**
             * @brief Default destructor
             */
            virtual ~IScene() = default;

            /**
             * @brief Load the scene
             */
            virtual void Load() = 0;

            /**
             * @brief Unload the scene
             */
            virtual void Unload() = 0;

            /**
             * @brief Update the scene
             */
            virtual void Update() = 0;

            /**
             * @brief Analyze events for the scene
             *
             * @param event The event to analyze
             */
            virtual void AnalyzeEvent(const sf::Event& event) = 0;
    };
}
