/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Wait.hpp
*/

#pragma once

#include "Client/Scene/AScene.hpp"

namespace Jetpack::Client
{
    /**
     * @class Wait
     * @brief Wait scene class
     */
    class Wait : public AScene
    {
        public:
            /**
             * @brief Create new wait scene object
             *
             * @param database The database to use
             */
            Wait(std::shared_ptr<Database> database);

            /**
             * @brief Load the scene
             */
            void Load() override;

            /**
             * @brief Unload the scene
             */
            void Unload() override;

            /**
             * @brief Analyze events for the scene
             *
             * @param event The event to analyze
             */
            void AnalyzeEvent(const sf::Event& event) override;
    };
}
