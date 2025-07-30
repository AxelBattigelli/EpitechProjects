/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** IGame.hpp
*/

#pragma once

#include "Shared/Data.hpp"

namespace Arcade
{
    namespace Game
    {
        /**
         * @class IGame
         * @brief Interface for game libraries
         */
        class IGame {
          public:
            /**
             * @brief Destroy the IGame object
             */
            virtual ~IGame() = default;

            /**
             * @brief Get the informations from the game by getting the map
             *
             * @return The map of the game
             */
            virtual const Shared::Data::Map &GetMap() const = 0;

            /**
             * @brief Update the informations from the game by modifying the map
             *
             * @param event The event to update the game
             */
            virtual void UpdateMap(const Shared::Data::Event event) = 0;

            /**
             * @brief Get the game back to its original state
             */
            virtual void Reset() = 0;

            /**
             * @brief Get the score from the player in the game
             *
             * @return The score from the player in the game
             */
            virtual std::size_t GetScore() const = 0;
        };
    } // namespace Game
} // namespace Arcade
