/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** IGraphical.hpp
*/

#pragma once

#include "Shared/Data.hpp"

namespace Arcade::Graphical
{
    /**
     * @class IGraphical
     * @brief Interface for graphical libraries
     */
    class IGraphical {
      public:
        /**
         * @brief Destroy the IGraphical object
         */
        virtual ~IGraphical() = default;

        /**
         * @brief Load the graphical library
         *
         * This function must be called when the window is created
         */
        virtual void Load() = 0;

        /**
         * @brief Unload the graphical library
         *
         * This function must be called when the window is destroyed
         */
        virtual void Unload() = 0;

        /**
         * @brief Draw the map on the screen
         *
         * @param map The map to render
         */
        virtual void RenderMap(const Shared::Data::Map &map) = 0;

        /**
         * @brief Get the event from the user
         *
         * @return The event from the user
         */
        [[nodiscard]] virtual Shared::Data::Event GetEvent() const = 0;
    };
} // namespace Arcade::Graphical
