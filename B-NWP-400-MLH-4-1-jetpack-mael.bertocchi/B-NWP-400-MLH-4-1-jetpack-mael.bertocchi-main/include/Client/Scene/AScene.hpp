/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** AScene.hpp
*/

#pragma once

#include "Client/Logic/Database.hpp"
#include "Client/Scene/IScene.hpp"

namespace Jetpack::Client
{
    /**
     * @class AScene
     * @brief Abstract class for all scenes
     */
    class AScene : public IScene
    {
        public:
            /**
             * @brief Create new scene object
             *
             * @param database The database to use
             */
            AScene(std::shared_ptr<Database> database);

            /**
             * @brief Update the scene
             */
            virtual void Update() override;

        protected:
            std::shared_ptr<Database> _database;  /*!< Database to use */
    };
}
