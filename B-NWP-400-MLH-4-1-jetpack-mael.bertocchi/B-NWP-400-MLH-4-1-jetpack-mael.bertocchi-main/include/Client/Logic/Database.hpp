/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Database.hpp
*/

#pragma once

#include "Client/Object/Button.hpp"
#include "Shared/Data.hpp"

#include <unordered_map>
#include <memory>
#include <array>
#include <queue>
#include <map>

namespace Jetpack::Client
{
    /**
     * @class Database
     * @brief Database class to manage all the data we used in the game.
     */
    class Database
    {
        public:
            /**
             * @brief Create a new database
             *
             * @param isDebugEnable Whether the debug mode is enabled or not
             */
            Database(bool isDebugEnable);

            /**
             * @brief Create a sprite
             *
             * @param name The name of the sprite
             * @param path The path of the sprite
             * @return A unique pointer to the sprite
             */
            std::shared_ptr<sf::Sprite>& CreateSprite(const std::string& name, const std::string& path);

            /**
             * @brief Destroy a sprite
             *
             * @param name The name of the sprite
             */
            void DestroySprite(const std::string& name);

            /**
             * @brief Get all the sprites
             *
             * @return A map of all the sprites
             */
            std::map<std::string, std::shared_ptr<sf::Sprite>>& GetSprites();

            /**
             * @brief Get a sprite
             *
             * @param name The name of the sprite
             * @return A shared pointer to the sprite
             */
            std::shared_ptr<sf::Sprite>& GetSprite(const std::string& name);

            /**
             * @brief Create a button
             *
             * @param name The name of the button
             * @param str The text of the button
             * @param position The position of the button
             * @param size The size of the button
             */
            std::shared_ptr<Button>& CreateButton(const std::string&name, const std::string& str, const sf::Vector2f& position, const std::size_t size);

            /**
             * @brief Get all the buttons
             *
             * @return A map of all the buttons
             */
            void DestroyButton(const std::string& name);

            /**
             * @brief Get all the buttons
             *
             * @return A map of all the buttons
             */
            std::map<std::string, std::shared_ptr<Button>>& GetButtons();

            /**
             * @brief Get a button
             *
             * @param name The name of the button
             * @return A shared pointer to the button
             */
            std::shared_ptr<Button>& GetButton(const std::string& name);

            /**
             * @brief Create a text
             *
             * @param name The name of the text
             * @param str The string to display
             * @param position The position of the text
             * @param size The size of the text
             * @return A shared pointer to the text
             */
            std::shared_ptr<sf::Text>& CreateText(const std::string& name, const std::string& str, const sf::Vector2f& position, const std::size_t size);

            /**
             * @brief Destroy a text
             *
             * @param name The name of the text
             */
            void DestroyText(const std::string& name);

            /**
             * @brief Get all the texts
             *
             * @return A map of all the texts
             */
            std::map<std::string, std::shared_ptr<sf::Text>>& GetTexts();

            /**
             * @brief Get a text
             *
             * @param name The name of the text
             * @return A shared pointer to the text
             */
            std::shared_ptr<sf::Text>& GetText(const std::string& name);

            /**
             * @brief Push a new event to the server into the queue
             *
             * @param header The header of the event
             * @param data The data of the event (default is empty)
             */
            void PushEvent(const Shared::Data::Header header, const std::string& data = "");

            /**
             * @brief Push a new event from the server into the queue
             *
             * @param event The event to push
             */
            void PushEvent(const Shared::Data::Event& event);

            /**
             * @brief Pop an event from the queue
             *
             * @param catrgory The category of the event to pop
             * @return The event popped from the queue
             */
            Shared::Data::Event PopEvent(Shared::Data::Category catrgory);

            /**
             * @brief Check if the server event queue is empty
             *
             * @param category The category of the event to check
             * @return true if the event queue is empty, false otherwise
             */
            bool IsEventQueueEmpty(Shared::Data::Category category) const;

            /**
             * @brief Clear the server event queue
             *
             * @param category The category of the event to clear
             */
            void ClearEventQueue(Shared::Data::Category category);

            /**
             * @brief Check if the debug mode is enabled
             *
             * @return true if the debug mode is enabled, false otherwise
             */
            bool IsDebugEnabled() const;

            /**
             * @brief Set the id of the client
             *
             * @param id The id of the client
             */
            void SetId(Shared::Data::Id id);

            /**
             * @brief Get the id of the client
             *
             * @return The id of the client
             */
            Shared::Data::Id GetId() const;

            /**
             * @brief Load a map from a string
             *
             * @param raw The raw string containing the map data
             */
            void LoadMap(const std::string& raw);

            /**
             * @brief Get the map
             *
             * @return A vector of strings containing the map data
             */
            const std::vector<std::string>& GetMap() const;

            /**
             * @brief Set the new scene
             *
             * @return The name of the new scene
             */
            void SetScene(const std::string& scene);

            /**
             * @brief Get the current scene
             *
             * @return The name of the current scene
             */
            const std::string& GetScene() const;

            /**
             * @brief Set the wait text
             *
             * @param text The wait text
             */
            void SetWaitText(const std::string& text);

            /**
             * @brief Get the wait text
             *
             * @return The wait text
             */
            const std::string& GetWaitText() const;

            /**
             * @brief Set the players position
             *
             * @param me My position
             * @param other The new other position
             */
            void SetPositions(const sf::Vector2f& me, const sf::Vector2f& other);

            /**
             * @brief Get the players position
             *
             * @return The player position
             */
            const std::array<sf::Vector2f, 2>& GetPositions() const;

            /**
             * @brief Collect a coin, update the coins and remove the coin from the map
             *
             * @param position The position of the coin collected
             * @param me The number of coins I have
             * @param other The number of coins the other player has
             */
            void CollectCoin(const sf::Vector2f& position, std::uint32_t me, std::uint32_t other);

        private:
            /**
             * @brief Check if a sprite exists
             *
             * @param name The name of the sprite
             * @return true if the sprite exists, false otherwise
             */
            bool DoesSpriteExist(const std::string& name) const;

            /**
             * @brief Check if a button exists
             *
             * @param name The name of the button
             * @return true if the button exists, false otherwise
             */
            bool DoesButtonExist(const std::string& name) const;

            /**
             * @brief Check if a texture exists
             *
             * @param name The name of the texture
             * @return true if the texture exists, false otherwise
             */
            bool DoesTextureExist(const std::string& name) const;

            /**
             * @brief Load a texture
             *
             * @param path The path of the texture
             */
            void LoadTexture(const std::string& path);

            /**
             * @brief Load a font
             *
             * @param path The path of the font
             */
            void LoadFont(const std::string& path);

            /**
             * @brief Check if a text exists
             *
             * @param name The name of the text
             * @return true if the text exists, false otherwise
             */
            bool DoesTextExist(const std::string& name) const;

            std::unordered_map<std::string, std::unique_ptr<sf::Texture>> _textures; /*!< Map of all the textures */
            std::map<std::string, std::shared_ptr<sf::Sprite>> _sprites; /*!< Map of all the sprites */
            std::map<std::string, std::shared_ptr<sf::Text>> _texts; /*!< Map of all the texts */
            std::map<std::string, std::shared_ptr<Button>> _buttons; /*!< Map of all the buttons */
            std::queue<Shared::Data::Event> _eventsFromServer; /*!< A queue with all the events comming from the server */
            std::queue<Shared::Data::Event> _eventsToServer; /*!< A queue with all the events destinated to the server */
            std::array<sf::Vector2f, 2> _positions; /*!< The positions of the player */
            std::shared_ptr<sf::Font> _font; /*!< Default font to use */
            std::vector<std::string> _map; /*!< The map of the game */
            std::string _waitText; /*!< The text to display while waiting for the server */
            std::string _scene; /*!< The current scene */
            Shared::Data::Id _id; /*!< The id of the client */
            bool _isDebugEnabled; /*!< Whether the debug mode is enabled or not */
    };
}
