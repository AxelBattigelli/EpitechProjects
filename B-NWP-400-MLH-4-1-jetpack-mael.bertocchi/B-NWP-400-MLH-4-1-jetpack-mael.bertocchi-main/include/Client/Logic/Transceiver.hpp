/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Transceiver.hpp
*/

#include "Client/Logic/Database.hpp"

namespace Jetpack::Client
{
    /**
     * @class Transceiver
     * @brief Transceiver class to manage the communication with the server.
     */
    class Transceiver
    {
        public:
            /**
             * @brief Create a new transceiver
             *
             * @param database The database to use
             * @param address The address of the server
             * @param port The port of the server
             */
            Transceiver(std::shared_ptr<Database> database, const std::string& address, const std::int16_t port);

            /**
             * @brief Stop the transceiver
             */
            void Stop();

            /**
             * @brief Run the transceiver
             */
            void Run();

        private:
            /**
             * @brief Connect to the server
             *
             * @param address The address of the server
             * @param port The port of the server
             */
            void Connect(const std::string& address, const std::int16_t port) const;

            /**
             * @brief Split the message into its events
             *
             * @param buffer The buffer containing the events
             */
            void ReadMessage(std::string& buffer);

            /**
             * @brief Send the last event of the queue to the server
             */
            void SendEvent(void);

            /**
             * @brief Receive an event from the server
             */
            void ReceiveEvent(void);

            std::shared_ptr<Database> _database; /*!< A pointer to the database */
            std::int32_t _socketFd; /*!< The socket file descriptor */
            bool _running; /*!< Whether the transceiver is running or not */
    };
}
