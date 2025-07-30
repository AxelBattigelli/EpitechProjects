/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Data.hpp
*/

#pragma once

#include <cstdint>
#include <string>

namespace Jetpack::Shared::Data
{
    /**
     * @struct Event
     * @brief Event structure to manage the events in the game
     */
    enum Header {
        CON_REQUESTED = 0x00, /*!< Connection requested */
        CON_ACCEPTED = 0x01, /*!< Connection accepted */
        CON_REJECTED = 0x02, /*!< Connection rejected */
        CON_CLOSED = 0x03, /*!< Connection closed */

        WANT_TO_PLAY = 0x10, /*!< Player wants to play to the game */
        WANT_TO_QUIT = 0x11, /*!< Player wants to quit the game */
        GAME_START = 0x12, /*!< The game has started */
        GAME_FINISH = 0x13, /*!< The game has finished */

        MAP_CONTENT = 0x20, /*!< The content of the map */
        COIN_COLLECTED = 0x21, /*!< A coin is collected */

        PLAYER_ACTION = 0x30, /*!< Player action */
        PLAYER_POSITION = 0x31, /*!< Player position, with the x and y coordinates separated by a space in the data */

        INTERNAL_ERROR = 0x50 /*!< Internal error */
    };

    /**
     * @struct Id
     * @brief Id structure to manage the ids of the actors
     */
    enum Id {
        NONE = -0x01, /*!< No id */
        SERVER = 0x00, /*!< The server */
        FST_CLIENT = 0x01, /*!< The first client */
        SND_CLIENT = 0x02 /*!< The second client */
    };

    /**
     * @enum Category
     * @brief The different event categories
     */
    enum Category {
        FROM_SERVER, /*!< Event from the server */
        TO_SERVER, /*!< Event to the server */
    };

    /**
     * @struct Event
     * @brief Event structure to manage the events in the game.
     */
    struct Event {
        Id sender; /*!< The id of the sender */
        Id receiver; /*!< The id of the receiver */
        Header header; /*!< The header of the event */
        std::string data; /*!< The data of the event */
    };
}
