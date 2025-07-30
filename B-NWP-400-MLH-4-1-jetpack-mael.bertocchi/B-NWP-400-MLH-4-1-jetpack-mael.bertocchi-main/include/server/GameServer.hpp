/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** GameServer.hpp
*/

#pragma once

#include <mutex>
#include <set>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>
#include "Client.hpp"

namespace Jetpack::Server
{
    class GameServer {
        private:
            int _serverSocket;
            std::vector<Client> _clients;
            std::mutex _clientsMutex;
            bool _isRunning;
            bool _debug;
            bool _alreadyStart = false;
            std::vector<std::vector<char>> _gameMap;
            std::unordered_map<int, std::set<std::pair<int, int>>> _coinsCollectedByPlayer;
            std::thread _gameLoopThread;

        public:
            GameServer();
            ~GameServer();
            void init_server(int port, const std::string &mapFile, bool debug);

            void accept_clients();
            void handle_client(Client client);
            void Treatment(std::string buffer, int client_fd);
            void runThread();
            void Collide();
            void load_map(const std::string &map_file);
            void send_data(int clientSocket, const std::string &emit, const std::string &dest, const std::string &type, const std::string &data) const;
    };
}
