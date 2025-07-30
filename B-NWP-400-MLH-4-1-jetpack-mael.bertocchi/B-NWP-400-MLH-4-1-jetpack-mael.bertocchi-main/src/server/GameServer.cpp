/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Main.cpp
*/

#include "Exception.hpp"
#include "server/Client.hpp"
#include "server/GameServer.hpp"
#include "server/Utils.hpp"
#include "Shared/Logger.hpp"
#include <iomanip>
#include <iostream>
#include <fcntl.h>
#include <format>
#include <fstream>
#include <mutex>
#include <netinet/in.h>
#include <poll.h>
#include <string.h>
#include <sys/socket.h>
#include <sstream>
#include <unistd.h>
#include <vector>
#include <thread>

Jetpack::Server::GameServer::GameServer() : _serverSocket(-1), _isRunning(false), _debug(false) {
    std::cout << "Starting ...\n";
}

Jetpack::Server::GameServer::~GameServer() {
    for (Client clientSocket : _clients) {
        std::clog << "Client " << clientSocket._id << " disconnected\n";
        close(clientSocket._socket);
    }
    if (_serverSocket >= 0) {
        close(_serverSocket);
    }
}

void Jetpack::Server::GameServer::init_server(int port, const std::string &mapFile, bool debug) {
    load_map(mapFile);

    this->_debug = debug;

    this->_serverSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (this->_serverSocket < 0) {
        throw Exception("Fail to create socket");
    }

    int reuse = 1;
    if (setsockopt(this->_serverSocket, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) < 0) {
        throw Exception("Failed to set SO_REUSEADDR");
    }

    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(port);

    if (bind(this->_serverSocket, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        throw Exception("Can't bind");
    }

    listen(this->_serverSocket, 2);

    std::cout << "Server listen on port " << port << "\n";
}

void Jetpack::Server::GameServer::Treatment(std::string buffer, int client_fd)
{
    std::string rawData(buffer);
    if (rawData.size() < 12) {
        Shared::Logger::Error(std::format("Malformed packet (too short): {}", rawData), this->_debug);
        return;
    }

    std::string fixed = rawData.substr(0, 2);
    std::string emit = rawData.substr(2, 2);
    std::string recv = rawData.substr(4, 2);
    std::string header = rawData.substr(6, 2);
    std::string data = rawData.substr(8, rawData.size() - 14);
    std::string checksum = rawData.substr(rawData.size() - 6, 4);

    std::string res = fixed + emit + recv + header + data;
    unsigned char checksum_recalc = 0;
    for (char c : res) {
        checksum_recalc += static_cast<unsigned char>(c);
    }
    std::ostringstream checksum_stream;
    checksum_stream << std::hex << std::setw(4) << std::setfill('0') << static_cast<int>(checksum_recalc);

    if (checksum == checksum_stream.str()) {
        if (header == "10") {
            if (this->_alreadyStart) {
                send_data(client_fd, "00", emit, "50", "01");
                return;
            }
            this->_clients.at(std::stoi(emit) - 1)._wantPlay = true;
            if (this->_clients.size() == 2) {
                for (auto& client : this->_clients) {
                    if (client._isActive) {
                        send_data(client._socket, "00", Utils::int_to_hex(client._id), "12", "");
                    }
                    client._x = 0;
                    client._scoreCoins = 0;
                }
                this->_alreadyStart = true;
            }
        }
        if (header == "30") {
            if (!this->_alreadyStart) {
                send_data(client_fd, "00", emit, "50", "01");
                return;
            }
            if (data == "")
                this->_clients.at(std::stoi(emit) - 1)._jetpackActive = false;
            if (data == "01")
                this->_clients.at(std::stoi(emit) - 1)._jetpackActive = true;
        }
    } else {
        send_data(client_fd, "00", emit, "50", "02");
    }
}

void Jetpack::Server::GameServer::Collide()
{
    for (auto& c : this->_clients) {
        int gridX = static_cast<int>(c._x) / 45;
        int gridY = static_cast<int>(c._y) / 72;

        if (gridY >= 0 && gridY < static_cast<int>(this->_gameMap.size()) &&
            gridX >= 0 && gridX < static_cast<int>(this->_gameMap[gridY].size())) {

            if (this->_gameMap[gridY][gridX] == 'e') {
                if (c._id == 1) {
                    send_data(this->_clients[0]._socket, "00", "01", "13", "02");
                    send_data(this->_clients[1]._socket, "00", "02", "13", "02");
                } else if (c._id == 2) {
                    send_data(this->_clients[0]._socket, "00", "01", "13", "01");
                    send_data(this->_clients[1]._socket, "00", "02", "13", "01");
                }
                this->_alreadyStart = false;
            }
            if (this->_gameMap[gridY][gridX] == 'c') {
                std::pair<int, int> coinPos = {gridY, gridX};

                if (!this->_coinsCollectedByPlayer[c._id].contains(coinPos)) {
                    this->_coinsCollectedByPlayer[c._id].insert(coinPos);
                    c._scoreCoins += 1;

                    std::string coinsStr1 = std::format("{} {} {} {}", gridX, gridY, this->_clients[0]._scoreCoins, this->_clients[1]._scoreCoins);
                    std::string coinsStr2 = std::format("{} {} {} {}", gridX, gridY, this->_clients[1]._scoreCoins, this->_clients[0]._scoreCoins);

                    send_data(this->_clients[0]._socket, "00", "01", "21", coinsStr1);
                    send_data(this->_clients[1]._socket, "00", "02", "21", coinsStr2);
                }
                bool allCollected = true;
                for (auto& otherClient : this->_clients) {
                    if (!this->_coinsCollectedByPlayer[otherClient._id].contains(coinPos)) {
                        allCollected = false;
                        break;
                    }
                }
                if (allCollected) {
                    this->_gameMap[gridY][gridX] = '_';
                }
            }
        } else {
            std::string winner = "";
            if (this->_clients[0]._scoreCoins > this->_clients[1]._scoreCoins)
                winner = "01";
            else if (this->_clients[0]._scoreCoins < this->_clients[1]._scoreCoins)
                winner = "02";
            else
                winner = "00";

            send_data(this->_clients[0]._socket, "00", "01", "13", winner);
            send_data(this->_clients[1]._socket, "00", "02", "13", winner);
            this->_alreadyStart = false;
            break;
        }
    }
}

void Jetpack::Server::GameServer::runThread()
{
    this->_gameLoopThread = std::thread([this]() {
        while (this->_isRunning) {
            std::this_thread::sleep_for(std::chrono::milliseconds(8));

            std::lock_guard<std::mutex> lock(this->_clientsMutex);

            if (this->_clients.size() == 2 && this->_alreadyStart) {
                Client* client1 = nullptr;
                Client* client2 = nullptr;

                for (auto& c : this->_clients) {
                    c._x += 1;
                }

                for (auto& c : this->_clients) {
                    if (c._id == 1) {
                        if (c._jetpackActive) {
                            c._y -= 2;
                        } else {
                            c._y += 1;
                        }
                        if (c._y > 550)
                            c._y = 550;
                        else if (c._y < 60)
                            c._y = 60;
                        client1 = &c;
                    } else if (c._id == 2) {
                        if (c._jetpackActive) {
                            c._y -= 2;
                        } else {
                            c._y += 1;
                        }
                        if (c._y > 550)
                            c._y = 550;
                        else if (c._y < 60)
                            c._y = 60;
                        client2 = &c;
                    }
                }

                if (client1 && client2 && client1->_isActive && client2->_isActive) {
                    for (auto& client : this->_clients) {
                        std::string coordData1 = std::format("{:02} {:02} {:02} {:02}", client1->_x, client1->_y, client2->_x, client2->_y);
                        std::string coordData2 = std::format("{:02} {:02} {:02} {:02}", client2->_x, client2->_y, client1->_x, client1->_y);

                        if (client._id == 1) {
                            send_data(client._socket, "00", "01", "31", coordData1);
                        } else if (client._id == 2) {
                            send_data(client._socket, "00", "02", "31", coordData2);
                        }
                    }
                }
                this->Collide();
            }
        }
    });
}

void Jetpack::Server::GameServer::accept_clients()
{
    fcntl(_serverSocket, F_SETFL, O_NONBLOCK);

    std::vector<struct pollfd> poll_fds;
    pollfd server_pollfd = {
        .fd = _serverSocket,
        .events = POLLIN,
        .revents = 0
    };
    poll_fds.push_back(server_pollfd);

    this->_isRunning = true;
    this->runThread();

    while (this->_isRunning) {
        int ready = poll(poll_fds.data(), poll_fds.size(), 100);
        if (ready < 0) {
            perror("poll");
            continue;
        }

        for (size_t i = 0; i < poll_fds.size(); ++i) {
            if (poll_fds[i].revents & POLLIN) {
                if (poll_fds[i].fd == _serverSocket) {
                    sockaddr_in client_addr;
                    socklen_t client_len = sizeof(client_addr);
                    int clientSocket = accept(_serverSocket, (struct sockaddr*)&client_addr, &client_len);
                    if (clientSocket < 0) {
                        perror("accept fail");
                        continue;
                    }

                    fcntl(clientSocket, F_SETFL, O_NONBLOCK);

                    std::lock_guard<std::mutex> lock(_clientsMutex);

                    int currentPlayers = 0;
                    for (auto &client : _clients) {
                        if (client._isActive) currentPlayers++;
                    }

                    Client newClient{clientSocket, currentPlayers < 2};
                    newClient._id = _clients.size() + 1;

                    std::clog << "Client " << newClient._id << " connected\n";
                    if (newClient._isActive) {
                        std::string playerId = currentPlayers == 0 ? "01" : "02";
                        send_data(clientSocket, "00", playerId, "01", playerId);

                        std::string result;
                        for (const auto& row : this->_gameMap) {
                            result.append(row.begin(), row.end());
                            result.push_back('\n');
                        }

                        send_data(clientSocket, "00", std::format("{:02}", newClient._id), "20", result);
                    } else {
                        send_data(clientSocket, "00", "00", "02", "00");
                        std::clog << "Client " << newClient._id << " refused and disconnected\n";
                        close(clientSocket);
                        continue;
                    }

                    pollfd clientPollFd = {
                        .fd = clientSocket,
                        .events = POLLIN,
                        .revents = 0
                    };
                    poll_fds.push_back(clientPollFd);
                    _clients.push_back(newClient);
                } else {
                    int client_fd = poll_fds[i].fd;
                    char buffer[1024];
                    ssize_t bytesReceived = read(client_fd, buffer, sizeof(buffer) - 1);

                    if (bytesReceived <= 0) {
                        std::cout << "Client " << i << " disconnected\n";
                        close(client_fd);
                        poll_fds.erase(poll_fds.begin() + i);

                        std::lock_guard<std::mutex> lock(_clientsMutex);
                        for (auto it = _clients.begin(); it != _clients.end(); ++it) {
                            if (it->_socket == client_fd) {
                                _clients.erase(it);
                                break;
                            }
                        }

                        --i;
                        continue;
                    }

                    std::ostringstream oss;
                    for (ssize_t j = 0; j < bytesReceived; ++j) {
                        oss << std::hex << std::setw(2) << std::setfill('0')
                            << (int)(unsigned char)buffer[j];
                    }

                    std::string hexString = oss.str();

                    auto hexToString = [](const std::string& hex) {
                        std::string result;
                        for (size_t i = 0; i < hex.length(); i += 2) {
                            std::string byteStr = hex.substr(i, 2);
                            char ch = static_cast<char>(std::stoi(byteStr, nullptr, 16));
                            result += ch;
                        }
                        return result;
                    };

                    std::string decodedText = hexToString(hexString);
                    Shared::Logger::Info(std::format("Received event: {}", decodedText), this->_debug);
                    this->Treatment(decodedText, client_fd);
                }
            }
        }
    }

    for (auto& pfd : poll_fds) {
        close(pfd.fd);
    }
    if (this->_gameLoopThread.joinable())
        this->_gameLoopThread.join();
}

void Jetpack::Server::GameServer::send_data(int clientSocket, const std::string &emit, const std::string &dest, const std::string &type, const std::string &data) const {
    std::ostringstream oss;
    oss << std::hex << data.size();

    std::string res = "aa" + emit + dest + type + Utils::string_to_hex(data);

    unsigned char checksum = 0;
    for (char c : res) {
        checksum += static_cast<unsigned char>(c);
    }

    std::ostringstream checksum_stream;
    checksum_stream << std::hex << std::setw(4) << std::setfill('0') << static_cast<int>(checksum);

    std::string final_res = res + checksum_stream.str() + "\r\n";
    Shared::Logger::Info(std::format("Sending event: {}", final_res), this->_debug);

    ::write(clientSocket, final_res.c_str(), final_res.size());
}

void Jetpack::Server::GameServer::load_map(const std::string &mapFile) {
    std::ifstream map(mapFile);
    if (!map.is_open()) {
        throw Exception("Can't open this map file");
    }

    std::string line;
    size_t lineLength = 0;

    while (std::getline(map, line)) {
        if (line.empty()) {
            continue;
        }

        if (lineLength == 0) {
            lineLength = line.length();
        } else if (line.length() != lineLength) {
            throw Exception("Bad line length");
        }

        for (char c : line) {
            if (c != '_' && c != 'e' && c != 'c') {
                throw Exception("Invalid character in map");
            }
        }

        std::vector<char> row(line.begin(), line.end());
        this->_gameMap.push_back(row);
    }
    map.close();
}
