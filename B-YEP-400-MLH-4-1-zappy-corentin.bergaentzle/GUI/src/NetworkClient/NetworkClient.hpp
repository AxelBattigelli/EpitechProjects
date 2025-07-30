/*
** EPITECH PROJECT, 2025
** ZappyKomJamet
** File description:
** NetworkClient
*/

#pragma once

#include <cstddef>
#include <string>

class NetworkClient {
    public:
        NetworkClient(const std::string &host, int port);
        ~NetworkClient();

        void debugPrint(const std::string &tag, const std::string &msg) const;

        bool connectToServer();
        void closeConnection();
        std::string readLine();
        int getSocketFd() const;

        bool sendLine(const std::string &line) const;
        bool sendMsz(bool debugMode) const;
        bool sendBct(int x, int y, bool debugMode) const;
        bool sendMct(bool debugMode) const;
        bool sendTna(bool debugMode) const;
        bool sendPpo(int playerId, bool debugMode) const;
        bool sendPlv(int playerId, bool debugMode) const;
        bool sendPin(int playerId, bool debugMode) const;
        bool sendSgt(bool debugMode) const;
        bool sendSst(int time, bool debugMode) const;

    private:
        int sockfd;
        std::string host;
        int port;
        std::string buffer;
};
