/*
** EPITECH PROJECT, 2025
** ZappyKomJamet
** File description:
** NetworkClient
*/

#include "NetworkClient.hpp"
#include "../Wrappers/Network/NetworkWrapper.hpp"

#define COLOR_SEND     "\033[1;34m"
#define COLOR_TAG      "\033[1;32m"
#define COLOR_MSG      "\033[0;37m"
#define COLOR_RESET    "\033[0m"

void NetworkClient::debugPrint(const std::string &tag, const std::string &msg) const
{
    std::cout << COLOR_SEND << "[DEBUG-SEND] " << COLOR_TAG << tag << " " << COLOR_MSG << msg << COLOR_RESET << std::endl;
}

NetworkClient::NetworkClient(const std::string &host, int port)
    : sockfd(-1), host(host), port(port), buffer("")
{
}

NetworkClient::~NetworkClient()
{
    closeConnection();
}

bool NetworkClient::connectToServer()
{
    struct addrinfo hints{}, *res;
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    int status = NetworkWrapper::getaddrinfo(host.c_str(), std::to_string(port).c_str(), &hints, &res);
    if (status != 0) {
        std::cerr << "getaddrinfo: " << NetworkWrapper::gai_strerror(status) << std::endl;
        return false;
    }

    sockfd = NetworkWrapper::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (sockfd == -1) {
        NetworkWrapper::perror("socket");
        NetworkWrapper::freeaddrinfo(res);
        return false;
    }

    if (NetworkWrapper::connect(sockfd, res->ai_addr, res->ai_addrlen) == -1) {
        NetworkWrapper::perror("connect");
        NetworkWrapper::close(sockfd);
        sockfd = -1;
        NetworkWrapper::freeaddrinfo(res);
        return false;
    }
    NetworkWrapper::fcntl(sockfd, F_SETFL, O_NONBLOCK);
    NetworkWrapper::freeaddrinfo(res);
    return true;
}

void NetworkClient::closeConnection()
{
    if (sockfd != -1) {
        NetworkWrapper::close(sockfd);
        sockfd = -1;
    }
}

std::string NetworkClient::readLine()
{
    char c;
    ssize_t n;
    while (true) {
        n = NetworkWrapper::recv(sockfd, &c, 1, 0);
        if (n > 0) {
            if (c == '\n') {
                std::string line = buffer;
                buffer.clear();
                return line;
            }
            buffer += c;
        } else {
            break;
        }
    }
    return "";
}

bool NetworkClient::sendMsz(bool debugMode) const
{
    if (debugMode)
        debugPrint("MSZ", "Sending MSZ command to server.");
    return sendLine("msz");
}

bool NetworkClient::sendBct(int x, int y, bool debugMode) const
{
    if (debugMode)
        debugPrint("BCT", "Sending BCT command for coordinates (" + std::to_string(x) + ", " + std::to_string(y) + ") to server.");
    return sendLine("bct " + std::to_string(x) + " " + std::to_string(y));
}

bool NetworkClient::sendMct(bool debugMode) const
{
    if (debugMode)
        debugPrint("MCT", "Sending MCT command to server.");
    return sendLine("mct");
}

bool NetworkClient::sendTna(bool debugMode) const
{
    if (debugMode)
        debugPrint("TNA", "Sending TNA command to server.");
    return sendLine("tna");
}

bool NetworkClient::sendPpo(int playerId, bool debugMode) const
{
    if (debugMode)
        debugPrint("PPO", "Sending PPO command for player ID " + std::to_string(playerId) + " to server.");
    return sendLine("ppo " + std::to_string(playerId));
}

bool NetworkClient::sendPlv(int playerId, bool debugMode) const
{
    if (debugMode)
        debugPrint("PLV", "Sending PLV command for player ID " + std::to_string(playerId) + " to server.");
    return sendLine("plv " + std::to_string(playerId));
}

bool NetworkClient::sendPin(int playerId, bool debugMode) const
{
    if (debugMode)
        debugPrint("PIN", "Sending PIN command for player ID " + std::to_string(playerId) + " to server.");
    return sendLine("pin " + std::to_string(playerId));
}

bool NetworkClient::sendSgt(bool debugMode) const
{
    if (debugMode)
        debugPrint("SGT", "Sending SGT command to server.");
    return sendLine("sgt");
}

bool NetworkClient::sendSst(int time, bool debugMode) const
{
    if (debugMode)
        debugPrint("SST", "Sending SST command with time " + std::to_string(time) + " to server.");
    return sendLine("sst " + std::to_string(time));
}

bool NetworkClient::sendLine(const std::string &line) const
{
    std::string toSend = line + "\n";
    ssize_t sent = NetworkWrapper::send(sockfd, toSend.c_str(), toSend.size(), 0);
    return sent == (ssize_t)toSend.size();
}

int NetworkClient::getSocketFd() const
{
    return sockfd;
}
