/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Transceiver.cpp
*/

#include "Client/Logic/Transceiver.hpp"
#include "Shared/Exception.hpp"
#include "Shared/Logger.hpp"
#include "Shared/Utils.hpp"

#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <iostream>
#include <cstring>
#include <poll.h>
#include <thread>
#include <chrono>

Jetpack::Client::Transceiver::Transceiver(std::shared_ptr<Database> database, const std::string& address, const std::int16_t port) : _database(database), _running(true)
{
    _socketFd = socket(AF_INET, SOCK_STREAM, 0);

    if (_socketFd < 0) {
        throw Jetpack::Shared::Exception("Failed to create socket");
    }
    Connect(address, port);
    Shared::Logger::Info(std::format("Transceiver initialized with address: {} and port: {}", address, port), _database->IsDebugEnabled());
}

void Jetpack::Client::Transceiver::Stop()
{
    Shared::Logger::Info("Stopping transceiver...", _database->IsDebugEnabled());
    _running = false;
}

void Jetpack::Client::Transceiver::Run()
{
    struct pollfd fd {};

    fd.fd = _socketFd;
    fd.events = POLLIN | POLLOUT;
    while (_running) {
        std::int32_t result = poll(&fd, 1, 200);

        if (result > 0) {
            if (fd.revents & POLLIN) {
                ReceiveEvent();
            }
            if ((fd.revents & POLLOUT) && !_database->IsEventQueueEmpty(Shared::Data::Category::TO_SERVER)) {
                SendEvent();
            }
        } else if (result < 0) {
            std::cerr << "Poll error: " << strerror(errno) << std::endl;
        }
        if (!_database->IsEventQueueEmpty(Shared::Data::Category::TO_SERVER)) {
            SendEvent();
        }
    }
}

void Jetpack::Client::Transceiver::Connect(const std::string& address, const std::int16_t port) const
{
    struct sockaddr_in addr {};

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    if (inet_pton(AF_INET, address.c_str(), &addr.sin_addr) <= 0) {
        throw Shared::Exception(std::format("The address is not valid: {}", std::strerror(errno)));
    }
    if (connect(_socketFd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        throw Shared::Exception(std::format("Connection failed: {}", std::strerror(errno)));
    }
}

void Jetpack::Client::Transceiver::ReadMessage(std::string& buffer)
{
    std::size_t pos = 0;

    while ((pos = buffer.find("\r\n")) != std::string::npos) {
        try {
            std::string message = buffer.substr(0, pos);
            Shared::Data::Event event = Shared::Utils::DeserializeEvent(message);

            buffer.erase(0, pos + 2);
            Shared::Logger::Info(std::format("Received event: {}", message), _database->IsDebugEnabled());
            _database->PushEvent(event);
        } catch (const Shared::Exception& e) {
            Shared::Logger::Error(std::format("Error while processing message: {}", e.what()), _database->IsDebugEnabled());
        }
    }
}

void Jetpack::Client::Transceiver::ReceiveEvent()
{
    char buffer[8192] = {0};
    ssize_t bytesRead = read(_socketFd, buffer, sizeof(buffer) - 1);

    if (bytesRead > 0) {
        buffer[bytesRead] = '\0';
        std::string message(buffer);
        ReadMessage(message);
    } else if (!bytesRead) {
        throw Shared::Exception("Server closed connection.");
    } else if (errno != EAGAIN && errno != EWOULDBLOCK) {
        Shared::Logger::Error(std::format("Error receiving data: {}", std::strerror(errno)), _database->IsDebugEnabled());
    }
}

void Jetpack::Client::Transceiver::SendEvent()
{
    Shared::Data::Event event = _database->PopEvent(Shared::Data::Category::TO_SERVER);
    std::string serializedEvent = Shared::Utils::SerializeEvent(event);
    std::string packet = serializedEvent + "\r\n";

    if (write(_socketFd, packet.c_str(), packet.size()) > 0) {
        Shared::Logger::Info(std::format("Sending event: {}", serializedEvent), _database->IsDebugEnabled());
    } else {
        Shared::Logger::Error(std::format("Failed to send event: {}", std::strerror(errno)), _database->IsDebugEnabled());
    }
}
