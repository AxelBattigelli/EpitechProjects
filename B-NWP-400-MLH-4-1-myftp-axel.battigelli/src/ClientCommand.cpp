#include "Client.hpp"
#include "Exception.hpp"
#include "Utils.hpp"
#include <string_view>
#include <algorithm>
#include <arpa/inet.h>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
#include <netinet/in.h>
#include <ostream>
#include <ranges>
#include <sched.h>
#include <stdexcept>
#include <string>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

void Client::passCommand()
{
    std::string tmpUsername;

    for (size_t x = 0; x < std::strlen(this->_username.c_str()); ++x)
        tmpUsername += (tolower(this->_username[x]));
    if (tmpUsername == "anonymous") {
        this->_loggedIn = true;
        this->sendResponse(AUTH_SUCCESS, "Successfully connected");
    } else {
        this->sendResponse(NOT_LOGGED_IN, "Invalid log in");
    }
}

void Client::pwdCommand()
{
    this->sendResponse(PATHNAME_CREATE, "\"" + this->getPWD() + "\" is curent home folder");
}

void Client::cwdCommand(const std::string &arg)
{
    struct stat sb;

    std::string path = arg;

    if (arg.empty()) {
        this->sendResponse(FILE_UNAVAILABLE, "Invalid path.");
        return;
    }
    if (arg[0] != '/') {
        path = this->getPWD() + "/" + arg;
    }
    while (path.find("//") != std::string::npos) {
        path.replace(path.find("//"), 2, "/");
    }
    if (stat(path.c_str(), &sb) != 0) {
        this->sendResponse(FILE_UNAVAILABLE, std::format("Cannot cd to path: {}", path));
        return;
    }
    this->setPWD(path);
    this->sendResponse(PATHNAME_UPDATE, this->getPWD());
}

void Client::cdupCommand()
{
    struct stat sb;

    std::string path = this->getPWD();
    std::size_t const idx = path.find_last_of('/');

    if (idx != std::string::npos) {
        path.erase(idx);
        if (path.empty()) {
            path = "/";
        }
    }
    if (stat(path.c_str(), &sb) != 0) {
        this->sendResponse(FILE_UNAVAILABLE, std::format("Cannot cd to path: {}", path));
        return;
    }
    this->setPWD(path);
    this->sendResponse(PATHNAME_UPDATE, "Directory successfully changed.");
}

void Client::portCommand(const std::string &arg)
{
    auto split = arg
        | std::ranges::views::split(',')
        | std::ranges::views::transform([](auto&& str) {
            return std::string_view(&*str.begin(), std::ranges::distance(str));
    });

    std::string ip_value;
    int port_value = 0;
    int index = 0;

    for (auto&& word : split) {
        int const num = std::stoi(std::string(word));

        if (index < 4) {
            if (!ip_value.empty()) {
                ip_value += ".";
            }
            ip_value += std::to_string(num);
        } else if (index == 4) {
            port_value += num * 256;
        } else if (index == 5) {
            port_value += num;
        }
        ++index;
    }

    std::cout << "IP Value: " << ip_value << "\n";
    std::cout << "Port Value: " << port_value << "\n";

    this->_ip = ip_value;
    this->_port = port_value;
    this->_mode = PORT;
    sendResponse(SUCCESS, "Successfully select port");
}

void Client::pasvCommand() {
    int const dataSocket = socket(AF_INET, SOCK_STREAM, 0);

    if (dataSocket < 0) {
        throw std::runtime_error("Error creating socket");
    }

    struct sockaddr_in serverAddr;
    serverAddr.sin_family = AF_INET;
    serverAddr.sin_addr.s_addr = inet_addr("0.0.0.0");
    serverAddr.sin_port = 0;

    if (bind(dataSocket, (struct sockaddr *)&serverAddr, sizeof(serverAddr)) == -1) {
        perror("Bind failed");
        close(dataSocket);
        throw std::runtime_error("Error with bind");
    }
    if (::listen(dataSocket, 5) < 0) {
        perror("Listen failed");
        close(dataSocket);
        return;
    }

    socklen_t len = sizeof(serverAddr);

    getsockname(dataSocket, (struct sockaddr *)&serverAddr, &len);
    this->_port = ntohs(serverAddr.sin_port);
    getsockname(this->_controlSocket->fd(), (struct sockaddr *)&serverAddr, &len);
    this->_ip = inet_ntoa(serverAddr.sin_addr);
    this->_mode = PASV;
    this->_dataSocket = dataSocket;

    std::string ip_str = this->_ip;
    std::ranges::replace(ip_str, '.', ',');
    std::string const pasv_response = std::format(
        "Entering Passive Mode ({},{},{})", ip_str, this->_port / 256, this->_port % 256);
    sendResponse(SUCCESS_PASV, pasv_response);
}

void Client::listCommand()
{
    std::string list = Utils::Exec((std::string("ls -l ") + this->getPWD()).c_str());
    list = Utils::ReplaceAll(list, std::string("\n"), std::string("\r\n"));

    int clientSocket;

    if (this->_mode == PORT) {
        clientSocket = socket(AF_INET, SOCK_STREAM, 0);
        if (clientSocket == -1) {
            sendResponse(FILE_UNAVAILABLE, "Failed to create socket");
            return;
        }

        if (std::string(this->_ip).empty() || this->_port == 0) {
            sendResponse(FILE_UNAVAILABLE, "PORT not set");
            close(clientSocket);
            return;   
        }

        sockaddr_in server_address = {};
        server_address.sin_family = AF_INET;
        server_address.sin_port = htons(this->_port);
        server_address.sin_addr.s_addr = inet_addr(this->_ip.c_str());

        if (connect(clientSocket, reinterpret_cast<sockaddr*>(&server_address), sizeof(server_address)) == -1) {
            sendResponse(FILE_UNAVAILABLE, "Failed to connect to server");
            close(clientSocket);
            return;
        }
    }
    if (this->_mode == PASV) {
        sockaddr_in serverAddr = {};
        serverAddr.sin_family = AF_INET;
        serverAddr.sin_port = htons(this->_port);
        serverAddr.sin_addr.s_addr = inet_addr(this->_ip.c_str());

        socklen_t len = sizeof(serverAddr);

        // pid_t const pid = fork();

        // if (pid == -1) {
        //     throw Exception("Fail to fork");
        //     exit(84);
        // }
        // if (pid == 0) {
            clientSocket = ::accept(this->_dataSocket, (struct sockaddr *) &serverAddr, &len);
            if (clientSocket < 0) {
                perror("Cann't accept client");
                return;
            }

        //     _exit(84);
        // }  
    }
    if (this->_mode == NOT_CONFIG) {
        sendResponse(FILE_UNAVAILABLE, "PORT not set");
    }

    sendResponse(FILE_STATUS_OK, "Open connexion for listing...");

    pid_t const pid = fork();

    if (pid == -1) {
        throw Exception("Fail to fork");
        exit(84);
    }
    if (pid == 0) {
        ssize_t const bytes_sent = ::write(clientSocket, list.c_str(), list.size());
        if (bytes_sent == -1) {
            sendResponse(FILE_UNAVAILABLE, "Failed to send file list");
        }
        shutdown(clientSocket, SHUT_RDWR);
        close(clientSocket);
        if (bytes_sent != -1)
        sendResponse(TRANSFER_ENDING, "End of listing");
        
        _exit(84);
    }   
}

void Client::storCommand(const std::string &arg)
{
    const std::string &fileName = arg;
    std::ofstream file(this->getPWD() + "/" + fileName, std::ios::binary);

    if (!file.is_open()) {
        sendResponse(FILE_UNAVAILABLE, "Failed to open file for writing");
        return;
    }

    int clientSocket;

    if (this->_mode == PORT) {
        clientSocket = socket(AF_INET, SOCK_STREAM, 0);
        if (clientSocket == -1) {
            sendResponse(FILE_UNAVAILABLE, "Failed to create socket");
            return;
        }

        if (std::string(this->_ip).empty() || this->_port == 0) {
            sendResponse(FILE_UNAVAILABLE, "PORT not set");
            close(clientSocket);
            return;   
        }

        sockaddr_in server_address = {};
        server_address.sin_family = AF_INET;
        server_address.sin_port = htons(this->_port);
        server_address.sin_addr.s_addr = inet_addr(this->_ip.c_str());

        if (connect(clientSocket, reinterpret_cast<sockaddr*>(&server_address), sizeof(server_address)) == -1) {
            sendResponse(FILE_UNAVAILABLE, "Failed to connect to server");
            close(clientSocket);
            return;
        }
    }
    if (this->_mode == PASV) {
        sockaddr_in serverAddr = {};
        serverAddr.sin_family = AF_INET;
        serverAddr.sin_port = htons(this->_port);
        serverAddr.sin_addr.s_addr = inet_addr(this->_ip.c_str());

        socklen_t len = sizeof(serverAddr);

        clientSocket = ::accept(this->_dataSocket, (struct sockaddr *) &serverAddr, &len);
        if (clientSocket < 0) {
            perror("Cann't accept client");
            return;
        }
    }
    if (this->_mode == NOT_CONFIG) {
        sendResponse(FILE_UNAVAILABLE, "PORT not set");
    }

    sendResponse(FILE_STATUS_OK, "Open connexion for listing...");

    pid_t const pid = fork();

    if (pid == -1) {
        throw Exception("Fail to fork");
        exit(84);
    }
    if (pid == 0) {
        char buffer[1024];
        ssize_t bytes_received;
        while ((bytes_received = ::read(clientSocket, buffer, sizeof(buffer))) > 0) {
            file.write(buffer, bytes_received);
        }

        if (bytes_received == -1) {
            sendResponse(FILE_UNAVAILABLE, "Failed to receive file");
        }

        shutdown(clientSocket, SHUT_RDWR);
        close(clientSocket);
        sendResponse(TRANSFER_ENDING, "End of file storage");
        
        _exit(84);
    } 
}

void Client::retrCommand(const std::string &arg)
{
    const std::string &fileName = arg;
    std::ifstream file(this->getPWD() + "/" + fileName, std::ios::binary);

    if (!file.is_open()) {
        sendResponse(FILE_UNAVAILABLE, "File not found");
        return;
    }

    int clientSocket;

    if (this->_mode == PORT) {
        clientSocket = socket(AF_INET, SOCK_STREAM, 0);
        if (clientSocket == -1) {
            sendResponse(FILE_UNAVAILABLE, "Failed to create socket");
            return;
        }

        if (std::string(this->_ip).empty() || this->_port == 0) {
            sendResponse(FILE_UNAVAILABLE, "PORT not set");
            close(clientSocket);
            return;   
        }

        sockaddr_in server_address = {};
        server_address.sin_family = AF_INET;
        server_address.sin_port = htons(this->_port);
        server_address.sin_addr.s_addr = inet_addr(this->_ip.c_str());

        if (connect(clientSocket, reinterpret_cast<sockaddr*>(&server_address), sizeof(server_address)) == -1) {
            sendResponse(FILE_UNAVAILABLE, "Failed to connect to server");
            close(clientSocket);
            return;
        }
    }
    if (this->_mode == PASV) {
        sockaddr_in serverAddr = {};
        serverAddr.sin_family = AF_INET;
        serverAddr.sin_port = htons(this->_port);
        serverAddr.sin_addr.s_addr = inet_addr(this->_ip.c_str());

        socklen_t len = sizeof(serverAddr);

        clientSocket = ::accept(this->_dataSocket, (struct sockaddr *) &serverAddr, &len);
        if (clientSocket < 0) {
            perror("Cann't accept client");
            return;
        }
    }
    if (this->_mode == NOT_CONFIG) {
        sendResponse(FILE_UNAVAILABLE, "PORT not set");
    }

    sendResponse(FILE_STATUS_OK, "Open connexion for listing...");

    pid_t const pid = fork();

    if (pid == -1) {
        throw Exception("Fail to fork");
        exit(84);
    }
    if (pid == 0) {
        std::string const fileContent((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
        ssize_t const bytes_sent = ::write(clientSocket, fileContent.c_str(), fileContent.size());
        if (bytes_sent == -1) {
            sendResponse(FILE_UNAVAILABLE, "Failed to send file");
        }

        shutdown(clientSocket, SHUT_RDWR);
        close(clientSocket);
        sendResponse(TRANSFER_ENDING, "End of file retrieval");
        
        _exit(84);
    }
}

void Client::deleCommand(const std::string &arg)
{
    const std::string &fileName = arg;
    std::ifstream file(this->getPWD() + "/" + fileName, std::ios::binary);

    if (!file.is_open()) {
        sendResponse(FILE_UNAVAILABLE, "File not found");
        return;
    }

    file.close();

    if (std::remove((this->getPWD() + "/" + fileName).c_str()) != 0) {
        sendResponse(FILE_UNAVAILABLE, "Failed to delete file");
    } else {
        sendResponse(PATHNAME_UPDATE, "File deleted successfully");
    }
}
