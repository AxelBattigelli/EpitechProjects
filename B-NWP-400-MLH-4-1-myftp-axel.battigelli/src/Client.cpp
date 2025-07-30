#include "Client.hpp"
#include "TcpSocket.hpp"
#include "Utils.hpp"
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <sys/poll.h>
#include <utility>

Client::Client(std::unique_ptr<TcpSocket> socket, std::string _anonymousHome)
    : _controlSocket(std::move(socket)), _pwd(std::move(std::move(_anonymousHome)))
{
    sendResponse(SERVICE_READY, "myFTP server");
}

bool Client::ready(const pollfd& polled)
{
    if ((polled.revents & POLLIN) != 0) {
        std::string const input = _controlSocket->receive();
        if (input.empty()) {
            return false;
        }
        this->_buffer += input;
        size_t pos;
        while ((pos = this->_buffer.find("\r\n")) != std::string::npos) {
            std::string const line = this->_buffer.substr(0, pos);
            this->_buffer = this->_buffer.substr(pos + 2);
            std::istringstream stream(line);
            std::string command;
            std::string arg;

            stream >> command;
            std::getline(stream, arg);
            if (!arg.empty() && arg[0] == ' ') {
                arg.erase(0, 1);
            }
            if (!handleCommand(command, arg)) {
                return false;
            }
        }
    }
    return true;
}

void Client::sendResponse(FTPStatus status, const std::string &message)
{
    std::string const response = std::to_string(status) + " " + message + "\r\n";
    _controlSocket->send(response);
}

std::string Client::getPWD()
{
    return this->_pwd;
}

void Client::setPWD(std::string newPwd)
{
    this->_pwd = std::move(newPwd);
}

bool Client::handleCommand(const std::string &command, const std::string &arg)
{
    std::cout << this->fd() << "'" << command << "'\n";
    std::cout << "DBG : " << arg << "\n";
    std::string const upperCommand(Utils::toUpperString(command));

    if (upperCommand == "USER") {
        this->_username = arg;
        this->sendResponse(NEED_PASSWORD, "Password require");
        return true;
    }
    if (upperCommand == "PASS") {
        this->passCommand();
        return true;
    }
    if (upperCommand == "QUIT") {
        this->_loggedIn = false;
        this->sendResponse(DISCONNECTED, "Bye...");
        return false;
    }
    if (upperCommand == "HELP") {
        this->sendResponse(HELP_STATUS, "The following commands are recognized.\nUSER\nPASS\nLIST\nCWD\nPWD\nRETR\nSTOR\nDELE\nQUIT\nHELP");
        return true;
    }
    if (upperCommand == "PWD") {
        if (!this->_loggedIn) {
            this->sendResponse(NOT_LOGGED_IN, "Your are not logged in");
            return true;
        }
        this->pwdCommand();
        return true;
    }
    if (upperCommand == "CWD") {
        if (!this->_loggedIn) {
            this->sendResponse(NOT_LOGGED_IN, "Your are not logged in");
            return true;
        }
        this->cwdCommand(arg);
        return true;
    }
    if (upperCommand == "CDUP") {
        if (!this->_loggedIn) {
            this->sendResponse(NOT_LOGGED_IN, "Your are not logged in");
            return true;
        }
        this->cdupCommand();
        return true;
    }
    if (upperCommand == "LIST") {
        if (!this->_loggedIn) {
            this->sendResponse(NOT_LOGGED_IN, "Your are not logged in");
            return true;
        }
        this->listCommand();
        return true;
    }
    if (upperCommand == "NOOP") {
        this->sendResponse(SUCCESS, "Successfully did nothing");
        return true;
    }
    if (upperCommand == "TYPE") {
        std::cout << "type: " << arg << "\n";
        this->sendResponse(SUCCESS, "Successfully select type");
        return true;
    }
    if (upperCommand == "PORT") {
        if (!this->_loggedIn) {
            this->sendResponse(NOT_LOGGED_IN, "Your are not logged in");
            return true;
        }
        this->portCommand(arg);
        return true;
    }
    if (upperCommand == "PASV") {
        if (!this->_loggedIn) {
            this->sendResponse(NOT_LOGGED_IN, "Your are not logged in");
            return true;
        }
        this->pasvCommand();
        return true;
    }
    if (upperCommand == "STOR") {
        if (!this->_loggedIn) {
            this->sendResponse(NOT_LOGGED_IN, "Your are not logged in");
            return true;
        }
        this->storCommand(arg);
        return true;
    }
    if (upperCommand == "RETR") {
        if (!this->_loggedIn) {
            this->sendResponse(NOT_LOGGED_IN, "Your are not logged in");
            return true;
        }
        this->retrCommand(arg);
        return true;
    } 
    if (upperCommand == "DELE") {
        if (!this->_loggedIn) {
            this->sendResponse(NOT_LOGGED_IN, "Your are not logged in");
            return true;
        }
        this->deleCommand(arg);
        return true;
    } 

    sendResponse(COMMAND_UNKNOWN, "Unknown command");
    return true;
}
