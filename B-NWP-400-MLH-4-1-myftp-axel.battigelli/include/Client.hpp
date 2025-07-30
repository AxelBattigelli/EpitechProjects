#pragma once

#include "TcpSocket.hpp"
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <poll.h>
#include <string>
#include <sys/poll.h>

enum FTPStatus {
    FILE_STATUS_OK = 150,
    SUCCESS = 200,
    HELP_STATUS = 214,
    SERVICE_READY = 220,
    DISCONNECTED = 221,
    TRANSFER_ENDING = 226,
    SUCCESS_PASV = 227,
    AUTH_SUCCESS = 230,
    PATHNAME_UPDATE = 250,
    PATHNAME_CREATE = 257,
    NEED_PASSWORD = 331,
    NOT_LOGGED_IN = 530,
    COMMAND_UNKNOWN = 500,
    FILE_UNAVAILABLE = 550
};

enum SelectedMode {
    NOT_CONFIG = 0,
    PORT = 1,
    PASV = 2,
};

class Client {
  public:
    explicit Client(std::unique_ptr<TcpSocket> socket, std::string _anonymousHome);
    void sendResponse(FTPStatus status, const std::string &message = "");
    std::string getPWD();
    void setPWD(std::string);
    [[nodiscard]] int fd() const { return this->_controlSocket->fd(); };
    bool ready(const pollfd&);

    void passCommand();
    void pwdCommand();
    void cwdCommand(const std::string &);
    void cdupCommand();
    void portCommand(const std::string &);
    void pasvCommand();
    void listCommand();
    void storCommand(const std::string &);
    void retrCommand(const std::string &);
    void deleCommand(const std::string &);

  private:
    bool handleCommand(const std::string &command, const std::string &arg);
    std::unique_ptr<TcpSocket> _controlSocket;
    std::string _username;
    bool _loggedIn = false;
    std::string _pwd;
    int _port = 0;
    std::string _ip;
    std::string _buffer;
    SelectedMode _mode = NOT_CONFIG;
    int _dataSocket;
};
