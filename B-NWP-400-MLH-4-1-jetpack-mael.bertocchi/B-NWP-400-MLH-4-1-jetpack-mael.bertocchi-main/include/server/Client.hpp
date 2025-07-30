#pragma once

#include <string>
#include <sys/socket.h>

namespace Jetpack::Server
{
  class Client {
    public:
      int _socket;
      bool _isActive;
      int _id = 0;
      bool _wantPlay = false;
      int _x = 0;
      int _y = 550;
      bool _jetpackActive = false;
      int _scoreCoins = 0;

      Client(int socket, bool isActive) : _socket(socket), _isActive(isActive) {}
  };
}
