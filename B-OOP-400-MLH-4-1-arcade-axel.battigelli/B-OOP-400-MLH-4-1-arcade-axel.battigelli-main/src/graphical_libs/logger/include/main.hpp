#pragma once

#include "ffi.hpp"
#include <cerrno>
#include <iostream>
#include <ncurses.h>

class NcursesFrontend {
  private:
    WINDOW *_window;

  public:
    NcursesFrontend();
    ~NcursesFrontend();

    void deinit();
    void init();
    void renderMap(tile_t **data, uint64_t length, const uint64_t *lengths);
    event_t getEvent();
};
