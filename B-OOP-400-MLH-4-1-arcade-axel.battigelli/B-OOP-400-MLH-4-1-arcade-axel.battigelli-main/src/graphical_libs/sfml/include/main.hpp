#pragma once

#include "ffi.hpp"
#include <SFML/Graphics.hpp>
#include <cerrno>
#include <iostream>

class SfmlFrontend {
  private:
    sf::RenderWindow *_window;
    sf::Font *_gfont;

  public:
    SfmlFrontend();
    ~SfmlFrontend();
    void init();
    void deinit();
    void renderMap(tile_t **data, uint64_t length, const uint64_t *lengths);
    event_t getEvent();
    sf::Texture *loadTexture(const std::string &filePath);
};
