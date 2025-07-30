#pragma once

#include "ffi.hpp"
#include <SDL2/SDL_ttf.h>
#include <unordered_map>
#include <SDL2/SDL.h>
#include <array>
#include <cerrno>
#include <iostream>

struct TileThing {
    std::string image_path;
    color_t front, back;
    std::array<char, 2> characters;

    TileThing(const tile_t &input)
        : image_path(input.image_path.ptr, input.image_path.len), front(input.front),
          back(input.back), characters{input.characters[0], input.characters[1]}
    {}
};

bool operator==(const TileThing &l, const TileThing &r)
{
    return (l.image_path == r.image_path && l.characters[0] == r.characters[0]
        && l.characters[1] == r.characters[1] && l.front.r == r.front.r && l.front.g == r.front.g
        && l.front.b == r.front.b && l.back.r == r.back.r && l.back.g == r.back.g
        && l.back.b == r.back.b);
}

std::size_t mix(std::size_t x)
{
    std::size_t const m = 0xe9846af9b1a615d;
    x ^= x >> 32;
    x *= m;
    x ^= x >> 32;
    x *= m;
    x ^= x >> 28;
    return x;
}

std::size_t combine(std::size_t seed, std::size_t value)
{
    return mix(seed + 0x9e3779b9 + value);
}

namespace std
{
    template<> struct hash<color_t> {
        size_t operator()(const color_t &val) const
        {
            size_t h1 = hash<::uint8_t>{}(val.r);
            size_t h2 = hash<::uint8_t>{}(val.g);
            size_t h3 = hash<::uint8_t>{}(val.b);
            return combine(h1, combine(h2, h3));
        }
    };

    template<> struct hash<TileThing> {
        size_t operator()(const TileThing &val) const
        {
            size_t h1 = hash<std::string>{}(val.image_path);
            size_t h2 = hash<std::string>{}(std::string(val.characters.data(), 2));
            size_t h3 = hash<color_t>{}(val.front);
            size_t h4 = hash<color_t>{}(val.back);
            return combine(h1, combine(h2, combine(h3, h4)));
        }
    };
} // namespace std

class Sdl2Frontend {
  private:
    SDL_Window *_window;
    SDL_Renderer *_render;
    TTF_Font *_gFont;
    std::unordered_map<TileThing, SDL_Texture *> _tileTextures;

  public:
    Sdl2Frontend();
    ~Sdl2Frontend();
    void init();
    void deinit();
    void renderMap(tile_t **data, uint64_t length, const uint64_t *lengths);
    event_t getEvent();
    SDL_Texture *loadTexture(const std::string &filePath);
};
