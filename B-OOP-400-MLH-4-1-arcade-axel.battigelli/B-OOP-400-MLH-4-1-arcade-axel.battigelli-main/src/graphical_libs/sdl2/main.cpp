#include "main.hpp"
#include "Shared/Exception.hpp"
#include "ffi.hpp"
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_ttf.h>
#include <unordered_map>
#include <SDL2/SDL.h>
#include <cerrno>
#include <format>
#include <iostream>

struct frontend_handle {
    Sdl2Frontend inner;
    std::string lastString;
};

Sdl2Frontend::Sdl2Frontend()
{
    std::clog << "Creating the SDL2 frontend!\n";
}

Sdl2Frontend::~Sdl2Frontend()
{
    for (auto &texture : this->_tileTextures) {
        if (texture.second)
            SDL_DestroyTexture(texture.second);
        texture.second = nullptr;
    }
    std::clog << "Deleting the SDL2 frontend!\n";
}

void Sdl2Frontend::init()
{
    std::clog << "Initing the SDL2 frontend!\n";
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        throw arcade::shared::Exception(
            std::format("Error initializing SDL2: {}", SDL_GetError()));
    }
    this->_window =
        SDL_CreateWindow("Arcade", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1300, 1000, 0);
    if (!this->_window) {
        throw arcade::shared::Exception(
            std::format("Error could not create window: {}", SDL_GetError()));
    }
    Uint32 render_flags = SDL_RENDERER_ACCELERATED;
    this->_render = SDL_CreateRenderer(this->_window, -1, render_flags);

    if (TTF_Init() == -1) {
        throw arcade::shared::Exception(
            std::format("Error could not initialize TTF: {}", TTF_GetError()));
    }
    this->_gFont = TTF_OpenFont("./assets/courier.ttf", 14); /// defined our typo
    if (!this->_gFont) {
        throw arcade::shared::Exception(
            std::format("Error could not create font: {}", SDL_GetError()));
    }
}

void Sdl2Frontend::deinit()
{
    SDL_DestroyRenderer(this->_render);
    SDL_DestroyWindow(this->_window);
    TTF_Quit();
    IMG_Quit();
    SDL_Quit();
    std::clog << "Deiniting the SDL2 frontend!\n";
}

int color(uint8_t color)
{
    return (int) (((double) color) * 1000.0 / 256.0);
}

SDL_Texture *Sdl2Frontend::loadTexture(const std::string &filePath)
{
    SDL_Surface *surface = IMG_Load(filePath.c_str());
    if (!surface) {
        SDL_Log("Fail to load image %s: %s", filePath.c_str(), SDL_GetError());
        return nullptr;
    }
    SDL_Texture *texture = SDL_CreateTextureFromSurface(this->_render, surface);
    SDL_FreeSurface(surface);
    if (!texture) {
        SDL_Log("Fail to create texture: %s", SDL_GetError());
    }
    return texture;
}

void Sdl2Frontend::renderMap(tile_t **data, uint64_t length, const uint64_t *lengths)
{
    int size_tile = 16;

    SDL_SetRenderDrawColor(this->_render, 0, 0, 0, 255);
    SDL_RenderClear(this->_render);
    for (uint64_t y = 0; y < length; ++y) {
        for (uint64_t x = 0; x < lengths[y]; ++x) {
            auto tile = data[y][x];
            int posX = x * size_tile;
            int posY = y * size_tile;

            if (!this->_tileTextures.contains(tile)) {
                if (tile.image_path.ptr && tile.image_path.len > 0) {
                    this->_tileTextures.insert_or_assign(
                        tile, loadTexture(std::string(tile.image_path.ptr, tile.image_path.len)));
                } else {
                    char text[2] = {};
                    SDL_Texture *target = SDL_CreateTexture(this->_render,
                        SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_TARGET, size_tile, size_tile);
                    for (int i = 0; i < 2; ++i) {
                        text[0] = tile.characters[i];

                        SDL_Color textColor = {tile.front.r, tile.front.g, tile.front.b, 255};
                        SDL_Surface *textSurface =
                            TTF_RenderUTF8_Solid(this->_gFont, text, textColor);
                        if (!textSurface) {
                            SDL_DestroyTexture(target);
                            SDL_Log("Error with TTF_RenderText_Solid: %s", TTF_GetError());
                            break;
                        }

                        SDL_Texture *textTexture =
                            SDL_CreateTextureFromSurface(this->_render, textSurface);
                        SDL_FreeSurface(textSurface);
                        if (!textTexture) {
                            SDL_DestroyTexture(target);
                            SDL_Log("Error with SDL_CreateTextureFromSurface: %s", SDL_GetError());
                            break;
                        }

                        int textWidth = 0, textHeight = 0;
                        SDL_QueryTexture(textTexture, NULL, NULL, &textWidth, &textHeight);

                        SDL_Rect backgroundRect = {
                            (i * (int) (((double) size_tile) / 2.0)), 0, 8, 16};
                        SDL_SetRenderTarget(this->_render, target);
                        SDL_SetRenderDrawColor(
                            this->_render, tile.back.r, tile.back.g, tile.back.b, 255);
                        SDL_RenderFillRect(this->_render, &backgroundRect);

                        SDL_Rect renderQuad = {
                            (i * (int) (((double) size_tile) / 2.0)), 0, textWidth, textHeight};
                        SDL_RenderCopy(this->_render, textTexture, NULL, &renderQuad);
                        SDL_SetRenderTarget(this->_render, NULL);

                        if (textTexture != NULL) {
                            SDL_DestroyTexture(textTexture);
                            textTexture = NULL;
                        }
                        this->_tileTextures.insert_or_assign(tile, target);
                    }
                }
            }
            if (this->_tileTextures.find(tile) != this->_tileTextures.end()) {
                SDL_Texture *texture = this->_tileTextures[tile];
                SDL_Rect destRect = {posX, posY, size_tile, size_tile};
                SDL_RenderCopy(this->_render, texture, NULL, &destRect);
            }
        }
    }
    SDL_RenderPresent(this->_render);
}

event_t Sdl2Frontend::getEvent()
{
    int size_tile = 16;
    SDL_Event evt = {};
    for (;;) {
        if (SDL_PollEvent(&evt) == 0) {
            return event_t{.other = event_other_t{.type = EVT_OTHER}};
        }
        if (evt.type == SDL_MOUSEBUTTONUP) {
            int x, y;

            SDL_GetMouseState(&x, &y);
            if (evt.button.button == SDL_BUTTON_LEFT) {
                return event_t{
                    .mouse = event_mouse_t{.type = EVT_MOUSE,
                        .position{.x = (uint64_t) x % size_tile, .y = (uint64_t) y % size_tile},
                        .button = MOUSE_LEFT}};
            } else if (evt.button.button == SDL_BUTTON_RIGHT) {
                return event_t{
                    .mouse = event_mouse_t{.type = EVT_MOUSE,
                        .position{.x = (uint64_t) x % size_tile, .y = (uint64_t) y % size_tile},
                        .button = MOUSE_RIGHT}};
            } else if (evt.button.button == SDL_BUTTON_MIDDLE) {
                return event_t{
                    .mouse = event_mouse_t{.type = EVT_MOUSE,
                        .position{.x = (uint64_t) x % size_tile, .y = (uint64_t) y % size_tile},
                        .button = MOUSE_CENTER}};
            }
        }

        if (evt.type == SDL_KEYUP) {
            switch (evt.key.keysym.sym) {
                case SDLK_UP:
                    return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_UP}};
                case SDLK_DOWN:
                    return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_DOWN}};
                case SDLK_LEFT:
                    return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_LEFT}};
                case SDLK_RIGHT:
                    return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_RIGHT}};
                case SDLK_ESCAPE:
                    return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_ESCAPE}};
                case SDLK_RETURN:
                case SDLK_KP_ENTER:
                    return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_ENTER}};
                case SDLK_SPACE:
                    return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_SPACE}};
                case SDLK_TAB:
                    return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_TAB}};
                case SDLK_BACKSPACE:
                    return event_t{
                        .keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_BACKSPACE}};
                case SDLK_DELETE:
                    return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_DELETE}};
                default:
                    if (evt.key.keysym.sym >= SDLK_EXCLAIM && evt.key.keysym.sym <= SDLK_z) {
                        bool isUppercase = false;

                        if (evt.key.keysym.mod & KMOD_SHIFT) {
                            isUppercase = true;
                        }
                        if (evt.key.keysym.mod & KMOD_CAPS) {
                            isUppercase = !isUppercase;
                        }
                        if (isUppercase && evt.key.keysym.sym >= SDLK_a
                            && evt.key.keysym.sym <= SDLK_z) {
                            return event_t{.keyboard = event_keyboard_t{.type = EVT_KB,
                                               .key = (key_e) (evt.key.keysym.sym - ('a' - 'A'))}};
                        } else {
                            return event_t{.keyboard = event_keyboard_t{
                                               .type = EVT_KB, .key = (key_e) evt.key.keysym.sym}};
                        }
                    } else {
                        return event_t{
                            .keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_UNKNOWN}};
                    }
            }
        }
        if (evt.type != SDL_MOUSEMOTION) {
            return event_t{.other = event_other_t{.type = EVT_OTHER}};
        }
    }
}

extern "C"
{
    extern const lib_info_t ARCADE_AACMMN_LIB_INFO = {.magic = LIB_MAGIC_VALUE,
        .api_version = LIB_API_VERSION,
        .type = "FRONTEND",
        .name = {.ptr = "SDL2 (AA)", .len = 9},
        .id = {.ptr = "eu.epitech.anicetaxel.sdl2", .len = 26}};

    void frontend_destroyInstance(frontend_handle_t *hdl)
    {
        try {
            delete hdl;
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    }

    frontend_handle_t *frontend_getInstance()
    {
        try {
            return new frontend_handle_t(Sdl2Frontend());
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    }

    void frontend_init(frontend_handle_t *hdl)
    {
        try {
            hdl->inner.init();
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    };

    void frontend_deinit(frontend_handle_t *hdl)
    {
        try {
            hdl->inner.deinit();
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    }

    void frontend_renderMap(
        frontend_handle_t *hdl, tile_t **data, uint64_t length, const uint64_t *lengths)
    {
        try {
            hdl->inner.renderMap(data, length, lengths);
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    }

    event_t frontend_getEvent(frontend_handle_t *hdl)
    {
        try {
            return hdl->inner.getEvent();
        } catch (const arcade::shared::Exception &e) {
            std::cerr << "Error: " << e << '\n';
            std::exit(84);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what()
                      << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
            std::exit(84);
        } catch (...) {
            std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                      << std::current_exception().__cxa_exception_type()->name() << '\n';
            std::exit(84);
        }
    }
}

#if 0
    // SDL_RenderClear(this->_render);
    // SDL_RenderPresent(this->_render);
    // SDL_Delay(1000 / 60);

    SDL_Event event;

    while (SDL_PollEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                // handling of close button
                break;

            case SDL_KEYDOWN:
                // keyboard API for key pressed
                switch (event.key.keysym.scancode) {
                    case SDL_SCANCODE_Z:
                    case SDL_SCANCODE_UP:
                        std::clog << "z pressed\n";
                        break;
                    case SDL_SCANCODE_A:
                    case SDL_SCANCODE_LEFT:
                        break;
                    case SDL_SCANCODE_S:
                    case SDL_SCANCODE_DOWN:
                        break;
                    case SDL_SCANCODE_D:
                    case SDL_SCANCODE_RIGHT:
                        break;
                    default:
                        break;
                }
        }
    }
    // int cha = getch();
    // if (cha == ERR)
    //     return arcade::shared::base::frontend::event::Timeout();
    // if (cha == KEY_MOUSE)
    //     return arcade::shared::base::frontend::event::Mouse();
    // switch (cha) {
    //     case KEY_UP:
    //     case 'z':
    //         return
    //         arcade::shared::base::frontend::event::Keyboard(arcade::shared::base::frontend::event::KB_UP);
    //     case KEY_DOWN:
    //     case 's':
    //         return
    //         arcade::shared::base::frontend::event::Keyboard(arcade::shared::base::frontend::event::KB_DOWN);
    //     case KEY_LEFT:
    //     case 'q':
    //         return
    //         arcade::shared::base::frontend::event::Keyboard(arcade::shared::base::frontend::event::KB_LEFT);
    //     case KEY_RIGHT:
    //     case 'd':
    //         return
    //         arcade::shared::base::frontend::event::Keyboard(arcade::shared::base::frontend::event::KB_RIGHT);
    //     case 27:
    //         return
    //         arcade::shared::base::frontend::event::Keyboard(arcade::shared::base::frontend::event::KB_ESCAPE);
    //     case '\r':
    //     case '\n':
    //         return
    //         arcade::shared::base::frontend::event::Keyboard(arcade::shared::base::frontend::event::KB_ENTER);
    //     case ' ':
    //         return
    //         arcade::shared::base::frontend::event::Keyboard(arcade::shared::base::frontend::event::KB_SPACE);
    //     default:
    //         if (cha > 255 || cha < 0) {
    //             return arcade::shared::base::frontend::event::Quit();
    //         }
    // }
    // return arcade::shared::base::frontend::event::Keyboard(cha);

#endif