#include "main.hpp"
#include "Shared/Exception.hpp"
#include "ffi.hpp"
#include <unordered_map>
#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <cerrno>
#include <iostream>

struct frontend_handle {
    SfmlFrontend inner;
    std::string lastString;
};

SfmlFrontend::SfmlFrontend()
{
    std::clog << "Creating the SFML frontend!\n";
}

SfmlFrontend::~SfmlFrontend()
{
    std::clog << "Deleting the SFML frontend!\n";
}

void SfmlFrontend::init()
{
    std::clog << "Initing the SFML frontend!\n";
    _window = new sf::RenderWindow(sf::VideoMode(1300, 1000), "Arcade");

    if (!_window->isOpen()) {
        throw arcade::shared::Exception("Error creating SFML window");
    }
    this->_gfont = new sf::Font();
    if (!this->_gfont->loadFromFile("./assets/courier.ttf")) {
        throw arcade::shared::Exception("Error could not create font");
    }
}

void SfmlFrontend::deinit()
{
    delete _gfont;
    if (_window && _window->isOpen()) {
        _window->close();
    }
    std::clog << "Deiniting the SFML frontend!\n";
}

int color(uint8_t color)
{
    return (int) (((double) color) * 1000.0 / 256.0);
}

sf::Texture *SfmlFrontend::loadTexture(const std::string &filePath)
{
    sf::Image image;
    if (!image.loadFromFile(filePath)) {
        std::cerr << "Error loading image from path: " << filePath << std::endl;
        return nullptr;
    }

    sf::Image resizedImage;
    resizedImage.create(16, 16);

    sf::Vector2u originalSize = image.getSize();
    for (unsigned int y = 0; y < 16; ++y) {
        for (unsigned int x = 0; x < 16; ++x) {
            unsigned int srcX = x * originalSize.x / 16;
            unsigned int srcY = y * originalSize.y / 16;
            resizedImage.setPixel(x, y, image.getPixel(srcX, srcY));
        }
    }

    sf::Texture *texture = new sf::Texture();
    if (!texture->loadFromImage(resizedImage)) {
        std::cerr << "Error creating texture from resized image." << std::endl;
        delete texture;
        return nullptr;
    }

    return texture;
}

void SfmlFrontend::renderMap(tile_t **data, uint64_t length, const uint64_t *lengths)
{
    std::unordered_map<std::string, sf::Texture *> tileTextures;
    int size_tile = 16;

    this->_window->clear(sf::Color::Black);

    for (uint64_t y = 0; y < length; ++y) {
        for (uint64_t x = 0; x < lengths[y]; ++x) {
            auto tile = data[y][x];
            int posX = x * size_tile;
            int posY = y * size_tile;

            if (tile.image_path.ptr && tile.image_path.len > 0) {
                if (!tileTextures.contains(
                        std::string(tile.image_path.ptr, tile.image_path.len))) {
                    tileTextures.insert_or_assign(
                        std::string(tile.image_path.ptr, tile.image_path.len),
                        loadTexture(std::string(tile.image_path.ptr, tile.image_path.len)));
                }

                if (tileTextures.find(std::string(tile.image_path.ptr, tile.image_path.len))
                    != tileTextures.end()) {
                    sf::Texture *texture =
                        tileTextures[std::string(tile.image_path.ptr, tile.image_path.len)];
                    sf::Sprite sprite(*texture);
                    sprite.setPosition(posX, posY);
                    this->_window->draw(sprite);
                }

            } else {
                char text[3] = {};
                text[0] = tile.characters[0];
                text[1] = tile.characters[1];
                text[2] = '\0';

                sf::Color textColor = {tile.front.r, tile.front.g, tile.front.b, 255};
                sf::Text textSurface;
                textSurface.setFont(*this->_gfont);
                textSurface.setString(text);
                textSurface.setCharacterSize(14);
                textSurface.setPosition(posX, posY);
                textSurface.setFillColor(textColor);

                float totalWidth = textSurface.getLocalBounds().width;
                sf::Color backColor = {tile.back.r, tile.back.g, tile.back.b, 255};
                sf::RectangleShape rectangle(sf::Vector2f(totalWidth + 1, 16));
                rectangle.setFillColor(backColor);
                rectangle.setPosition(posX, posY);
                this->_window->draw(rectangle);
                this->_window->draw(textSurface);
            }
        }
    }
    this->_window->display();
}

event_t SfmlFrontend::getEvent()
{
    int size_tile = 16;
    sf::Event evt;
    if (!this->_window->pollEvent(evt))
        return event_t{.other = event_other_t{.type = EVT_OTHER}};

    if (evt.type == sf::Event::MouseButtonReleased) {
        sf::Vector2i position = sf::Mouse::getPosition(*this->_window);
        int x = position.x;
        int y = position.y;

        if (evt.mouseButton.button == sf::Mouse::Left) {
            return event_t{
                .mouse = event_mouse_t{.type = EVT_MOUSE,
                    .position{.x = (uint64_t) x % size_tile, .y = (uint64_t) y % size_tile},
                    .button = MOUSE_LEFT}};
        } else if (evt.mouseButton.button == sf::Mouse::Right) {
            return event_t{
                .mouse = event_mouse_t{.type = EVT_MOUSE,
                    .position{.x = (uint64_t) x % size_tile, .y = (uint64_t) y % size_tile},
                    .button = MOUSE_RIGHT}};
        } else if (evt.mouseButton.button == sf::Mouse::Middle) {
            return event_t{
                .mouse = event_mouse_t{.type = EVT_MOUSE,
                    .position{.x = (uint64_t) x % size_tile, .y = (uint64_t) y % size_tile},
                    .button = MOUSE_CENTER}};
        }
    }

    if (evt.type == sf::Event::KeyReleased) {
        switch (evt.key.code) {
            case sf::Keyboard::Up:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_UP}};
            case sf::Keyboard::Down:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_DOWN}};
            case sf::Keyboard::Left:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_LEFT}};
            case sf::Keyboard::Right:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_RIGHT}};
            case sf::Keyboard::Escape:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_ESCAPE}};
            case sf::Keyboard::Enter:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_ENTER}};
            case sf::Keyboard::Space:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_SPACE}};
            case sf::Keyboard::Tab:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_TAB}};
            case sf::Keyboard::BackSpace:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_BACKSPACE}};
            case sf::Keyboard::Delete:
                return event_t{.keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_DELETE}};
            default:
                if (evt.key.code >= sf::Keyboard::A && evt.key.code <= sf::Keyboard::Z) {
                    bool isUppercase = false;

                    if (sf::Keyboard::isKeyPressed(sf::Keyboard::LShift)
                        || sf::Keyboard::isKeyPressed(sf::Keyboard::RShift)) {
                        isUppercase = true;
                    }

                    // if (sf::Keyboard::isKeyPressed(sf::Keyboard::CapsLock)) {
                    //     isUppercase = !isUppercase;
                    // }
                    if (isUppercase) {
                        return event_t{.keyboard = event_keyboard_t{.type = EVT_KB,
                                           .key = (key_e) (evt.key.code - sf::Keyboard::A + 'A')}};
                    } else {
                        return event_t{.keyboard = event_keyboard_t{.type = EVT_KB,
                                           .key = (key_e) (evt.key.code + 'a' - sf::Keyboard::A)}};
                    }
                } else {
                    return event_t{
                        .keyboard = event_keyboard_t{.type = EVT_KB, .key = KB_UNKNOWN}};
                }
        }
    }
    return event_t{.other = event_other_t{.type = EVT_OTHER}};
}

extern "C"
{
    extern const lib_info_t ARCADE_AACMMN_LIB_INFO = {.magic = LIB_MAGIC_VALUE,
        .api_version = LIB_API_VERSION,
        .type = "FRONTEND",
        .name = {.ptr = "SFML (AA)", .len = 9},
        .id = {.ptr = "eu.epitech.anicetaxel.sfml", .len = 26}};

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
            return new frontend_handle_t(SfmlFrontend());
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
