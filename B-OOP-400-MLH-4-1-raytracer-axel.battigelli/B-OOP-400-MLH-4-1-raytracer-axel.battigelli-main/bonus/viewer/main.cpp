#include <SFML/Graphics.hpp>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <thread>
#include <chrono>

bool loadPPM(const std::string& filename, sf::Image& image)
{
    std::ifstream file(filename, std::ios::binary);
    if (!file)
        return false;

    std::string format;
    file >> format;

    if (format != "P6") {
        std::cerr << "Unsupported PPM format: " << format << "\n";
        return false;
    }

    int width, height, maxVal;
    file >> width >> height >> maxVal;
    file.ignore();

    std::vector<sf::Uint8> pixels(width * height * 4);

    for (int i = 0; i < width * height; ++i) {
        char rgb[3];
        file.read(rgb, 3);
        pixels[i * 4 + 0] = static_cast<sf::Uint8>(rgb[0]);
        pixels[i * 4 + 1] = static_cast<sf::Uint8>(rgb[1]);
        pixels[i * 4 + 2] = static_cast<sf::Uint8>(rgb[2]);
        pixels[i * 4 + 3] = 255;
    }

    image.create(width, height, pixels.data());
    return true;
}

int main()
{
    const std::string filename = "preview.ppm";
    sf::Image image;

    if (!loadPPM(filename, image)) {
        std::cerr << "Failed to load PPM image.\n";
        return 84;
    }

    sf::Texture texture;
    texture.loadFromImage(image);

    sf::Sprite sprite(texture);
    sf::RenderWindow window(sf::VideoMode(texture.getSize().x, texture.getSize().y), "PPM Viewer");

    auto lastWriteTime = std::filesystem::last_write_time(filename);

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                window.close();
        }

        auto currentWriteTime = std::filesystem::last_write_time(filename);
        if (currentWriteTime != lastWriteTime) {
            lastWriteTime = currentWriteTime;
            if (loadPPM(filename, image)) {
                texture.loadFromImage(image);
                sprite.setTexture(texture, true);
            }
        }

        window.clear();
        window.draw(sprite);
        window.display();

        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
    }

    return 0;
}
