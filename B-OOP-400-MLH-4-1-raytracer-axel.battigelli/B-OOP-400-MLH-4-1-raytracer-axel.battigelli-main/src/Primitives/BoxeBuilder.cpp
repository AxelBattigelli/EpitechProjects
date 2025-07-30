/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** *
*/

#include "Primitives/BoxeBuilder.hpp"
#include "Math/Vector3.hpp"

#include <memory>

Raytracer::Primitives::BoxeBuilder::BoxeBuilder(const libconfig::Setting &contentList)
{
    const Vector3 position(static_cast<double>(contentList[0]["position"]["x"]),
                static_cast<double>(contentList[0]["position"]["y"]),
                static_cast<double>(contentList[0]["position"]["z"]));
    const Vector3 size(static_cast<double>(contentList[0]["size"]["width"]),
                static_cast<double>(contentList[0]["size"]["height"]),
                static_cast<double>(contentList[0]["size"]["depth"]));

    const Vector3 upnormal(static_cast<double>(contentList[0]["upnormal"]["x"]),
                static_cast<double>(contentList[0]["upnormal"]["y"]),
                static_cast<double>(contentList[0]["upnormal"]["z"]));
    
    const Vector3 rightnormal(static_cast<double>(contentList[0]["rightnormal"]["x"]),
                static_cast<double>(contentList[0]["rightnormal"]["y"]),
                static_cast<double>(contentList[0]["rightnormal"]["z"]));

    std::shared_ptr<Raytracer::IMaterial> material;
    if (contentList[0].exists("emission")) {
        std::optional<Color> color = std::nullopt;
        if (contentList[0].exists("color")) {
            color = {static_cast<double>(static_cast<int>(contentList[0]["color"]["r"]))
                / 255.0,
                static_cast<double>(static_cast<int>(contentList[0]["color"]["g"]))
                / 255.0,
                static_cast<double>(static_cast<int>(contentList[0]["color"]["b"]))
                / 255.0};
        }
        const Color emitted(
            static_cast<double>(static_cast<int>(contentList[0]["emission"]["r"])) / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["emission"]["g"])) / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["emission"]["b"]))
                / 255.0);
        material = std::make_shared<Raytracer::Materials::FlatColor>(color, emitted);
    } else if (contentList[0].exists("color")) {
        const Vector3 color(
            static_cast<double>(static_cast<int>(contentList[0]["color"]["r"])) / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["color"]["g"])) / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["color"]["b"])) / 255.0);
        material = std::make_shared<Raytracer::Materials::FlatColor>(color);
    } else if (contentList[0].exists("transparency")) {
        const Color color(
            static_cast<double>(static_cast<int>(contentList[0]["transparency"]["r"]))
                / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["transparency"]["g"]))
                / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["transparency"]["b"]))
                / 255.0);
        material = std::make_shared<Raytracer::Materials::Transparency>(color);
    } else if (contentList[0].exists("mirror")) {
        const Color color(
            static_cast<double>(static_cast<int>(contentList[0]["mirror"]["r"]))
                / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["mirror"]["g"]))
                / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["mirror"]["b"]))
                / 255.0);
        const double fuzz = contentList[0]["mirror"]["fuzz"];
        material = std::make_shared<Raytracer::Materials::Mirror>(color, fuzz);
    } else {
        throw Exception("Material not create");
    }

    this->setCenter(position)
        .setMaterial(material)
        .setSize(size)
        .setRightNormal(rightnormal)
        .setUpNormal(upnormal);
}

Raytracer::Primitives::BoxeBuilder &Raytracer::Primitives::BoxeBuilder::setCenter(Point3 center) 
{
    this->_center = center;
    return *this;
}

Raytracer::Primitives::BoxeBuilder &Raytracer::Primitives::BoxeBuilder::setUpNormal(Vector3 upNormal)
{
    this->_upNormal = upNormal;
    return *this;
}

Raytracer::Primitives::BoxeBuilder &Raytracer::Primitives::BoxeBuilder::setRightNormal(Vector3 rightNormal)
{
    this->_rightNormal = rightNormal;
    return *this;
}

Raytracer::Primitives::BoxeBuilder &Raytracer::Primitives::BoxeBuilder::setSize(Vector3 size)
{
    this->_size = size;
    return *this;
}

Raytracer::Primitives::BoxeBuilder &Raytracer::Primitives::BoxeBuilder::setMaterial(std::shared_ptr<IMaterial> material)
{
    this->_material = material;
    return *this;
}

Raytracer::Primitives::Boxe Raytracer::Primitives::BoxeBuilder::build() const 
{
    return {Boxe(this->_center, this->_upNormal, this->_rightNormal, this->_size, this->_material)};
}
