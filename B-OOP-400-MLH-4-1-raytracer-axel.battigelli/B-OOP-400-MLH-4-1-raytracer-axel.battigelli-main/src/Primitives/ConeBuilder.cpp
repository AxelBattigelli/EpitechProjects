/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** *
*/

#include "Primitives/ConeBuilder.hpp"
#include "Core/IMaterial.hpp"
#include "Math/Vector3.hpp"

#include <memory>
#include <iostream>

Raytracer::Primitives::ConeBuilder::ConeBuilder(const libconfig::Setting &contentList)
{
    const Vector3 position(static_cast<double>(contentList[0]["position"]["x"]),
        static_cast<double>(contentList[0]["position"]["y"]),
        static_cast<double>(contentList[0]["position"]["z"]));
    const Vector3 upNormal(static_cast<double>(contentList[0]["upnormal"]["x"]),
        static_cast<double>(contentList[0]["upnormal"]["y"]),
        static_cast<double>(contentList[0]["upnormal"]["z"]));
    double radius = static_cast<double>(contentList[0]["radius"]);
    double length = 1e153; // infinity : max val supported by code
    if (contentList[0].exists("length"))
        length = static_cast<double>(contentList[0]["length"]);
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
    } else if (contentList[0].exists("transparency")) {
        const Color color(
            static_cast<double>(static_cast<int>(contentList[0]["transparency"]["r"]))
            / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["transparency"]["g"]))
            / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["transparency"]["b"]))
            / 255.0);
        material = std::make_shared<Raytracer::Materials::Transparency>(color);
    } else if (contentList[0].exists("color")) {
        const Color color(
            static_cast<double>(static_cast<int>(contentList[0]["color"]["r"])) / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["color"]["g"])) / 255.0,
            static_cast<double>(static_cast<int>(contentList[0]["color"]["b"])) / 255.0);
        material = std::make_shared<Raytracer::Materials::FlatColor>(color);
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
        .setUpNormal(upNormal)
        .setLength(length)
        .setRadius(radius)
        .setMaterial(material);
}

Raytracer::Primitives::ConeBuilder &Raytracer::Primitives::ConeBuilder::setCenter(Point3 center) 
{
    this->_center = center;
    return *this;
}

Raytracer::Primitives::ConeBuilder &Raytracer::Primitives::ConeBuilder::setUpNormal(Vector3 upNormal)
{
    this->_upNormal = upNormal;
    return *this;
}

Raytracer::Primitives::ConeBuilder &Raytracer::Primitives::ConeBuilder::setLength(double length)
{
    this->_length = length;
    return *this;
}

Raytracer::Primitives::ConeBuilder &Raytracer::Primitives::ConeBuilder::setRadius(double radius)
{
    this->_radius = radius;
    return *this;
}

Raytracer::Primitives::ConeBuilder &Raytracer::Primitives::ConeBuilder::setMaterial(std::shared_ptr<IMaterial> material)
{
    this->_material = material;
    return *this;
}

Raytracer::Primitives::Cone Raytracer::Primitives::ConeBuilder::build() const 
{
    return Cone(this->_center, this->_upNormal, this->_length, this->_radius, this->_material);
}
