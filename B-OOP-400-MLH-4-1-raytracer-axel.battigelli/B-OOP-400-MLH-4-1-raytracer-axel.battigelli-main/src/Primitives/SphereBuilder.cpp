/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** SphereBuilder
*/

#include "Primitives/SphereBuilder.hpp"
#include "Core/IMaterial.hpp"
#include "Math/Vector3.hpp"

#include <optional>

Raytracer::Primitives::SphereBuilder::SphereBuilder(const libconfig::Setting &contentList)
{
    const Vector3 position(static_cast<double>(contentList[0]["x"]),
        static_cast<double>(contentList[0]["y"]),
        static_cast<double>(contentList[0]["z"]));
    double radius = static_cast<double>(contentList[0]["r"]);
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

    this->setRadius(radius)
        .setCenter(position)
        .setMaterial(material);
}

Raytracer::Primitives::SphereBuilder &Raytracer::Primitives::SphereBuilder::setRadius(double radius) 
{
    this->_radius = radius;
    return *this;
}

Raytracer::Primitives::SphereBuilder &Raytracer::Primitives::SphereBuilder::setCenter(Point3 center)
{
    this->_center = center;
    return *this;
}

Raytracer::Primitives::SphereBuilder &Raytracer::Primitives::SphereBuilder::setMaterial(std::shared_ptr<IMaterial> material)
{
    this->_material = material;
    return *this;
}

Raytracer::Primitives::Sphere Raytracer::Primitives::SphereBuilder::build() const 
{
    return Sphere(this->_center, this->_radius, this->_material);
}