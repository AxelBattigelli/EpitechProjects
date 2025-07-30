/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** *
*/

#include "Primitives/TorusBuilder.hpp"
#include "Core/IMaterial.hpp"
#include "Math/Vector3.hpp"

#include <memory>

Raytracer::Primitives::TorusBuilder::TorusBuilder(const libconfig::Setting &contentList)
{
    const Vector3 position(static_cast<double>(contentList[0]["position"]["x"]),
        static_cast<double>(contentList[0]["position"]["y"]),
        static_cast<double>(contentList[0]["position"]["z"]));
    const Vector3 upNormal(static_cast<double>(contentList[0]["upnormal"]["x"]),
        static_cast<double>(contentList[0]["upnormal"]["y"]),
        static_cast<double>(contentList[0]["upnormal"]["z"]));
    double innerRadius = static_cast<double>(contentList[0]["innerradius"]);
    double outerRadius = static_cast<double>(contentList[0]["outerradius"]);
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
        .setInnerRadius(innerRadius)
        .setOuterRadius(outerRadius)
        .setMaterial(material);
}

Raytracer::Primitives::TorusBuilder &Raytracer::Primitives::TorusBuilder::setCenter(Point3 center) 
{
    this->_center = center;
    return *this;
}

Raytracer::Primitives::TorusBuilder &Raytracer::Primitives::TorusBuilder::setUpNormal(Vector3 upNormal)
{
    this->_upNormal = upNormal;
    return *this;
}

Raytracer::Primitives::TorusBuilder &Raytracer::Primitives::TorusBuilder::setInnerRadius(double innerRadius)
{
    this->_innerRadius = innerRadius;
    return *this;
}

Raytracer::Primitives::TorusBuilder &Raytracer::Primitives::TorusBuilder::setOuterRadius(double outerRadius)
{
    this->_outerRadius = outerRadius;
    return *this;
}

Raytracer::Primitives::TorusBuilder &Raytracer::Primitives::TorusBuilder::setMaterial(std::shared_ptr<IMaterial> material)
{
    this->_material = material;
    return *this;
}

Raytracer::Primitives::Torus Raytracer::Primitives::TorusBuilder::build() const 
{
    return Torus(this->_center, this->_upNormal, this->_innerRadius, this->_outerRadius, this->_material);
}
