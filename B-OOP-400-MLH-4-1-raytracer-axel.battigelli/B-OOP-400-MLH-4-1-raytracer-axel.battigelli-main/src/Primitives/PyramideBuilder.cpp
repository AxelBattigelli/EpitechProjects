/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** PyramideBuilder
*/

#include "Primitives/PyramideBuilder.hpp"
#include "Core/IMaterial.hpp"
#include "Math/Vector3.hpp"

#include <optional>

Raytracer::Primitives::PyramideBuilder::PyramideBuilder(const libconfig::Setting &contentList)
{
    const Vector3 position(static_cast<double>(contentList[0]["x"]),
        static_cast<double>(contentList[0]["y"]),
        static_cast<double>(contentList[0]["z"]));
    const Vector3 upNormal(static_cast<double>(contentList[0]["x"]),
        static_cast<double>(contentList[0]["y"]),
        static_cast<double>(contentList[0]["z"]));
    const double baseLenght = static_cast<double>(contentList[0]["baselength"]);
    const double baseWidth = static_cast<double>(contentList[0]["basewidth"]);
    const double height = static_cast<double>(contentList[0]["height"]);
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

    this->setBaseLength(baseLenght)
        .setBaseWidth(baseWidth)
        .setHeight(height)
        .setCenter(position)
        .setUpNormal(upNormal)
        .setMaterial(material);
}

Raytracer::Primitives::PyramideBuilder &Raytracer::Primitives::PyramideBuilder::setCenter(Point3 center)
{
    this->_center = center;
    return *this;
}

Raytracer::Primitives::PyramideBuilder &Raytracer::Primitives::PyramideBuilder::setUpNormal(Point3 upNormal)
{
    this->_upNormal = upNormal;
    return *this;
}

Raytracer::Primitives::PyramideBuilder &Raytracer::Primitives::PyramideBuilder::setBaseLength(double baseLenght) 
{
    this->_baseLength = baseLenght;
    return *this;
}

Raytracer::Primitives::PyramideBuilder &Raytracer::Primitives::PyramideBuilder::setBaseWidth(double baseWidth) 
{
    this->_baseWidth = baseWidth;
    return *this;
}

Raytracer::Primitives::PyramideBuilder &Raytracer::Primitives::PyramideBuilder::setHeight(double height) 
{
    this->_height = height;
    return *this;
}

Raytracer::Primitives::PyramideBuilder &Raytracer::Primitives::PyramideBuilder::setMaterial(std::shared_ptr<IMaterial> material)
{
    this->_material = material;
    return *this;
}

Raytracer::Primitives::Pyramide Raytracer::Primitives::PyramideBuilder::build() const 
{
    return Pyramide(this->_center, this->_upNormal, this->_baseLength, this->_baseWidth, this->_height, this->_material);
}
