/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** *
*/

#include "Primitives/PlaneBuilder.hpp"
#include "Core/IMaterial.hpp"
#include "Math/Vector3.hpp"
#include "Materials/FlatColor.hpp"
#include "Materials/Transparency.hpp"

Raytracer::Primitives::PlaneBuilder::PlaneBuilder(const libconfig::Setting &contentList)
{
    const Vector3 normal(static_cast<double>(contentList[0]["normal"]["x"]),
                static_cast<double>(contentList[0]["normal"]["y"]),
                static_cast<double>(contentList[0]["normal"]["z"]));
            double const position = static_cast<double>(static_cast<int>(contentList[0]["position"]));
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
            } else  if (contentList[0].exists("color")) {
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

    this->setNormal(normal)
        .setPosition(position)
        .setMaterial(material);
}

Raytracer::Primitives::PlaneBuilder &Raytracer::Primitives::PlaneBuilder::setNormal(Point3 normal) 
{
    this->_normal = normal;
    return *this;
}

Raytracer::Primitives::PlaneBuilder &Raytracer::Primitives::PlaneBuilder::setPosition(double position)
{
    this->_position = position;
    return *this;
}

Raytracer::Primitives::PlaneBuilder &Raytracer::Primitives::PlaneBuilder::setMaterial(std::shared_ptr<IMaterial> material)
{
    this->_material = material;
    return *this;
}

Raytracer::Primitives::Plane Raytracer::Primitives::PlaneBuilder::build() const 
{
    return Plane(this->_normal, this->_position, this->_material);
}
