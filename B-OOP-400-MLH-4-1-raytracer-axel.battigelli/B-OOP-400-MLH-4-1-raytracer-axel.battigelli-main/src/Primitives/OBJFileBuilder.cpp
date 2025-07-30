/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** OBJFileBuilder
*/

#include "Primitives/OBJFileBuilder.hpp"

Raytracer::Primitives::OBJFileBuilder::OBJFileBuilder(const libconfig::Setting &contentList)
{
    const Vector3 scale(static_cast<double>(contentList[0]["scale"]["x"]),
                static_cast<double>(contentList[0]["scale"]["y"]),
                static_cast<double>(contentList[0]["scale"]["z"]));
    const Vector3 translate(static_cast<double>(contentList[0]["translate"]["x"]),
                static_cast<double>(contentList[0]["translate"]["y"]),
                static_cast<double>(contentList[0]["translate"]["z"]));

    const std::string filename = contentList[0]["filename"];

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

    this->setScale(scale)
        .setTranslate(translate)
        .setFilename(filename)
        .setMaterial(material);

}

Raytracer::Primitives::OBJFileBuilder &Raytracer::Primitives::OBJFileBuilder::setScale(Vector3 scale)
{
    this->_scale = scale;
    return *this;
}

Raytracer::Primitives::OBJFileBuilder &Raytracer::Primitives::OBJFileBuilder::setTranslate(Vector3 translate)
{
    this->_translate = translate;
    return *this;
}

Raytracer::Primitives::OBJFileBuilder &Raytracer::Primitives::OBJFileBuilder::setFilename(std::string filename)
{
    this->_filename = filename;
    return *this;
}

Raytracer::Primitives::OBJFileBuilder &Raytracer::Primitives::OBJFileBuilder::setMaterial(std::shared_ptr<IMaterial> material)
{
    this->_material = material;
    return *this;
}

Raytracer::Primitives::ObjFile Raytracer::Primitives::OBJFileBuilder::build() const
{
    return ObjFile(this->_material, this->_scale, this->_translate, this->_filename);
}
