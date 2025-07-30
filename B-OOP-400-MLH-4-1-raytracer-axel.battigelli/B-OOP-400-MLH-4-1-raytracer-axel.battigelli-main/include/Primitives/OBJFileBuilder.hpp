/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** OBJFileBuilder
*/

#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"
#include "OBJFile.hpp"
#include "Exception.hpp"
#include "Materials/FlatColor.hpp"
#include "Materials/Transparency.hpp"
#include "Materials/Mirror.hpp"
#include "Math/Vector3.hpp"
#include <libconfig.h++>
#include "Materials/Mirror.hpp"

#include <memory>
#include <vector>

namespace Raytracer::Primitives
{
    class OBJFileBuilder {
        public:
            OBJFileBuilder() {};
            /*SphereBuilder(std::unorederd_map<std::string, std::shared_ptr<IMaterial>> materials, configobject config) {
                this->setRadius(config.radius)
                    .setCenter(config.center)
                    .setMaterial(materials[config.material]);
            }*/
            OBJFileBuilder(const libconfig::Setting &contentList);
            OBJFileBuilder &setScale(Vector3 scale);
            OBJFileBuilder &setTranslate(Vector3 translate);
            OBJFileBuilder &setFilename(std::string filename);
            OBJFileBuilder &setMaterial(std::shared_ptr<IMaterial> material);

            ObjFile build() const;
        private:
            Vector3 _scale;
            Vector3 _translate;
            std::string _filename;
            std::shared_ptr<IMaterial> _material;

    };
}