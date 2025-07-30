/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** BoxeBuilder
*/

#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"
#include "Boxe.hpp"
#include "Exception.hpp"
#include "Materials/FlatColor.hpp"
#include "Materials/Transparency.hpp"
#include "Materials/Mirror.hpp"
#include "Math/Vector3.hpp"
#include <libconfig.h++>

#include <memory>
#include <vector>

namespace Raytracer::Primitives
{
    class BoxeBuilder {
        public:
            BoxeBuilder() {};
            /*SphereBuilder(std::unorederd_map<std::string, std::shared_ptr<IMaterial>> materials, configobject config) {
                this->setRadius(config.radius)
                    .setCenter(config.center)
                    .setMaterial(materials[config.material]);
            }*/
            BoxeBuilder(const libconfig::Setting &contentList);
            BoxeBuilder &setUpNormal(Vector3 upNormal);
            BoxeBuilder &setRightNormal(Vector3 rightNormal);
            BoxeBuilder &setSize(Vector3 size);
            BoxeBuilder &setCenter(Point3 center);
            BoxeBuilder &setMaterial(std::shared_ptr<IMaterial> material);

            Boxe build() const;
        private:
            Point3 _center;
            Vector3 _upNormal;
            Vector3 _rightNormal;
            Vector3 _size;
            std::shared_ptr<IMaterial> _material;

    };
}

