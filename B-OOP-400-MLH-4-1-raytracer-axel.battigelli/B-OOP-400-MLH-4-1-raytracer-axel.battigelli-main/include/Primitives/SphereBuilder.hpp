/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** SphereBuilder
*/

#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"
#include "Sphere.hpp"
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
    class SphereBuilder {
        public:
            SphereBuilder() {};
            /*SphereBuilder(std::unorederd_map<std::string, std::shared_ptr<IMaterial>> materials, configobject config) {
                this->setRadius(config.radius)
                    .setCenter(config.center)
                    .setMaterial(materials[config.material]);
            }*/
            SphereBuilder(const libconfig::Setting &contentList);
            SphereBuilder &setRadius(double radius);
            SphereBuilder &setCenter(Point3 center);
            SphereBuilder &setMaterial(std::shared_ptr<IMaterial> material);

            Sphere build() const;
        private:
            Point3 _center;
            double _radius;
            std::shared_ptr<IMaterial> _material;

    };
}

