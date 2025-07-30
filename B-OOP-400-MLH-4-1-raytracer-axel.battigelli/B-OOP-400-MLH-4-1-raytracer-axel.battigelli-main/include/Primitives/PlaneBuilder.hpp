/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** PlaneBuilder
*/

#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"
#include "Plane.hpp"
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
    class PlaneBuilder {
        public:
            PlaneBuilder() {};
            /*SphereBuilder(std::unorederd_map<std::string, std::shared_ptr<IMaterial>> materials, configobject config) {
                this->setRadius(config.radius)
                    .setCenter(config.center)
                    .setMaterial(materials[config.material]);
            }*/
            PlaneBuilder(const libconfig::Setting &contentList);
            PlaneBuilder &setPosition(double position);
            PlaneBuilder &setNormal(Point3 center);
            PlaneBuilder &setMaterial(std::shared_ptr<IMaterial> material);

            Plane build() const;
        private:
            Point3 _normal;
            double _position;
            std::shared_ptr<IMaterial> _material;

    };
}

