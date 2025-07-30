/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** ConeBuilder
*/

#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"
#include "Cone.hpp"
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
    class ConeBuilder {
        public:
            ConeBuilder() = default;
            ConeBuilder(const libconfig::Setting &contentList);
            auto setUpNormal(Vector3 upNormal) -> ConeBuilder &;
            auto setLength(double length) -> ConeBuilder &;
            auto setRadius(double radius) -> ConeBuilder &;
            auto setCenter(Point3 center) -> ConeBuilder &;
            auto setMaterial(std::shared_ptr<IMaterial> material) -> ConeBuilder &;

            [[nodiscard]] auto build() const -> Cone;
        private:
            Point3 _center;
            Vector3 _upNormal;
            double _length;
            double _radius;
            std::shared_ptr<IMaterial> _material;
    };
}

