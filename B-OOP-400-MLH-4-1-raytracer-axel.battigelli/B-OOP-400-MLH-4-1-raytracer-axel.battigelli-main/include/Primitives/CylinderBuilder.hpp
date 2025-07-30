/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** CylinderBuilder
*/

#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"
#include "Cylinder.hpp"
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
    class CylinderBuilder {
        public:
            CylinderBuilder() = default;
            CylinderBuilder(const libconfig::Setting &contentList);
            auto setUpNormal(Vector3 upNormal) -> CylinderBuilder &;
            auto setLength(double length) -> CylinderBuilder &;
            auto setRadius(double radius) -> CylinderBuilder &;
            auto setCenter(Point3 center) -> CylinderBuilder &;
            auto setMaterial(std::shared_ptr<IMaterial> material) -> CylinderBuilder &;

            [[nodiscard]] auto build() const -> Cylinder;
        private:
            Point3 _center;
            Vector3 _upNormal;
            double _length;
            double _radius;
            std::shared_ptr<IMaterial> _material;
    };
}

