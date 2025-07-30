/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** TorusBuilder
*/

#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"
#include "Torus.hpp"
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
    class TorusBuilder {
        public:
            TorusBuilder() = default;
            TorusBuilder(const libconfig::Setting &contentList);
            auto setUpNormal(Vector3 upNormal) -> TorusBuilder &;
            auto setOuterRadius(double innerRadius) -> TorusBuilder &;
            auto setInnerRadius(double outerRadius) -> TorusBuilder &;
            auto setCenter(Point3 center) -> TorusBuilder &;
            auto setMaterial(std::shared_ptr<IMaterial> material) -> TorusBuilder &;

            [[nodiscard]] auto build() const -> Torus;
        private:
            Point3 _center;
            Vector3 _upNormal;
            double _innerRadius;
            double _outerRadius;
            std::shared_ptr<IMaterial> _material;
    };
}

