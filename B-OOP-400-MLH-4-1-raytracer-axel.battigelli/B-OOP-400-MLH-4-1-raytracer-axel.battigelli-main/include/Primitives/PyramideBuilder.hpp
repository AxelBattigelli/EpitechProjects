/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** PyramideBuilder
*/

#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"
#include "Pyramide.hpp"
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
    class PyramideBuilder {
        public:
            PyramideBuilder() = default;
            PyramideBuilder(const libconfig::Setting &contentList);
            auto setUpNormal(Vector3 upNormal) -> PyramideBuilder &;
            auto setBaseLength(double baseLength) -> PyramideBuilder &;
            auto setBaseWidth(double _baseWidth) -> PyramideBuilder &;
            auto setHeight(double _height) -> PyramideBuilder &;
            auto setCenter(Point3 center) -> PyramideBuilder &;
            auto setMaterial(std::shared_ptr<IMaterial> material) -> PyramideBuilder &;

            Pyramide build() const;
        private:
            Point3 _center;
            Vector3 _upNormal;
            double _baseLength;
            double _baseWidth;
            double _height;
            std::shared_ptr<IMaterial> _material;
    };
}
