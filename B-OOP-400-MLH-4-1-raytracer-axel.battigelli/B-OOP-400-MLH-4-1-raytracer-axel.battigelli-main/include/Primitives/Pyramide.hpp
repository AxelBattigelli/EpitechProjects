#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"

#include <memory>
#include <vector>

namespace Raytracer::Primitives
{
    class Pyramide : public IPrimitive {
      public:
        Point3 center;
        Vector3 upNormal;
        double baseLength, baseWidth, height;
        std::shared_ptr<IMaterial> material;

        Pyramide(Point3 center, Vector3 upNormal, double baseLength, double baseWidth, double height,
            std::shared_ptr<IMaterial> material);
        virtual std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override;
        virtual std::optional<AABB> boundingBox() const override;
    };
} // namespace Raytracer::Primitives
