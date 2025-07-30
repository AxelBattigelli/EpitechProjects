#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"

#include <memory>
#include <vector>

namespace Raytracer::Primitives
{
    class Sphere : public IPrimitive {
      public:
        Point3 center;
        double radius;
        std::shared_ptr<IMaterial> material;

        Sphere(const Vector3 &center, double radius, std::shared_ptr<IMaterial> material);
        [[nodiscard]] virtual std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override;
        [[nodiscard]] virtual std::optional<AABB> boundingBox() const override
        {
            return AABB(center - Vector3(radius, radius, radius),
                center + Vector3(radius, radius, radius));
        }
    };
} // namespace Raytracer::Primitives
