#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"

#include <memory>
#include <vector>

namespace Raytracer::Primitives
{
    class Cone : public IPrimitive {
      public:
        Point3 center;
        Vector3 upNormal;
        double radius, height;
        std::shared_ptr<IMaterial> material;

        Cone(Point3 center, Vector3 upNormal, double radius, double height,
            std::shared_ptr<IMaterial> material);
        [[nodiscard]] virtual std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override;
        [[nodiscard]] virtual std::optional<AABB> boundingBox() const override;
    };
} // namespace Raytracer::Primitives
