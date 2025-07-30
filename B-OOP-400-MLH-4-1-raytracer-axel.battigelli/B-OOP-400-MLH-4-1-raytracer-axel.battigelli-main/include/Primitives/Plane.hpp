#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"

#include <memory>
#include <vector>

namespace Raytracer::Primitives
{
    class Plane : public IPrimitive {
      public:
        Point3 normal;
        double position;
        std::shared_ptr<IMaterial> material;

        Plane(const Vector3 &normal, double position, std::shared_ptr<IMaterial> material);
        [[nodiscard]] virtual std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override;
        [[nodiscard]] virtual std::optional<AABB> boundingBox() const override { return std::nullopt; }
    };
} // namespace Raytracer::Primitives
