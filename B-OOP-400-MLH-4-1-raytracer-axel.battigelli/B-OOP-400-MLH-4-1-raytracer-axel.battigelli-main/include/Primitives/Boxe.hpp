#pragma once

#include "Core/IMaterial.hpp"
#include "Core/IPrimitive.hpp"

#include <memory>
#include <vector>

namespace Raytracer::Primitives
{
    class Boxe : public IPrimitive {
      public:
        Point3 center;
        Vector3 upNormal;
        Vector3 rightNormal;
        Vector3 size;
        std::shared_ptr<IMaterial> material;

        Boxe(Point3 center, Vector3 upNormal, Vector3 rightNormal, Vector3 size,
            std::shared_ptr<IMaterial> material);
        [[nodiscard]] virtual std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override;
        [[nodiscard]] virtual std::optional<AABB> boundingBox() const override;

        private:
        AABB _boundingBox;
        Vector3 _forwardNormal;

    };
} // namespace Raytracer::Primitives
