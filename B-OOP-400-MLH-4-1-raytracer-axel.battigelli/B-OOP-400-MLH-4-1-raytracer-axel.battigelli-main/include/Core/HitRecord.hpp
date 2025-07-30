#pragma once

#include "Core/IMaterial.hpp"
#include "Math/Vector3.hpp"

#include <memory>

namespace Raytracer
{
    class HitRecord {
      public:
        Point3 point;
        Vector3 normal;
        std::shared_ptr<IMaterial> material;
        double t;
        bool frontFace;
    };

} // namespace Raytracer
