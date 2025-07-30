#pragma once
#include "Core/Camera.hpp"
#include "Core/IPrimitive.hpp"
#include "Math/Vector3.hpp"

namespace Raytracer
{
    class Scene {
      public:
        std::shared_ptr<IPrimitive> world;
        Camera camera;
    };
} // namespace Raytracer
