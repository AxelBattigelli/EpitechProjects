#pragma once

#include "Camera.hpp"
#include "Primitives/List.hpp"

namespace Raytracer
{
    class Parser {
      private:
        Camera _cam;
        Primitives::List _world;

      public:
        void readFile(const std::string &filename);
        auto getCam() -> Camera { return this->_cam; };
        Primitives::List getWorld() { return this->_world; };
    };
} // namespace Raytracer
