#pragma once

#include "Core/IPrimitive.hpp"
#include "Core/ILight.hpp"
#include "Math/Vector3.hpp"

#include <functional>
#include <vector>
#include <memory>

namespace Raytracer
{
    class Camera {
      private:
        double _aspectRatio;
        double _tanFov;
        std::vector<std::shared_ptr<ILight>> _lights;
    
      public:
        int width, height;
        Vector3 lookFrom;
        Vector3 lookAt;
        Vector3 vup;
        double fov;
        int samplePerPixel;
        std::uint64_t maxThreads;

        Camera() = default;
        Camera(int width, int height, Vector3 lookFrom, Vector3 lookAt, Vector3 vup, double fov,
            int samplePerPixel);
        std::vector<Color> render(std::default_random_engine &rng, const IPrimitive &world,
            std::function<void(const std::vector<Color> &, int, int, int)>);

        Color getSky(const Ray &_ray);
        void addLight(std::shared_ptr<ILight> light) {
            _lights.push_back(std::move(light));
        }
        Color getRayColor(
            std::default_random_engine &rng, int depth, const IPrimitive &world, const Ray &ray);
        static void threadpoolWorker(Camera &, int part, std::vector<Color> &image,
            const Raytracer::IPrimitive &world,
            std::function<void(const std::vector<Color> &, int, int, int)> fun,
            std::default_random_engine rng);
    };
} // namespace Raytracer
