#include "Core/Camera.hpp"

#include "Core/HitRecord.hpp"
#include "Core/IPrimitive.hpp"
#include "Math/Ray.hpp"
#include "Math/Vector3.hpp"

#include <cmath>
#include <iostream>
#include <thread>
#include <vector>

#include <cstdlib>
#include <random>
#include <functional>
#include <memory>
#include <utility>
#include <cstdint>


Raytracer::Camera::Camera(int width, int height, Vector3 lookFrom, Vector3 lookAt, Vector3 vup,
    double fov, int samplePerPixel)
    : width(width), height(height), lookFrom(lookFrom), lookAt(lookAt), vup(vup), fov(fov),
      samplePerPixel(samplePerPixel),
      maxThreads(std::jthread::hardware_concurrency())
{
    this->_aspectRatio = static_cast<double>(width) / height;
    this->_tanFov = tan((fov / 2.0) * (M_PI / 180.0));
}

auto Raytracer::Camera::getSky(const Ray& ray) -> Color {
    Color result(0.0, 0.0, 0.0);
    for (const auto& light : _lights) {
        result = result + light->illuminate(ray);
    }
    return result;
}

auto Raytracer::Camera::getRayColor(std::default_random_engine &rng, int depth,
    const Raytracer::IPrimitive &world, const Raytracer::Ray &ray) -> Color
{
    if (depth == 0)
        return {};
    auto hitRecord = world.intersect(ray, 0.001, std::numeric_limits<double>::infinity());
    if (!hitRecord)
        return getSky(ray);
    auto emitted = hitRecord->material->emitted(hitRecord.value(), rng);
    auto res = hitRecord->material->scatter(ray, hitRecord.value(), rng);
    if (!res)
        return emitted;
    return res->first.pairwise_mul(getRayColor(rng, depth - 1, world, res->second)) + emitted;
}

void Raytracer::Camera::threadpoolWorker(Camera &self, int part, std::vector<Color> &image,
    const Raytracer::IPrimitive &world,
    std::function<void(const std::vector<Color> &, int, int, int)> fun,
    std::default_random_engine rng)
{
    auto theta = self.fov * 3.1415926535897932385 / 180.0;
    if (std::isnan(theta)) {
        std::cerr << "Theta NAN\n";
        exit(5);
    }
    auto h = std::tan(theta / 2);
    if (std::isnan(h)) {
        std::cerr << "h NAN\n";
        exit(5);
    }
    auto viewport_height = 2 * h * 1;

    auto viewport_width = viewport_height * (double(self.width) / self.height);
    if (std::isnan(viewport_width)) {
        std::cerr << "viewport_width NAN\n";
    }
    auto w = (self.lookFrom - self.lookAt).normalize();
    if (std::isnan(w.x) || std::isnan(w.y) || std::isnan(w.z)) {
        std::cerr << "w NAN\n";
    }
    auto u_big = (self.vup).cross(w);
    if (std::isnan(u_big.x) || std::isnan(u_big.y) || std::isnan(u_big.z)) {
        std::cerr << "u_big NAN\n";
    }
    auto u = u_big.normalize();
    if (std::isnan(u.x) || std::isnan(u.y) || std::isnan(u.z)) {
        std::cerr << "u NAN\n";
    }
    auto v = w.cross(u);
    if (std::isnan(v.x) || std::isnan(v.y) || std::isnan(v.z)) {
        std::cerr << "v NAN\n";
    }

    auto viewport_u = viewport_width * u;   // Vector across viewport horizontal edge
    auto viewport_v = viewport_height * -v; // Vector down viewport vertical edge

    if (std::isnan(viewport_v.x) || std::isnan(viewport_v.y) || std::isnan(viewport_v.z)) {
        std::cerr << "viewport_v NAN\n";
    }
    if (std::isnan(viewport_u.x) || std::isnan(viewport_u.y) || std::isnan(viewport_u.z)) {
        std::cerr << "viewport_u NAN\n";
    }

    // Calculate the horizontal and vertical delta vectors from pixel to pixel.
    auto pixel_delta_u = viewport_u / self.width;
    auto pixel_delta_v = viewport_v / self.height;

    auto viewport_upper_left = self.lookFrom - (1 * w) - viewport_u / 2 - viewport_v / 2;
    if (std::isnan(viewport_upper_left.x) || std::isnan(viewport_upper_left.y)
        || std::isnan(viewport_upper_left.z)) {
        std::cerr << "viewport_upper_left NAN\n";
    }

    auto pixel00_loc = viewport_upper_left + 0.5 * (pixel_delta_u + pixel_delta_v);

    if (std::isnan(pixel00_loc.x) || std::isnan(pixel00_loc.y) || std::isnan(pixel00_loc.z)) {
        std::cerr << "pixel00_loc NAN\n";
    }

    std::uniform_real_distribution<> dis(0.0, 1.0);
    std::vector<std::uint32_t> seed_data(self.samplePerPixel);
    for (auto &s : seed_data) {
        s = rng();
    }

    std::seed_seq seed_seq(seed_data.begin(), seed_data.end());
    std::vector<std::uint32_t> thread_seeds(self.samplePerPixel);
    seed_seq.generate(thread_seeds.begin(), thread_seeds.end());

    for (int i = 0; i < self.samplePerPixel; ++i) {
        auto engine = std::default_random_engine(thread_seeds[i]);
        for (int y = part; y < self.height; y += self.maxThreads) {
            for (int x = 0; x < self.width; ++x) {
                auto direction = (pixel00_loc + ((x + dis(engine)) * pixel_delta_u)
                                     + ((y + dis(engine)) * pixel_delta_v))
                    - self.lookFrom;
                Ray ray = Ray(self.lookFrom, direction);
                image[x + (y * self.width)] += self.getRayColor(engine, 50, world, ray);
            }
        }
        fun(image, i + 1, part, self.maxThreads);
    }
}

auto Raytracer::Camera::render(std::default_random_engine &rng, const Raytracer::IPrimitive &world,
    std::function<void(const std::vector<Color> &, int, int, int)> fun)
    -> std::vector<Raytracer::Color>
{
    std::vector<Color> image(width * height, Color{});
    std::vector<std::jthread> threads;

    std::vector<std::uint32_t> seed_data(maxThreads);
    for (auto &s : seed_data) {
        s = rng();
    }

    std::seed_seq seed_seq(seed_data.begin(), seed_data.end());
    std::vector<std::uint32_t> thread_seeds(maxThreads);
    seed_seq.generate(thread_seeds.begin(), thread_seeds.end());

    // Create a thread for each part
    for (int t = 0; t < maxThreads; ++t) {
        auto engine = std::default_random_engine(thread_seeds[t]);
        auto worker = [this, &image, &world, &fun, engine = std::move(engine)](int part) {
            threadpoolWorker(*this, part, image, world, fun, engine);
        };
        threads.emplace_back(worker, t);
    }
    threads.clear();
    for (auto &pix : image) {
        pix = (pix / samplePerPixel);
        if (pix.sqr_length() > 1)
            pix = pix.normalize();
    }
    return image;
}
