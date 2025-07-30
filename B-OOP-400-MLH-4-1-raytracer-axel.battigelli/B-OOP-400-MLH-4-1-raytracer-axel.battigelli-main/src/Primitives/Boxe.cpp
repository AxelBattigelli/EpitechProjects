#include "Primitives/Boxe.hpp"
#include "Core/HitRecord.hpp"
#include "Core/IMaterial.hpp"
#include "Math/Ray.hpp"
#include "Math/Vector3.hpp"

#include <algorithm>
#include <memory>
#include <optional>
#include <utility>
#include <vector>
#include <cmath>

Raytracer::Primitives::Boxe::Boxe(Point3 center, Vector3 upNormal, Vector3 rightNormal, Vector3 size, std::shared_ptr<IMaterial> material)
    : center(center), upNormal(upNormal.normalize()), rightNormal(rightNormal.normalize()), size(size), material(std::move(material))
{
    this->_forwardNormal = this->upNormal.cross(this->rightNormal).normalize();

    const auto halfSize = size * 0.5;
    const Vector3 uVal = this->rightNormal * halfSize.x;
    const Vector3 vVal = this->upNormal * halfSize.y;
    const Vector3 wVal = this->_forwardNormal * halfSize.z;
    std::vector<Point3> corners;

    for (const int dxVal : {-1, 1}) {
        for (const int dyVal : {-1, 1}) {
            for (const int dzVal : {-1, 1}) {
                corners.emplace_back(center + dxVal * uVal + dyVal * vVal + dzVal * wVal);
            }
        }
    }

    Point3 min = corners[0];
    Point3 max = corners[0];
    for (const auto &pointer : corners) {
        min = min.pairwise_min(pointer);
        max = max.pairwise_max(pointer);
    }
    this->_boundingBox = AABB(min, max);
}

auto Raytracer::Primitives::Boxe::intersect(const Ray &ray, double t_min, double t_max) const -> std::optional<Raytracer::HitRecord>
{
    const auto halfSize = this->size * 0.5;
    const Vector3 uVal = this->rightNormal;
    const Vector3 vVal = this->upNormal;
    const Vector3 wVal = this->_forwardNormal;
    const Vector3 roVal = ray.origin - this->center;
    const Vector3 localOrigin = {
        roVal.dot(uVal),
        roVal.dot(vVal),
        roVal.dot(wVal)
    };
    const Vector3 localDir = {
        ray.direction.dot(uVal),
        ray.direction.dot(vVal),
        ray.direction.dot(wVal)
    };

    Ray localRay(localOrigin, localDir);
    const Vector3 min = {-halfSize.x, -halfSize.y, -halfSize.z};
    const Vector3 max = { halfSize.x,  halfSize.y,  halfSize.z};

    double t0Val = t_min;
    double t1Val = t_max;
    for (int i = 0; i < 3; ++i) {
        const double origin = localRay.origin[i];
        const double dir = localRay.direction[i];
        const double invD = 1.0 / dir;

        double tNear = (min[i] - origin) * invD;
        double tFar = (max[i] - origin) * invD;

        if (invD < 0.0)
            std::swap(tNear, tFar);

        t0Val = std::max(tNear, t0Val);
        t1Val = std::min(tFar, t1Val);

        if (t1Val <= t0Val)
            return std::nullopt;
    }

    HitRecord rec;
    rec.t = t0Val;
    rec.point = ray.at(t0Val);

    Vector3 localHit = localOrigin + t0Val * localDir;
    Vector3 normal = {0, 0, 0};
    const double bias = 1e-4;
    for (int i = 0; i < 3; ++i) {
        if (std::abs(localHit[i] - max[i]) < bias) {
            normal[i] = 1;
            break;
        }
        if (std::abs(localHit[i] - min[i]) < bias) {
            normal[i] = -1;
            break;
        }
    }

    rec.normal = {
        (normal.x * uVal.x) + (normal.y * vVal.x) + (normal.z * wVal.x),
        (normal.x * uVal.y) + (normal.y * vVal.y) + (normal.z * wVal.y),
        (normal.x * uVal.z) + (normal.y * vVal.z) + (normal.z * wVal.z)
    };

    rec.material = this->material;
    return rec;
}

auto Raytracer::Primitives::Boxe::boundingBox() const -> std::optional<Raytracer::AABB>
{
    return this->_boundingBox;
}
