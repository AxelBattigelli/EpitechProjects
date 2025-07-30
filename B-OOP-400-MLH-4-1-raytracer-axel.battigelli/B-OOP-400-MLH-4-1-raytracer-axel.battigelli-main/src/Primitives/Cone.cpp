#include "Primitives/Cone.hpp"
#include "Core/HitRecord.hpp"
#include "Math/Ray.hpp"

#include <cmath>
#include <optional>
#include <complex>

Raytracer::Primitives::Cone::Cone(Point3 center, Vector3 upNormal, double radius, double height, std::shared_ptr<IMaterial> material)
    : center(center),
      upNormal(upNormal),
      radius(radius),
      height(height),
      material(std::move(material)) {}

std::optional<Raytracer::HitRecord> Raytracer::Primitives::Cone::intersect(const Ray &ray, double t_min, double t_max) const
{
    Vector3 co = ray.origin - this->center;

    Vector3 axis = this->upNormal.normalize();

    double cosTheta = this->height / std::sqrt(this->height * this->height + this->radius * this->radius);
    double cos2 = cosTheta * cosTheta;

    Vector3 d = ray.direction;
    Vector3 v = axis;
    Vector3 x = co;

    double dv = d.dot(v);
    double xv = x.dot(v);

    Vector3 d_ = d - dv * v;
    Vector3 x_ = x - xv * v;

    double A = d_.dot(d_) - cos2 * dv * dv;
    double B = 2.0 * (d_.dot(x_) - cos2 * dv * xv);
    double C = x_.dot(x_) - cos2 * xv * xv;

    double discriminant = B * B - 4 * A * C;

    if (discriminant < 0) {
        return std::nullopt;
    }

    double sqrtDisc = std::sqrt(discriminant);
    double t1 = (-B - sqrtDisc) / (2 * A);
    double t2 = (-B + sqrtDisc) / (2 * A);

    for (double t : {t1, t2}) {
        if (t < t_min || t > t_max)
            continue;

        Point3 p = ray.at(t);
        Vector3 v0 = p - this->center;
        double h = v0.dot(axis);

        if (h < 0 || h > this->height)
            continue;

        Vector3 cp = v0 - h * axis;
        Vector3 normal = (cp.normalize() * this->height + axis * this->radius).normalize();

        HitRecord rec;
        rec.t = t;
        rec.point = p;
        rec.normal = normal;
        rec.material = this->material;
        return rec;
    }

    return std::nullopt;
}

std::optional<Raytracer::AABB> Raytracer::Primitives::Cone::boundingBox() const
{
    return std::nullopt;

    Vector3 axis = this->upNormal.normalize();
    Vector3 ortho1, ortho2;

    if (std::abs(axis.x) > 0.9)
        ortho1 = Vector3(0, 1, 0).cross(axis).normalize();
    else
        ortho1 = Vector3(1, 0, 0).cross(axis).normalize();
    ortho2 = axis.cross(ortho1).normalize();

    ortho1 = ortho1 * this->radius;
    ortho2 = ortho2 * this->radius;

    Vector3 tip = center + axis * this->height;
    std::vector<Point3> points = {
        center + ortho1 + ortho2,
        center + ortho1 - ortho2,
        center - ortho1 + ortho2,
        center - ortho1 - ortho2,
        tip
    };

    Point3 min = points[0];
    Point3 max = points[0];
    for (const auto& p : points) {
        min = min.pairwise_min(p);
        max = max.pairwise_max(p);
    }

    return AABB(min, max);
}
