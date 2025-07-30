#include "Primitives/Torus.hpp"

#include "Core/HitRecord.hpp"
#include "Math/Ray.hpp"
#include <cmath>
#include <optional>
#include <complex>

Raytracer::Primitives::Torus::Torus(Point3 center, Vector3 upNormal, double innerRadius, double outerRadius, std::shared_ptr<IMaterial> material)
    : center(center),
      upNormal(upNormal),
      innerRadius(innerRadius),
      outerRadius(outerRadius),
      material(std::move(material)) {}

std::optional<Raytracer::HitRecord> Raytracer::Primitives::Torus::intersect(const Ray &ray, double t_min, double t_max) const
{
    auto solveQuartic = [](double A, double B, double C, double D, double E) -> std::vector<double> {
        if (A == 0)
            return {};

        B /= A;
        C /= A;
        D /= A;
        E /= A;

        const std::complex<double> coeffs[] = {
            std::complex<double>(1, 0),
            std::complex<double>(B, 0),
            std::complex<double>(C, 0),
            std::complex<double>(D, 0),
            std::complex<double>(E, 0),
        };

        std::vector<std::complex<double>> roots(4);
        roots[0] = std::complex<double>(1, 0);
        roots[1] = std::complex<double>(0, 1);
        roots[2] = std::complex<double>(-1, 0);
        roots[3] = std::complex<double>(0, -1);

        const int max_iter = 100;
        const double tol = 1e-12;
        for (int iter = 0; iter < max_iter; ++iter) {
            bool converged = true;
            for (int i = 0; i < 4; ++i) {
                std::complex<double> prod(1, 0);
                for (int j = 0; j < 4; ++j) {
                    if (i != j)
                        prod *= (roots[i] - roots[j]);
                }

                const std::complex<double> fx = (((coeffs[0] * roots[i] + coeffs[1]) * roots[i] + coeffs[2]) * roots[i] + coeffs[3]) * roots[i] + coeffs[4];
                const std::complex<double> delta = fx / prod;
                roots[i] -= delta;
                if (std::abs(delta) > tol)
                    converged = false;
            }
            if (converged)
                break;
        }

        std::vector<double> realRoots;
        for (const auto &r : roots) {
            if (std::abs(r.imag()) < 1e-8) {
                realRoots.push_back(r.real());
            }
        }

        return realRoots;
    };

    const double R = this->outerRadius;
    const double r = this->innerRadius;

    Vector3 w = upNormal.normalize();
    Vector3 u = (std::abs(w.x) > 0.9 ? Vector3(0, 1, 0) : Vector3(1, 0, 0)).cross(w).normalize();
    Vector3 v = w.cross(u);

    auto toLocal = [&](const Point3 &p) {
        const Vector3 offset = p - center;
        return Point3(
            offset.dot(u),
            offset.dot(v),
            offset.dot(w)
        );
    };

    auto dirToLocal = [&](const Vector3 &d) {
        return Vector3(
            d.dot(u),
            d.dot(v),
            d.dot(w)
        );
    };

    const Vector3 Ro_local = toLocal(ray.origin);
    const Vector3 Rd_local = dirToLocal(ray.direction);

    const double ox = Ro_local.x, oy = Ro_local.y, oz = Ro_local.z;
    const double dx = Rd_local.x, dy = Rd_local.y, dz = Rd_local.z;

    const double sum_d_sq = (dx*dx) + (dy*dy) + (dz*dz);
    const double e = (ox*ox) + (oy*oy) + (oz*oz) - (R*R) - (r*r);
    const double f = (ox*dx) + (oy*dy) + (oz*dz);
    const double four_R_sq = 4.0 * R * R;

    const double A = sum_d_sq * sum_d_sq;
    const double B = 4.0 * sum_d_sq * f;
    const double C = (2.0 * sum_d_sq * e) + (4.0 * f * f) + (four_R_sq * dz * dz);
    const double D = (4.0 * f * e) + (2.0 * four_R_sq * oz * dz);
    const double E = (e * e) - (four_R_sq * (r * r - oz * oz));

    auto roots = solveQuartic(A, B, C, D, E);
    if (roots.empty())
        return std::nullopt;

    double t_hit = std::numeric_limits<double>::infinity();
    for (const double t : roots) {
        if (t > t_min && t < t_max && t < t_hit) {
            t_hit = t;
        }
    }

    if (t_hit == std::numeric_limits<double>::infinity())
        return std::nullopt;

    const Point3 hit_point_local = Ro_local + t_hit * Rd_local;
    const Vector3 rel = hit_point_local;

    const double x = rel.x;
    const double y = rel.y;
    const double z = rel.z;
    const double sum_sq = (x*x) + (y*y) + (z*z);
    const double param = sum_sq - (r*r) - (R*R);

    Vector3 normal_local(
        (4.0 * x * param) - (8.0 * R * R * x),
        (4.0 * y * param) - (8.0 * R * R * y),
        (4.0 * z * param)
    );
    normal_local = normal_local.normalize();

    auto toWorld = [&](const Vector3 &v_local) {
        return (v_local.x * u) + (v_local.y * v) + (v_local.z * w);
    };

    const Point3 hit_point = ray.at(t_hit);
    const Vector3 normal = toWorld(normal_local).normalize();

    Raytracer::HitRecord rec;
    rec.t = t_hit;
    rec.point = hit_point;
    rec.frontFace = ray.direction.dot(normal) < -1e-6;
    rec.normal = rec.frontFace ? normal : -normal;
    rec.material = this->material;
    return rec;
}

std::optional<Raytracer::AABB> Raytracer::Primitives::Torus::boundingBox() const {
    const Vector3 axis = this->upNormal.normalize();
    Vector3 ortho1, ortho2;

    if (std::abs(axis.x) > 0.9)
        ortho1 = Vector3(0, 1, 0).cross(axis).normalize();
    else
        ortho1 = Vector3(1, 0, 0).cross(axis).normalize();
    ortho2 = axis.cross(ortho1).normalize();

    const double r = this->innerRadius;
    const double R = this->outerRadius;
    const std::vector<Vector3> directions = {
        ortho1,
        -ortho1,
        ortho2,
        -ortho2
    };

    std::vector<Point3> points;
    for (const auto& dir : directions) {
        const Point3 ringPoint = this->center + dir * R;

        points.push_back(ringPoint + axis * r);
        points.push_back(ringPoint - axis * r);
        points.push_back(ringPoint + ortho1 * r);
        points.push_back(ringPoint - ortho1 * r);
        points.push_back(ringPoint + ortho2 * r);
        points.push_back(ringPoint - ortho2 * r);
    }

    Point3 min = points[0];
    Point3 max = points[0];
    for (const auto& pointer : points) {
        min = min.pairwise_min(pointer);
        max = max.pairwise_max(pointer);
    }
    return Raytracer::AABB(min, max);
}
