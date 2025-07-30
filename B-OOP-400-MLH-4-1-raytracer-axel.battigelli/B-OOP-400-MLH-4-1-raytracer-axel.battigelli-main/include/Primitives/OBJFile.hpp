/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** OBJFile
*/

#pragma once

#include "Core/HitRecord.hpp"
#include "Core/IPrimitive.hpp"
#include "BVHNode.hpp"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <limits>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

namespace Raytracer::Primitives
{

    class Triangle : public IPrimitive {
      private:
        Vector3 _v0, _v1, _v2;
        Vector3 _n0, _n1, _n2;
        std::shared_ptr<IMaterial> _mat;

      public:
        Triangle(std::shared_ptr<IMaterial> mat, const Vector3 &v0, const Vector3 &v1,
            const Vector3 &v2, const Vector3 &n0, const Vector3 &n1, const Vector3 &n2)
            : _mat(mat), _v0(v0), _v1(v1), _v2(v2), _n0(n0), _n1(n1), _n2(n2)
        {}
        [[nodiscard]] std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override;
        std::optional<AABB> boundingBox() const override;
    };

    class ObjFile : public IPrimitive {
      private:
        std::vector<Vector3> _vertices;
        std::vector<Vector3> _normals;
        std::vector<std::shared_ptr<IPrimitive>> _objects;
        std::shared_ptr<BVHNode> _bvhRoot;
        std::shared_ptr<IMaterial> _mat;

        bool parseFaceVertexNormal(std::istringstream &ss, int &vIdx, int &nIdx);

      public:
        ObjFile(std::shared_ptr<IMaterial> mat, Vector3 scale, Vector3 translate,
            const std::string &filename)
            : _mat(mat)
        {
            loadFromFile(scale, translate, filename);
        }

        bool loadFromFile(Vector3 scale, Vector3 translate, const std::string &filename);

        [[nodiscard]] std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override
        {
            return _bvhRoot->intersect(ray, t_min, t_max);
        }

        std::optional<AABB> boundingBox() const override { return _bvhRoot->boundingBox(); }
    };

}
