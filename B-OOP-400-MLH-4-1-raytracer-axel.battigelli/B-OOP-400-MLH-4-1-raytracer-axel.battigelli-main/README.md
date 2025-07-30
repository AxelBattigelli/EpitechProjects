# Raytracer

## Module Oriented Objects Programming

## Language CPP

## Overview

This project is a custom raytracer designed to simulate realistic 3D scenes by tracing rays of light through virtual space. It supports a variety of geometric primitives (e.g., cylinders, cones, fractals, .OBJ), lighting models (including ambient occlusion), advanced materials (reflection, refraction, textures), and scene transformations. Performance is enhanced through features like multithreading and spatial partitioning, and the interface allows for real-time preview, image display, and automatic scene reloading. The system is modular, extensible, and built for both visual quality and efficiency.

## Functionnalities

Use the raytracer program to create a PPM image in text format (output.ppm).
During the generation, you can observe the temporary result in preview.ppm, in binary mode.
You can also use the PPMPreview executable to watch in real time, the progress of the image creation.

## Architecture

```
├── include/
│   ├── Core/
│   │   ├── Camera.hpp
│   │   ├── HitRecord.hpp
│   │   ├── IMaterial.hpp
│   │   ├── IPrimitive.hpp
│   │   ├── Parser.hpp
│   │   ├── Ppm.hpp
│   │   └── Scene.hpp
│   ├── Material/
│   │   ├── FlatColor.hpp
│   │   ├── Mirror.hpp
│   │   └── Transparency.hpp
│   ├── Math/
│   │   ├── AABB.hpp                 // optimise collides
│   │   ├── Ray.hpp
│   │   └── Vector3.hpp
│   ├── Primitives/
│   │   ├── BVHNode.cpp              // bounding volume manager
│   │   ├── List.cpp                 // store all primitives
│   │   ├── Boxe.cpp
│   │   ├── BoxeBuilder.cpp
│   │   ├── Cone.cpp
│   │   ├── ConeBuilder.cpp
│   │   └── ...
│   └── Exception.hpp                // error and ignore class exception
├── src/
│   ├── Core/
│   │   ├── Camera.cpp
│   │   ├── Parser.cpp
│   │   └── Ppm.cpp
│   ├── Primitives/
│   │   ├── BVHNode.cpp              // bounding volume manager
│   │   ├── List.cpp                 // store all primitives
│   │   ├── Boxe.cpp
│   │   ├── BoxeBuilder.cpp
│   │   ├── Cone.cpp
│   │   ├── ConeBuilder.cpp
│   │   └── ...
│   └── main.cpp
├── test/
│   └── demos.cfg                    // config file for live demo
│── output.ppm                       // output generated
└── preview.ppm                      // real time preview generated
```

## Definition of the interfaces
In Include/Core/IMaterial.hpp, you have the interface for the material class.
```
namespace Raytracer
{
    class HitRecord;

    class IMaterial {
      public:
        virtual Color emitted(const HitRecord &rec, std::default_random_engine &rng) = 0;
        virtual std::optional<std::pair<Color, Ray>> scatter(
            const Ray &r_in, const HitRecord &rec, std::default_random_engine &rng) = 0;
        virtual ~IMaterial() = default;
    };
}
```

In Include/Core/IPrimitive.hpp, you have the interface for the primitive (object) class.
```
namespace Raytracer
{

    class IPrimitive {
      public:
        [[nodiscard]] virtual std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const = 0;
        virtual std::optional<AABB> boundingBox() const = 0;
        virtual ~IPrimitive() = default;
    };
}
```

In Include/Core/ILight.hpp, you have the interface for the light class. This concern the sku and sun module only. An object which must emit a light is considere as a primitive with a emissive material.
```
namespace Raytracer {

    class ILight {
      public:
        virtual ~ILight() = default;

        virtual Color illuminate(const Ray& ray) const = 0;
    };

}
```

## Config file format
### Camera Configuration

The camera configuration defines the properties of the camera, such as its position, direction, and field of view.

- Resolution: Defines the width and height of the rendered image.

- Position: Sets the position of the camera in 3D space (x, y, z).

- LookAt: Defines the point in the scene the camera is looking at.

- ViewUp: Sets the direction of "up" for the camera.

- Field of View (FOV): Controls the camera's field of view in degrees.

- Samples per Pixel: Determines the quality of the image by controlling how many samples are taken for each pixel.

### Primitives in the scene

Primitives are basic 3D shapes placed in the scene. These can include spheres, planes, boxes, cylinders, cones, pyramides and toruses. Each shape has properties like position, color, size, and other characteristics that define its appearance.
We have also an object

For example:

    Sphere: Defined by its position (x, y, z), radius, and color.

    Plane: Defined by its normal vector and position.

    Other Shapes (Box, Cylinder, Cone, Pyramide, Torus): Similar to spheres, they can have size, position, and color.

Example:

A sphere might look like this:
```
{
    type = "sphere";
    content = (
        { x = 5.0; y = 40.0; z = 60.0; r = 25.0; color = { r = 255; g = 64; b = 64; }; }
    );
}
```

## Adding extensions
### Add more Textures
To add a new texture, you must implement the following functions in include/Materials/. But the number of parameters is free, like the content of each function if it's follow the defenition and the return type.

```
namespace Raytracer::Materials
{
    class MyNewMaterial : public IMaterial {
      public:
        // Material-specific properties
        // e.g., color, roughness, etc.

        MyNewMaterial(/* constructor params */) {
            // initialize properties
        }

        virtual Color emitted(const HitRecord &rec, std::default_random_engine &rng) override {
            // Optional: return emitted light (e.g., for emissive materials)
            return Color(0, 0, 0); // default: no emission
        }

        virtual std::optional<std::pair<Color, Ray>> scatter(
            const Ray &r_in, const HitRecord &rec, std::default_random_engine &rng) override {
            // Implement material-specific scattering logic
            return std::nullopt;
        }
    };
} // namespace Raytracer::Materials
```

And the next step is to call the material

E.g. for a color :
```
const Color color(0.0, 0.0, 0.0);
material = std::make_shared<Raytracer::Materials::Color>(color);
```

### Add more Primitives
To add a new primitive, you must implement the following functions in include/Primitives/. 
But the number of parameters is free, like the content of each function if it's follow the defenition and the return type.

```
namespace Raytracer::Primitives
{
    class EG : public IPrimitive {
      public:
        Point3 center;
        Vector3 upNormal;
        // Type to store
        std::shared_ptr<IMaterial> material;

        EG(Point3 center, Vector3 upNormal, /* parameters */
            std::shared_ptr<IMaterial> material);
        [[nodiscard]] virtual std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override;
        [[nodiscard]] virtual std::optional<AABB> boundingBox() const override;

      private:
        AABB _boundingBox;
        Vector3 _forwardNormal;

    };
} // namespace Raytracer::Primitives
```
When you have your object, you probably want to add the builder functionnalities and use the .cfg file.
Create now the file at the same place (Eg.hpp will become EgBuilder.hpp) and place this functions at minimum:
```
namespace Raytracer::Primitives
{
    class EgBuilder {
        public:
            EgBuilder() {};
            EgBuilder(const libconfig::Setting &contentList);
            EgBuilder &setUpNormal(Vector3 upNormal);
            EgBuilder &setRightNormal(Vector3 rightNormal);
            // setter for parameter to store
            EgBuilder &setMaterial(std::shared_ptr<IMaterial> material);

            Eg build() const;
        private:
            Point3 _center;
            Vector3 _upNormal;
            // Type to store
            std::shared_ptr<IMaterial> _material;

    };
}
```

And next you can add a parsing using the libconfig++ in order to parse the data provide by the config file. (see other code for references)

## Compilation
```
./compile

or 

./compile clean      # use in order to clear your build folder and executable
```

## Usage

```
./raytracer <SCENE_FILE>
    SCENE_FILE: scene configuration
```
