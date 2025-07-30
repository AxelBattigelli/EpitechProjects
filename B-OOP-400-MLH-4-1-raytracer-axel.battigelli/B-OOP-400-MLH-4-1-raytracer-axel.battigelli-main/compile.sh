#!/bin/bash

set -e

if [ "$1" == "clean" ]; then
    echo "Cleaning build artifacts..."
    rm -rf build
    rm -rf bonus/viewer/build
fi

echo "=== Build PPMViewer ==="

mkdir -p bonus/viewer/build
cd bonus/viewer/build

if command -v ninja &> /dev/null; then
    echo "Using Ninja"
    cmake .. -G Ninja
else
    echo "Ninja not found, using default"
    cmake ..
fi

cmake --build .
cd ../../../

cp bonus/viewer/build/PPMViewer .

echo "=== Build Raytracer ==="

mkdir -p build
cd build

if command -v ninja &> /dev/null; then
    echo "Using Ninja"
    cmake .. -G Ninja
else
    echo "Ninja not found, using default"
    cmake ..
fi

cmake --build .
