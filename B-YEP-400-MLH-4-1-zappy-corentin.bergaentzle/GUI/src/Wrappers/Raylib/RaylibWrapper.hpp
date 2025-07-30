/*
** EPITECH PROJECT, 2025
** RaylibWrapper
** File description:
** RaylibWrapper
*/

#pragma once

#include "raylib.h"
#include "rlgl.h"
#include "raymath.h"
#include <string>

class RaylibWrapper {
    public:
        RaylibWrapper();
        ~RaylibWrapper();
        static void CloseWindow();
        static void InitAudioDevice();
        static Music LoadMusicStream(std::string filename);
        static void PlayMusicStream(Music music);
        static Texture2D LoadTexture(std::string filename);
        static double GetTime();
        static bool WindowShouldClose();
        static void UpdateMusicStream(Music music);
        static void BeginDrawing();
        static void ClearBackground(Color color);
        static void DrawTexture(Texture2D texture, int posX, int poxY, Color tint);
        static void EndDrawing();
        static void UnloadTexture(Texture2D texture);
        static Font LoadFont(std::string filename);
        static Vector2 MeasureTextEx(Font font, std::string text, float fontSize, float spacing);
        static void DrawTextEx(Font font, std::string text, Vector2 position, float fontSize, float spacing, Color tint);
        static void DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color);
        static void DrawRectangleRoundedLines(Rectangle rec, float roundness, int segments, Color color);
        static void StopMusicStream(Music music);
        static void UnloadMusicStream(Music music);
        static void CloseAudioDevice();
        static void UnloadFont(Font font);
        static bool IsKeyPressed(int key);
        static bool IsKeyDown(int key);
        static void SetTraceLogLevel(int logLevel);
        static void InitWindow(int width, int height, std::string title);
        static void DisableCursor();
        static void BeginMode3D(Camera3D camera);
        static void EndMode3D();
        static void DrawText(std::string text, int posX, int posY, int fontSize, Color color);
        static int GetFPS();
        static void DrawRectangle(int posX, int posY, int width, int height, Color color);
        static void DrawRectangleLines(int posX, int posY, int width, int height, Color color);
        static Color Fade(Color color, float alpha);
        static void DrawSphere(Vector3 centerPos, float radius, Color color);
        static Model LoadModel(std::string filename);
        static void DrawModel(Model model, Vector3 position, float scale, Color tint);
        static void DrawCube(Vector3 position, float width, float height, float length, Color color);
        static void DrawCubeWires(Vector3 position, float width, float height, float length, Color color);
        static float GetMouseWheelMove();
        static void UpdateCamera(Camera &camera, int mode);
        static void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color);
        static void DrawModelEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint);
        static void SetExitKey(int key);
        static Mesh GenMeshCube(float width, float height, float length);
        static Model LoadModelFromMesh(Mesh mesh);
        static Shader LoadShader(const std::string vsFileName, const std::string fsFileName);
        static void SetShaderValue(Shader shader, int locIndex, int value);
        static int GetShaderLocation(Shader shader, const std::string uniformName);
        static Image LoadImage(const std::string path);
        static TextureCubemap LoadTextureCubemap(Image image, int layout);
        static void UnloadImage(Image image);
        static void rlDisableBackfaceCulling(void);
        static void rlEnableBackfaceCulling(void);
        static void rlEnableDepthMask(void);
        static void rlDisableDepthMask(void);
        static void UnloadShader(Shader shader);
        static void UnloadModel(Model model);
        static TextureCubemap GenTextureCubemap(Shader shader, Texture2D panorama, int size, int format);
        static float GetFrameTime();
        static void DrawLine3D(Vector3 startPos, Vector3 endPos, Color color);
        static bool IsMusicStreamPlaying(Music music);
};
