/*
** EPITECH PROJECT, 2025
** RaylibWrapper
** File description:
** RaylibWrapper
*/

#include "RaylibWrapper.hpp"

extern "C" {
    RaylibWrapper::RaylibWrapper()
    {
    }

    RaylibWrapper::~RaylibWrapper()
    {
    }

    void RaylibWrapper::CloseWindow()
    {
        ::CloseWindow();
    }

    void RaylibWrapper::InitAudioDevice()
    {
        ::InitAudioDevice();
    }

    Music RaylibWrapper::LoadMusicStream(std::string filename)
    {
        return ::LoadMusicStream(filename.c_str());
    }

    void RaylibWrapper::PlayMusicStream(Music music)
    {
        ::PlayMusicStream(music);
    }

    Texture2D RaylibWrapper::LoadTexture(std::string filename)
    {
        return ::LoadTexture(filename.c_str());
    }

    double RaylibWrapper::GetTime()
    {
        return ::GetTime();
    }

    bool RaylibWrapper::WindowShouldClose()
    {
        return ::WindowShouldClose();
    }

    void RaylibWrapper::UpdateMusicStream(Music music)
    {
        ::UpdateMusicStream(music);
    }

    void RaylibWrapper::BeginDrawing()
    {
        ::BeginDrawing();
    }

    void RaylibWrapper::ClearBackground(Color color)
    {
        ::ClearBackground(color);
    }

    void RaylibWrapper::DrawTexture(Texture2D texture, int posX, int posY, Color tint)
    {
        ::DrawTexture(texture, posX, posY, tint);
    }

    void RaylibWrapper::EndDrawing()
    {
        ::EndDrawing();
    }

    void RaylibWrapper::UnloadTexture(Texture2D texture)
    {
        ::UnloadTexture(texture);
    }

    Font RaylibWrapper::LoadFont(std::string filename)
    {
        return ::LoadFont(filename.c_str());
    }

    Vector2 RaylibWrapper::MeasureTextEx(Font font, std::string text, float fontSize, float spacing)
    {
        return ::MeasureTextEx(font, text.c_str(), fontSize, spacing);
    }

    void RaylibWrapper::DrawTextEx(Font font, std::string text, Vector2 position, float fontSize, float spacing, Color tint)
    {
        ::DrawTextEx(font, text.c_str(), position, fontSize, spacing, tint);
    }

    void RaylibWrapper::DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color)
    {
        ::DrawRectangleRounded(rec, roundness, segments, color);
    }

    void RaylibWrapper::DrawRectangleRoundedLines(Rectangle rec, float roundness, int segments, Color color)
    {
        ::DrawRectangleRoundedLines(rec, roundness, segments, color);
    }

    void RaylibWrapper::StopMusicStream(Music music)
    {
        ::StopMusicStream(music);
    }

    void RaylibWrapper::UnloadMusicStream(Music music)
    {
        ::UnloadMusicStream(music);
    }

    void RaylibWrapper::CloseAudioDevice()
    {
        ::CloseAudioDevice();
    }

    void RaylibWrapper::UnloadFont(Font font)
    {
        ::UnloadFont(font);
    }

    bool RaylibWrapper::IsKeyPressed(int key)
    {
        return ::IsKeyPressed(key);
    }

    bool RaylibWrapper::IsKeyDown(int key)
    {
        return ::IsKeyDown(key);
    }

    void RaylibWrapper::SetTraceLogLevel(int logLevel)
    {
        ::SetTraceLogLevel(logLevel);
    }

    void RaylibWrapper::InitWindow(int width, int height, std::string title)
    {
        ::InitWindow(width, height, title.c_str());
    }

    void RaylibWrapper::DisableCursor()
    {
        ::DisableCursor();
    }

    void RaylibWrapper::BeginMode3D(Camera3D camera)
    {
        ::BeginMode3D(camera);
    }

    void RaylibWrapper::EndMode3D()
    {
        ::EndMode3D();
    }

    void RaylibWrapper::DrawText(std::string text, int posX, int posY, int fontSize, Color color)
    {
        ::DrawText(text.c_str(), posX, posY, fontSize, color);
    }

    int RaylibWrapper::GetFPS()
    {
        return ::GetFPS();
    }

    void RaylibWrapper::DrawRectangle(int posX, int posY, int width, int heigth, Color color)
    {
        ::DrawRectangle(posX, posY, width, heigth, color);
    }

    void RaylibWrapper::DrawRectangleLines(int posX, int posY, int width, int heigth, Color color)
    {
        ::DrawRectangleLines(posX, posY, width, heigth, color);
    }

    Color RaylibWrapper::Fade(Color color, float alpha)
    {
        return ::Fade(color, alpha);
    }

    void RaylibWrapper::DrawSphere(Vector3 centerPos, float radius, Color color)
    {
        ::DrawSphere(centerPos, radius, color);
    }

    Model RaylibWrapper::LoadModel(std::string filename)
    {
        return ::LoadModel(filename.c_str());
    }

    void RaylibWrapper::DrawModel(Model model, Vector3 position, float scale, Color tint)
    {
        ::DrawModel(model, position, scale, tint);
    }

    void RaylibWrapper::DrawCube(Vector3 position, float width, float height, float length, Color color)
    {
        ::DrawCube(position, width, height, length, color);
    }

    void RaylibWrapper::DrawCubeWires(Vector3 position, float width, float height, float length, Color color)
    {
        ::DrawCubeWires(position, width, height, length, color);
    }

    float RaylibWrapper::GetMouseWheelMove()
    {
        return ::GetMouseWheelMove();
    }

    void RaylibWrapper::UpdateCamera(Camera &camera, int mode)
    {
        ::UpdateCamera(&camera, mode);
    }

    void RaylibWrapper::DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color)
    {
        ::DrawLine(startPosX, startPosY, endPosX, endPosY, color);
    }

    void RaylibWrapper::DrawModelEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint)
    {
        ::DrawModelEx(model, position, rotationAxis, rotationAngle, scale, tint);
    }

    void RaylibWrapper::SetExitKey(int key)
    {
        ::SetExitKey(key);
    }

    Mesh RaylibWrapper::GenMeshCube(float width, float height, float length)
    {
        return ::GenMeshCube(width, height, length);
    }

    Model RaylibWrapper::LoadModelFromMesh(Mesh mesh)
    {
        return ::LoadModelFromMesh(mesh);
    }

    Shader RaylibWrapper::LoadShader(const std::string vsFileName, const std::string fsFileName)
    {
        return ::LoadShader(vsFileName.c_str(), fsFileName.c_str());
    }

    void RaylibWrapper::SetShaderValue(Shader shader, int locIndex, int value)
    {
        ::SetShaderValue(shader, locIndex, &value, SHADER_UNIFORM_INT);
    }

    int RaylibWrapper::GetShaderLocation(Shader shader, const std::string uniformName)
    {
        return ::GetShaderLocation(shader, uniformName.c_str());
    }

    Image RaylibWrapper::LoadImage(const std::string path)
    {
        return ::LoadImage(path.c_str());
    }

    TextureCubemap RaylibWrapper::LoadTextureCubemap(Image image, int layout)
    {
        return ::LoadTextureCubemap(image, layout);
    }

    void RaylibWrapper::UnloadImage(Image image)
    {
        ::UnloadImage(image);
    }

    void RaylibWrapper::rlDisableBackfaceCulling(void)
    {
        ::rlDisableBackfaceCulling();
    }

    void RaylibWrapper::rlEnableBackfaceCulling(void)
    {
        ::rlEnableBackfaceCulling();
    }

    void RaylibWrapper::rlEnableDepthMask(void)
    {
        ::rlEnableDepthMask();
    }

    void RaylibWrapper::rlDisableDepthMask(void)
    {
        ::rlDisableDepthMask();
    }

    void RaylibWrapper::UnloadShader(Shader shader)
    {
        ::UnloadShader(shader);
    }

    void RaylibWrapper::UnloadModel(Model model)
    {
        ::UnloadModel(model);
    }

    TextureCubemap RaylibWrapper::GenTextureCubemap(Shader shader, Texture2D panorama, int size, int format)
    {

        TextureCubemap cubemap = {0, 0, 0, 0, 0}; // id, width, height, mipmaps, format

        rlDisableBackfaceCulling();

        unsigned int rbo = rlLoadTextureDepth(size, size, true);
        cubemap.id = rlLoadTextureCubemap(0, size, format, 1);
        unsigned int fbo = rlLoadFramebuffer();
        rlFramebufferAttach(fbo, rbo, RL_ATTACHMENT_DEPTH, RL_ATTACHMENT_RENDERBUFFER, 0);
        rlFramebufferAttach(fbo, cubemap.id, RL_ATTACHMENT_COLOR_CHANNEL0, RL_ATTACHMENT_CUBEMAP_POSITIVE_X, 0);

        if (rlFramebufferComplete(fbo)) TraceLog(LOG_INFO, "FBO: [ID %i] Framebuffer object created successfully", fbo);

        rlEnableShader(shader.id);

        Matrix matFboProjection = MatrixPerspective(90.0*DEG2RAD, 1.0, rlGetCullDistanceNear(), rlGetCullDistanceFar());
        rlSetUniformMatrix(shader.locs[SHADER_LOC_MATRIX_PROJECTION], matFboProjection);
        Matrix fboViews[6] = {
            MatrixLookAt(Vector3{ 0.0f, 0.0f, 0.0f }, Vector3{  1.0f,  0.0f,  0.0f }, Vector3{ 0.0f, -1.0f,  0.0f }),
            MatrixLookAt(Vector3{ 0.0f, 0.0f, 0.0f }, Vector3{ -1.0f,  0.0f,  0.0f }, Vector3{ 0.0f, -1.0f,  0.0f }),
            MatrixLookAt(Vector3{ 0.0f, 0.0f, 0.0f }, Vector3{  0.0f,  1.0f,  0.0f }, Vector3{ 0.0f,  0.0f,  1.0f }),
            MatrixLookAt(Vector3{ 0.0f, 0.0f, 0.0f }, Vector3{  0.0f, -1.0f,  0.0f }, Vector3{ 0.0f,  0.0f, -1.0f }),
            MatrixLookAt(Vector3{ 0.0f, 0.0f, 0.0f }, Vector3{  0.0f,  0.0f,  1.0f }, Vector3{ 0.0f, -1.0f,  0.0f }),
            MatrixLookAt(Vector3{ 0.0f, 0.0f, 0.0f }, Vector3{  0.0f,  0.0f, -1.0f }, Vector3{ 0.0f, -1.0f,  0.0f })
        };

        rlViewport(0, 0, size, size);
        rlActiveTextureSlot(0);
        rlEnableTexture(panorama.id);

        for (int i = 0; i < 6; i++)
        {
            rlSetUniformMatrix(shader.locs[SHADER_LOC_MATRIX_VIEW], fboViews[i]);
            rlFramebufferAttach(fbo, cubemap.id, RL_ATTACHMENT_COLOR_CHANNEL0, RL_ATTACHMENT_CUBEMAP_POSITIVE_X + i, 0);
            rlEnableFramebuffer(fbo);
            rlClearScreenBuffers();
            rlLoadDrawCube();
        }

        rlDisableShader();
        rlDisableTexture();
        rlDisableFramebuffer();
        rlUnloadFramebuffer(fbo);
        rlViewport(0, 0, rlGetFramebufferWidth(), rlGetFramebufferHeight());
        rlEnableBackfaceCulling();

        cubemap.width = size;
        cubemap.height = size;
        cubemap.mipmaps = 1;
        cubemap.format = format;

        return cubemap;
    }

    float RaylibWrapper::GetFrameTime()
    {
        return ::GetFrameTime();
    }

    void RaylibWrapper::DrawLine3D(Vector3 startPos, Vector3 endPos, Color color)
    {
        ::DrawLine3D(startPos, endPos, color);
    }

    bool RaylibWrapper::IsMusicStreamPlaying(Music music)
    {
        return ::IsMusicStreamPlaying(music);
    }
}
