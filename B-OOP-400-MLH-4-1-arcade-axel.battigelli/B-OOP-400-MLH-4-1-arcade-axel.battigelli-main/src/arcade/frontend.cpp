#include "Shared/Exception.hpp"
#include "ffi.hpp"
#include "frontend.hpp"
#include "utils.hpp"
#include <cstring>
#include <dlfcn.h>
#include <filesystem>
#include <string>
#include <format>

void Frontend::load(std::string path)
{
    lib_info_t info;

    this->dlHandle = getLibInfo(path, info);
    dlclose(this->dlHandle);
    this->dlHandle = dlopen(path.c_str(), RTLD_NOW | RTLD_LAZY);
    if (info.api_version == 0) {
        using fn = Arcade::Graphical::IGraphical *();
        auto fun = dlSym<fn>(this->dlHandle, "GetInstance");
        this->grHandle = fun();
        return;
    }
    this->frDeinit = dlSym<decltype(frontend_deinit)>(this->dlHandle, "frontend_deinit");
    this->frInit = dlSym<decltype(frontend_init)>(this->dlHandle, "frontend_init");
    this->frDestroyInstance =
        dlSym<decltype(frontend_destroyInstance)>(this->dlHandle, "frontend_destroyInstance");
    this->frRenderMap =
        dlSym<decltype(frontend_renderMap)>(this->dlHandle, "frontend_renderMap");
    this->frGetEvent = dlSym<decltype(frontend_getEvent)>(this->dlHandle, "frontend_getEvent");
    auto fun = dlSym<decltype(frontend_getInstance)>(this->dlHandle, "frontend_getInstance");
    this->frHandle = fun();
}

Frontend::~Frontend()
{
    if (this->grHandle) {
        if (this->frInited)
            this->grHandle->Unload();
        delete this->grHandle;
        this->grHandle = nullptr;
    }
    if (this->frHandle) {
        if (this->frInited)
            this->frDeinit(this->frHandle);
        this->frDestroyInstance(this->frHandle);
        this->frHandle = nullptr;
    }
    if (this->dlHandle) {
        dlclose(this->dlHandle);
        this->dlHandle = nullptr;
    }
}

void Frontend::renderMapGr(std::vector<std::vector<tile_t>> inMap)
{
    if (!this->frInited) {
        this->grHandle->Load();
        this->frInited = true;
    }
    Arcade::Shared::Data::Map outMap;
    for (const auto &inLine : inMap) {
        Arcade::Shared::Data::Map::value_type outLine;
        for (const auto &inTile : inLine) {
            outLine.push_back(convertFrToGrTile(inTile));
        }
        outMap.push_back(outLine);
    }
    this->grHandle->RenderMap(outMap);
}

void Frontend::renderMapFr(std::vector<std::vector<tile_t>> inMap)
{
    if (!this->frInited) {
        this->frInit(this->frHandle);
        this->frInited = true;
    }
    std::vector<tile_t *> outMap;
    std::vector<uint64_t> outMapSizes;
    for (auto &inLine : inMap) {
        outMap.push_back(inLine.data());
        outMapSizes.push_back(inLine.size());
    }
    this->frRenderMap(this->frHandle, outMap.data(), outMap.size(), outMapSizes.data());
}

void Frontend::renderMap(std::vector<std::vector<tile_t>> inMap)
{
    if (this->grHandle)
        this->renderMapGr(inMap);
    else if (this->frHandle)
        this->renderMapFr(inMap);
}

event_t Frontend::getEventGr()
{
    if (!this->frInited) {
        this->grHandle->Load();
        this->frInited = true;
    }
    auto event = this->grHandle->GetEvent();
    return convertGrToFrEvent(event);
}

event_t Frontend::getEventFr()
{
    if (!this->frInited) {
        this->frInit(this->frHandle);
        this->frInited = true;
    }
    return this->frGetEvent(this->frHandle);
}

event_t Frontend::getEvent()
{
    if (this->grHandle)
        return this->getEventGr();
    else if (this->frHandle)
        return this->getEventFr();
    return {};
}
