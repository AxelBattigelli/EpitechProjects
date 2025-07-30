#include "Graphical/IGraphical.hpp"
#include <common.hpp>
#include <ffi.hpp>
#include <vector>

#pragma once

class Frontend {
  private:
    void *dlHandle = nullptr;
    frontend_handle_t *frHandle = nullptr;
    Arcade::Graphical::IGraphical *grHandle = nullptr;
    bool frInited = false;

    decltype(frontend_destroyInstance) *frDestroyInstance = nullptr;
    decltype(frontend_deinit) *frDeinit = nullptr;
    decltype(frontend_init) *frInit = nullptr;
    decltype(frontend_renderMap) *frRenderMap = nullptr;
    decltype(frontend_getEvent) *frGetEvent = nullptr;

    void renderMapGr(std::vector<std::vector<tile_t>> inMap);
    void renderMapFr(std::vector<std::vector<tile_t>> inMap);

    event_t getEventGr();
    event_t getEventFr();

  public:
    void load(std::string path);
    ~Frontend();
    void renderMap(std::vector<std::vector<tile_t>> inMap);
    event_t getEvent();
};
