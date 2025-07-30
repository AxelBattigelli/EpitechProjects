#include "GraphicLayer.hpp"
#include "common.hpp"

GraphicLayer::GraphicLayer()
{
    this->_hdl = frontend_getInstance();
};

GraphicLayer::~GraphicLayer()
{
    if (this->_hdl)
        frontend_destroyInstance(this->_hdl);
    this->_hdl = nullptr;
}

void GraphicLayer::Load()
{
    frontend_init(this->_hdl);
}

void GraphicLayer::Unload()
{
    frontend_deinit(this->_hdl);
}

void GraphicLayer::RenderMap(const Arcade::Shared::Data::Map &inMap)
{
    std::vector<std::vector<tile_t>> outMap;
    std::vector<tile_t *> outMapLines;
    std::vector<uint64_t> outMapLengths;
    for (const auto &inLine : inMap) {
        std::vector<tile_t> outLine;
        for (const auto &inTile : inLine) {
            outLine.push_back(std::move(convertGrToFrTile(inTile)));
        }
        outMap.push_back(std::move(outLine));
        outMapLines.push_back(outMap.back().data());
        outMapLengths.push_back(outMap.back().size());
    }
    frontend_renderMap(this->_hdl, outMapLines.data(), outMap.size(), outMapLengths.data());
}

Arcade::Shared::Data::Event GraphicLayer::GetEvent() const
{
    return convertFrToGrEvent(frontend_getEvent(this->_hdl));
}

extern "C"
{
    const char *TYPE = "Graphical";

    Arcade::Graphical::IGraphical *GetInstance()
    {
        return new GraphicLayer();
    }
}
