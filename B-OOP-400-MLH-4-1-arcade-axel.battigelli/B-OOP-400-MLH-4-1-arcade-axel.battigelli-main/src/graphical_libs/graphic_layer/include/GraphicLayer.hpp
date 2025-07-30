#pragma once

#include "Graphical/IGraphical.hpp"
#include "Shared/Data.hpp"
#include "ffi.hpp"

class GraphicLayer : public Arcade::Graphical::IGraphical {
    frontend_handle_t *_hdl;

  public:
    GraphicLayer();

    virtual ~GraphicLayer() override;

    virtual void Load() override;

    virtual void Unload() override;

    virtual void RenderMap(const Arcade::Shared::Data::Map &) override;

    virtual Arcade::Shared::Data::Event GetEvent() const override;
};
