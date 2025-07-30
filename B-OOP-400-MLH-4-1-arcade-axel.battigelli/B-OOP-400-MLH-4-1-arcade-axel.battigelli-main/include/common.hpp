#pragma once

#include "Shared/Data.hpp"
#include "ffi.hpp"

tile_t convertGrToFrTile(const Arcade::Shared::Data::Tile &in);

Arcade::Shared::Data::Tile convertFrToGrTile(const tile_t &in);

event_t convertGrToFrEvent(const Arcade::Shared::Data::Event &in);

Arcade::Shared::Data::Event convertFrToGrEvent(const event_t &in);
