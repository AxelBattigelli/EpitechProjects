#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>

namespace nts::component
{
    class TrueComponent : public AComponent {
      public:
        Tristate compute(std::size_t /*pin*/) override { return True; }
    };
} // namespace nts::component
