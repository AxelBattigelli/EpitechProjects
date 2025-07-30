#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>

namespace nts::component
{
    class FalseComponent : public AComponent {
      public:
        Tristate compute(std::size_t /*pin*/) override { return False; }
    };
} // namespace nts::component
