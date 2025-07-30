#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>

namespace nts::component
{
    class NotComponent : public AComponent {
      public:
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
