#pragma once

#include "AComponent.hpp"
#include "Component/NotComponent.hpp"
#include "IComponent.hpp"
#include <array>
#include <cstddef>

namespace nts::component
{
    class Gate4069 : public AComponent {
      private:
        std::array<NotComponent, 6> _notComponents;

      public:
        Gate4069();
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
