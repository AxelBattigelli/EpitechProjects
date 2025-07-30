#pragma once

#include "AComponent.hpp"
#include "Component/AndComponent.hpp"
#include "Component/NotComponent.hpp"
#include "IComponent.hpp"
#include <array>
#include <cstddef>

namespace nts::component
{
    class Gate4011 : public AComponent {
      private:
        std::array<AndComponent, 4> _andComponents;
        std::array<NotComponent, 4> _notComponents;

      public:
        Gate4011();
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
