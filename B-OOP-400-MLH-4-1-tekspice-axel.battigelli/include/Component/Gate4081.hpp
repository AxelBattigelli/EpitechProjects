#pragma once

#include "AComponent.hpp"
#include "Component/AndComponent.hpp"
#include "IComponent.hpp"
#include <array>
#include <cstddef>

namespace nts::component
{
    class Gate4081 : public AComponent {
      private:
        std::array<AndComponent, 4> _andComponents;

      public:
        Gate4081();
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
