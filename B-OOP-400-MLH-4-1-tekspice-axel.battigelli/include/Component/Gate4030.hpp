#pragma once

#include "AComponent.hpp"
#include "Component/XorComponent.hpp"
#include "IComponent.hpp"
#include <array>
#include <cstddef>

namespace nts::component
{
    class Gate4030 : public AComponent {
      private:
        std::array<XorComponent, 4> _xorComponents;

      public:
        Gate4030();
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
