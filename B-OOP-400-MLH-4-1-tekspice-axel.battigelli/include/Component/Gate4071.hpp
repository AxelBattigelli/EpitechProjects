#pragma once

#include "AComponent.hpp"
#include "Component/OrComponent.hpp"
#include "IComponent.hpp"
#include <array>
#include <cstddef>

namespace nts::component
{
    class Gate4071 : public AComponent {
      private:
        std::array<OrComponent, 4> _orComponents;

      public:
        Gate4071();
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
