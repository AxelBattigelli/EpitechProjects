#pragma once

#include "AComponent.hpp"
#include "Component/NotComponent.hpp"
#include "Component/OrComponent.hpp"
#include "IComponent.hpp"
#include <array>
#include <cstddef>

namespace nts::component
{
    class Gate4001 : public AComponent {
      private:
        std::array<OrComponent, 4> _orComponents;
        std::array<NotComponent, 4> _notComponents;

      public:
        Gate4001();
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
