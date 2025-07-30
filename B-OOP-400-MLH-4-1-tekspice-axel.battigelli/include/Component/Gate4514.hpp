#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <cstdint>

namespace nts::component
{
    class Gate4514 : public AComponent {
      private:
        std::uint16_t _value;

      public:
        void simulate(std::size_t tick) override;
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
