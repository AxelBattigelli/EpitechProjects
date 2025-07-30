#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <cstdint>

namespace nts::component
{
    class Gate4094 : public AComponent {
      private:
        Tristate _old_clock = Tristate::Undefined;
        std::uint8_t _data = 0;
        Tristate _qs = Tristate::Undefined;
        Tristate _qps = Tristate::Undefined;

      public:
        void simulate(std::size_t tick) override;
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
