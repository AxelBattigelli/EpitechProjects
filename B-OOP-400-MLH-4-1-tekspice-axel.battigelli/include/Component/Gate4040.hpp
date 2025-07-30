#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <cstdint>

namespace nts::component
{
    class Gate4040 : public AComponent {
      private:
        Tristate _old_clock = Tristate::Undefined;
        std::uint16_t _data = 0;

      public:
        void simulate(std::size_t tick) override;
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
