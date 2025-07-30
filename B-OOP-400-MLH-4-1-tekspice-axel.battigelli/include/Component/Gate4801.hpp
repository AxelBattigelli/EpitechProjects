#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <array>
#include <cstddef>
#include <cstdint>

namespace nts::component
{
    class Gate4801 : public AComponent {
      private:
        std::array<std::uint8_t, 1024> _data = {};

      public:
        Tristate compute(std::size_t pin) override;
        void simulate(std::size_t tick) override;
    };
} // namespace nts::component
