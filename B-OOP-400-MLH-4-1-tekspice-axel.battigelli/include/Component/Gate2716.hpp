#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <cstdint>
#include <vector>

namespace nts::component
{
    class Gate2716 : public AComponent {
      private:
        std::vector<std::uint8_t> _data;

      public:
        Gate2716();
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
