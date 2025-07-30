#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>

namespace nts::component
{
    class Gate4512 : public AComponent {
      private:
      public:
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
