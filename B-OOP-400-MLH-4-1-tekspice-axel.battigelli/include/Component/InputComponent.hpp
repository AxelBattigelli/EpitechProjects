#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>

namespace nts::component
{
    class InputComponent : public AComponent {
      private:
        Tristate _value = Undefined;
        Tristate _nextValue = Undefined;

      public:
        Tristate compute(std::size_t pin) override;
        void simulate(std::size_t tick) override;
        void setValue(Tristate);
    };
} // namespace nts::component
