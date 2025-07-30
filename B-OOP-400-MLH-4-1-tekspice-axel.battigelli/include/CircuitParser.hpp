#pragma once

#include "IComponent.hpp"
#include <istream>
#include <memory>
#include <string>
namespace nts
{
    class CircuitParser;
}

#include "Circuit.hpp"

namespace nts
{
    class CircuitParser {
      private:
        enum class State { Begin, Chipsets, Links } _state{State::Begin};
        static std::unique_ptr<nts::IComponent> createComponent(const std::string &);

      public:
        CircuitParser() = default;
        Circuit parse(std::istream &);
    };
} // namespace nts
