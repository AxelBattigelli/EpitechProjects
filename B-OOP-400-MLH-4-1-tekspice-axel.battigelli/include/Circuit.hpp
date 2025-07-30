#pragma once

#include <cstddef>
namespace nts
{
    class Circuit;
}

#include "AComponent.hpp"
#include "CircuitParser.hpp"
#include "IComponent.hpp"
#include <unordered_map>
#include <istream>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace nts
{
    class Circuit : public AComponent {
      private:
        std::unordered_map<std::string, IComponent *> _inputs;
        std::map<std::string, IComponent *> _displays;
        std::vector<std::unique_ptr<IComponent>> _components;
        std::size_t _tick = 0;

        Circuit() = default;
        void addComponent(const std::string &, std::unique_ptr<IComponent>);
        friend nts::CircuitParser;

      public:
        Circuit(std::istream &);
        Tristate compute(size_t /*pin*/) override;
        bool display() const;
        bool changeInput(const std::string &, nts::Tristate);
        void simulate(std::size_t /*unused*/) override;
    };
} // namespace nts
