#include "CLIParser.hpp"
#include "Circuit.hpp"
#include "IComponent.hpp"
#include <atomic>
#include <csignal>
#include <cstring>
#include <iostream>
#include <ostream>
#include <string>

nts::CLIParser::CLIParser(Circuit &circuit)
{
    bool res = true;
    size_t tick = 1;
    std::string line;
    while (res && std::cout << "> " && std::getline(std::cin, line)) {
        const std::string &s = line;
        if ("exit" == s) {
            res = false;
            continue;
        }
        if ("display" == s) {
            res = circuit.display();
            continue;
        }
        if ("simulate" == s) {
            circuit.simulate(tick);
            tick++;
            continue;
        }
        if ("loop" == s) {
            static std::atomic_bool stop = false;
            auto old = signal(SIGINT, [](int) { stop = true; });
            while (res && !stop) {
                circuit.simulate(tick);
                res = circuit.display();
                tick++;
            }
            stop = false;
            signal(SIGINT, old);
            continue;
        }
        size_t const pos = line.find('=');
        if (pos != std::string::npos) {
            std::string const name = line.substr(0, pos);
            std::string const valueStr = line.substr(pos + 1);
            nts::Tristate const value = (valueStr == "1") ? nts::Tristate::True
                : (valueStr == "0")                       ? nts::Tristate::False
                                                           : nts::Tristate::Undefined;
            res = circuit.changeInput(name, value);
        }
    }
}
