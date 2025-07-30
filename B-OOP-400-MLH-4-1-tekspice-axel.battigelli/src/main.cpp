#include "CLIParser.hpp"
#include "Circuit.hpp"
#include "Exception.hpp"
#include <csignal>
#include <cstring>
#include <exception>
#include <fstream>
#include <iostream>
#include <ostream>
#include <typeinfo>

int main(int argc, char *argv[])
{
    for (int i = 0; i < argc; ++i) {
        if (std::strcmp(argv[i], "-h") == 0 || std::strcmp(argv[i], "--help") == 0) {
            std::cout << "Man nanotekspice\n"
                      << "\tRun an electonic circuit on your cumputer\n"
                      << "Usage :\n"
                      << "\tnanotekspice [FILE.nts]\n";
            return 0;
        }
    }
    try {
        if (argc < 2)
            throw nts::Exception("Bad usage");
        std::ifstream file(argv[1]);
        nts::Circuit circuit(file);
        nts::CLIParser const cli(circuit);
    } catch (const nts::Exception &e) {
        std::cerr << "Error: " << e << '\n';
        return 84;
    } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what()
                  << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
        return 84;
    } catch (...) {
        std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                  << std::current_exception().__cxa_exception_type()->name() << '\n';
        return 84;
    }
}
