# Jetpack Game documentation

## Introduction

This documentation describes the communication system between clients and the server for the Jetpack game. The communication relies on a simple packet-based protocol.

## Architecture of the project

The project is split in two distinct parts : client and server.
The Exeception.hpp is share between the two code.

## Communication structure

You can refer to [this documentation](./doc.txt) to get more details about the communication protocole we use.

## Compilation with Makefile

The project uses a `Makefile` to compile the source files into an executable. The compilation process is simplified through the use of explicit compilation rules within the `Makefile`.

- **make**: compile client and server
- **make clean**: delete objects files
- **make fclean**: clear all data use during and for compilation

## Documentation

We use Doxygen to generate the documentation. To generate the documentation, you can use the following command:

```bash
doxygen Doxyfile
```

Once the documentation is generated, you can open the `docs/html/index.html` file.

## Authors

- [Mael Bertocchi](https://github.com/mael-bertocchi)
- [Enzo Lorenzini](https://github.com/Enzolorenzini)
- [Axel Batigelli](https://github.com/AxelBattigelli)
