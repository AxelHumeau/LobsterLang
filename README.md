# LobsterLang

[![release](https://img.shields.io/github/v/release/AxelHumeau/LobsterLang?sort=semver)](https://github.com/AxelHumeau/LobsterLang/releases)
[![Documentation](https://img.shields.io/static/v1?label=Documentation&message=reference&color=blue)](https://axelhumeau.github.io/LobsterLang/)
[![Tests](https://github.com/AxelHumeau/LobsterLang/actions/workflows/Tests.yml/badge.svg)](https://github.com/AxelHumeau/LobsterLang/actions/workflows/Tests.yml)

LobsterLang is a mix of a imperative and functionnal language with a specific syntax, all of the brackets is defined with a '|' that correspond to a lobster claw.

### Build GLaDOS
To build the project, use the Makefile command:
```bash
make
```

### Launch LobsterLang Interpreter
To launch the LobsterLang interpreter, all you need to do is to launch the binary 'glados' without any argument
```bash
./glados
```

### Launch the LobsterLang Compiler
To launch the LobsterLang compiler, you can precise multiple flags.
By default, the file will be compiled and interpreted by the Virtual Machine
The file compiled will be in the format `filename`.o
```bash
./glados [-flags] `filename`
```

### Flags
```
    -c      compiling the file
    -e      interpretate the compiled file
```
