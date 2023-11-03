# OCaml Compiler Project

Welcome to the OCaml Compiler Project. This is a fully functioning compiler written in OCaml that includes lexing, parsing, semantic analysis, and code generation phases. This project is designed to be built using Dune and comes with a convenient Makefile for ease of use.

## Getting Started

To get started with the OCaml Compiler, you need to have OCaml and Dune installed on your system. If you do not have them installed, follow the instructions on the OCaml [official website](https://ocaml.org) and the Dune [GitHub page](https://github.com/ocaml/dune) to install them.

### Prerequisites

Make sure you have the following installed:

- OCaml
- Dune

### Installing

Clone the repository to your local machine:

```bash
git clone https://github.com/owebb1/ocaml-compiler.git
```

Then, change into the project directory:

```bash
cd ocam-compiler
```

### Building the Compiler

To build the compiler, you can use the provided Makefile. Just run:

```bash
make build
```

This will create the necessary output directories, compile the source code using Dune, and link the hatch and tests executables.

### Running Tests

After building, you can run the test suite to verify the functionality of the compiler:

```bash
make test
```

### Cleaning Build Artifacts

To clean up the build artifacts and reset the project directory:

```bash
make clean
```

### Usage

After building the project, you can use the hatch executable to compile your source files. Here's the basic usage:

```bash
./hatch [options] <filename>
```

Replace `<filename>` with the path to your OCaml source file.

### Contributing

If you'd like to contribute to the OCaml Compiler Project, please fork the repository and create a pull request with your changes.

### License

This project is licensed under the [LICENSE] - see the LICENSE.md file for details.

### Acknowledgments

Thanks to the faculty and staff at Swarthmore College for providing the project scaffolding.

### Support

If you encounter any problems or have any questions, please open an issue on the repository's issue tracker.
