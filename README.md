# UPEM / L3 Info / Functional programming / Project: URM

Unlimited Register Machine in OCaml.

## Requirements

* ocaml (≥4.05.0)
* GNU Make
* [OCaml Makefile](https://mmottl.github.io/ocaml-makefile/)
* [kaputt](http://kaputt.x9c.fr/)


## Make targets

In the `src` directory:

* Build: `make nc` produces a binary named `urm`
* Test: `make test`
* Clean: `make clean`


## Usage

```
./urm <run | trace> <urm | eurm> <program file> <initial register state file>
```

Examples programs are provided in the `examples` folder.


## Authors

* Pacien TRAN-GIRARD
* Adam NAILI


## License

Project distributed under the terms of the Creative Commons BY-NC-SA 3.0 license.

