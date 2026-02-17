<p align="center">
    <img src="images/icon.png">
</p>

<p align="center">
    <a href="https://github.com/PeterGriffinSr/lunno/releases"><img src="https://img.shields.io/github/release/PeterGriffinSr/lunno" alt="GitHub release"></a>
</p>

<div>&nbsp;</div>

Lunno is a purely functional language for building type-safe systems at scale!
For more information, see [the website](https://example.com/).

> [!WARNING]
> Lunno is in early development. APIs and language features may change without notice. No stable release is available yet.

## Building

Lunno is built with [OCaml](https://ocaml.org/) and [Dune](https://dune.build/). Dune handles all package dependencies automatically via the included dune.lock/ directory, so no additional package manager setup is required.

**Steps**

Clone the repository and navigate into it:

```sh
git clone https://github.com/PeterGriffinSr/lunno.git
cd lunno
```

Build the project - Dune will build all dependencies automatically:

```sh
dune build
```

To run the test suite:

```sh
dune test
```

## Support Lunno!