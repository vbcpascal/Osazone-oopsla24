# Osazone 

Artifact for OOPSLA'24: Semantics Lifting for Syntactic Sugar

The updates of this project can be found on [GitHub](https://github.com/vbcpascal/Osazone-core/).

## Introduction

Our paper (Semantics Lifting for Syntactic Sugar) proposes a systematic framework to lift semantics for syntactic sugars. According to the definition (semantics) of the host language and syntactic sugars to define DSL, we can obtain a standalone, host-independent semantics of DSL for free. The artifact Osazone is a prototype of the DSL development framework, to support the semantics-lifting algorithm. TODO

```
Define a language straightforwordly:

Define a language via syntactic sugars:

```

We are applying for the *Reusable* badge. We believe that the Osazone system can support the main claims of the paper and the code structure is clear and reusable.

We have recorded a brief video (TODO) to show how to define syntactic sugars and lift semantics for DSL in Osazone, and a detailed video to demonstrate all the DSL examples in the "Evaluation" section. These instructions can be found in Sec. TODO.

If reviewers have any problems, please contact us on the artifact submission system and we are always available to resolve all problems.



## Installation

The artifact *Osazone* can be built from the source or used in a docker container.

### Build from source (Test on Windows 11)

1. Install dependencies. This project requires stack and ghc (test on cabal 3.10.2.1, ghc 9.6.3 and stack 2.15.3 with resolver LTS 22.6)

2. Clone Osazone
```
$ git clone https://github.com/vbcpascal/Osazone-oopsla24
```

3. Build the whole project under the root directory of the project.
```
$ stack build
```

### Download docker image

TODO

### Run tests

Test whether *Osazone* is successfully installed:

```
$ stack run version
```

The expected output is as follows:
```
Osazone 0.1.0
standard library (in path lib) 0.1.0 
```

## Usage

Osazone supports two dimensions of language definition: straightforwardly and via syntactic sugars. The latter is the focus of our paper.

- **Straightforwardly.** Input: abstract syntax and semantics of the language; Output: the interpreter of the language (Haskell code).
- **Via syntactic sugars.** Input: an existing language definition and syntactic sugars; Output: a new language definition.


The expected output is a new directory named `build` in `./example/Host/LC`.
