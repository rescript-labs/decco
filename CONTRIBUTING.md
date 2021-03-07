# Contributing to this repository

When contributing to this repository, please first discuss the change you wish to make via issue or any other method with the owners of this repository before making a change.

## Before you get started

### PPXs

Writting [ppxs](https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem) isn't the easier to work with, more if it's the first time. Please ask any question, you can use an issue or go to the Discord https://discord.gg/SmsJyCdH2h.

### Requirements

There's a few system requirements that your enviroment needs to have started:

- [Node](https://nodejs.org/en) and [yarn](https://yarnpkg.com)
- [esy](https://esy.sh)

### Developing

- git clone https://github.com/reasonml-labs/decco
- cd decco
- yarn install
- cd ppx_src; esy;
- # Make your changes, commits, etc
- Open the PR

### Scripts

In order to see what are the scripts available on the repository, run `yarn run`. It will render a list of available commands in order to build the project, build the ppx, etc.

### Editor support with ocaml-lsp

> Note: Wasn't able to install ocaml-lsp-server with esy, instead can use opam.

`ocaml-lsp-server` is only needed for development.

You would need to have [opam](https://opam.ocaml.org) installed.

- cd ppx_src;
- opam switch create . 4.06.0 --deps-only # If it's the first time you create a switch from 4.06, it can take a while.
- eval $(opam env)
- opam install -y ocaml-lsp-server dune ocaml ppx_tools_versioned reason # Install ppx's dependencies inside the switch
- Profit, this should make your editor a little more smart

### Testing

Unit testing is done in BuckleScript and bs-jest, lives under `test/__tests__`.

A trick to see the output of the ppx is running bsc directly on the terminal:

```
npx bsc -bs-package-name decco -bs-package-output commonjs:lib/js/test/__tests__ -I test/__tests__ -I test -I src -I ./node_modules/@glennsl/bs-jest/lib/ocaml -ppx ./ppx -w +A-9-40-42 -dsource -bs-super-errors FILE_I_WANT_TO_COMPILE
```

For example:

```
npx bsc -bs-package-name decco -bs-package-output commonjs:lib/js/test/__tests__ -I test/__tests__ -I test -I src -I ./node_modules/@glennsl/bs-jest/lib/ocaml -ppx ./ppx -w +A-9-40-42 -dsource -bs-super-errors test/__tests__/test.re
```
