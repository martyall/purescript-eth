# chanterelle
[![Chanterelle example tests](https://github.com/martyall/purescript-eth/actions/workflows/test-chanterelle-example.yml/badge.svg)](https://github.com/martyall/purescript-eth/actions/workflows/test-chanterelle-example.yml)

<img src=https://github.com/martyall/purescript-eth/blob/main/chanterelle/chanterelle-logo.svg width="150">

_a more functional truffle_

## Overview

This project is meant to be a replacement to `truffle`. You can use it for development, deployment and testing of solidity smart contracts in purescript. For an example project, see the [chanterelle-halogen-template](https://github.com/f-o-a-m/chanterelle-halogen-template). We also recommend [cliquebait](https://github.com/f-o-a-m/cliquebait) as a replacement for `testrpc`.

## Docs

You can find our [documentation](https://chanterelle.readthedocs.io/en/latest/) hosted on ReadTheDocs.

### Project structure

Chanterelle includes a quasi-package-oriented project structure, to be used more thoroughly in the future when the Ethereum ecosystem settles on a contract package management system.
This is embodied in the `chanterelle.json` file which should be located in the
root of your project. It also supports the notion of dependencies (very rudimentary at the moment), as well as automatically generates PureScript bindings for your contracts using `purescript-web3-generator`.
