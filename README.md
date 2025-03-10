# ANTLR4 OCaml Grammar

This repository contains ANTLR4 grammar files for parsing OCaml language syntax.

## Files

- `OCamlLexer.g4`: Lexer rules for OCaml syntax.
- `OCamlParser.g4`: Parser rules for OCaml syntax.

## Usage

Generate parser and lexer using ANTLR4:

```bash
antlr4 -Dlanguage=Java OCamlLexer.g4 OCamlParser.g4
