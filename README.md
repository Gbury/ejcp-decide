# ejcp-decide

## Dependencies

Needs:
- Ocaml 4.03.0
- yojson
- ppx_deriving yojson

Probably not compatible with Ocaml 4.02.3 because of changes
in ppx_deriving_yojson (use of the `result` type instead of polymorphic
variant for generated parsing functions). Use of one of the 4
official ocaml 4.03.0 switches (released on april, 25th) on opam is
therefore probably needed.

## Build and test

Build:

```
make
```

Run tests:

```
make test
```

Count the number of "YES":

```
make count
```
