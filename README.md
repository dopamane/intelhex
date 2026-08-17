# [Intel HEX](https://en.wikipedia.org/wiki/Intel_HEX)

Usage

```
ihex --help
cat foo.hex | ihex -r > foo.bin
cat bar.bin | ihex -w > bar.hex
```

Development

```
cabal build
cabal run ihex
```
