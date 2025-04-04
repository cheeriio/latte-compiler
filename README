*Latte* compiler.
Compile using
```
make
```

This generates `latc_x86_64` executable. Example usage:
```
latc_x86_64 tests/good/core001.lat
```

Project was written fully in Haskell. Files `AbsLatte.hs`, `ParLatte.hs`,
`LexLatte.hs`, `ErrM.hs`were generated using `bnfc` and `alex`, `happy`
compilers.

File 'Typecheck.hs' contains code responsible mostly for typechecking but also
simple stuff as ensuring main function is present or built-in functions are
not overwritten.

File 'Compiler.hs' contains code for simplifying the structure and ensuring
control reaches return in every function. It changes
```
if(true) {
    B
}
```
into
```
B
```
and such.

File 'Generation.hs' emits the assembly code. The code is not very efficient
and treats the processor more like a stack machine rather than a register
machine. Register allocation using graph coloring methods is planned...

File 'runtime.c' contains definitions of built in and internal functions.
