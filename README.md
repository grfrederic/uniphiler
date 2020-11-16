### Uniphiler: the compiler you love!

This is a compiler written in Prolog using DCG for the simple Instant language.
It compiles to both JVM and LLVM.


#### Usage
For JVM:

```
> ./insc_jvm path/to/source.ins
```

For LLVM:

```
> ./insc_llvm path/to/source.ins
```

The executables `insc_jvm` and `insc_llvm` are just wrappers for [swi-prolog](https://www.swi-prolog.org/) running the scripts `src/compile_jvm.prolog` and `src/compile_llvm.prolog` and then respectively [Jasmin](http://jasmin.sourceforge.net/) or [llvm-asm](https://llvm.org/docs/CommandGuide/llvm-as.html).
