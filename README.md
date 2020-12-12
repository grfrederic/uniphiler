### Uniphiler: the compiler you love!

This is a compiler written in Prolog using DCG for the language Latte (for now only front end).


#### Usage

```
> ./latc path/to/source.lat
```

The executable `latc` is just a wrapper for [swi-prolog](https://www.swi-prolog.org/) running the scripts `src/check_latte.prolog`.
