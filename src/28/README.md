Profiling

```bash
$ stack ghc -- -O2 bench.hs
```

```bash
$ stack ghc -- -prof -fprof-auto -rtsopts -O2 profile.hs
$ ./profile +RTS -P
$ cat profile.prof
```

```bash
$ ghc -prof -fprof-auto -rtsopts -O2 loci.hs
$ ./loci +RTS -hc -p
$ hp2ps loci.hp
```
