Unexpected behavior of `Debug.Trace`:

```
*DebugBlahWoot
λ> :l 29/DebugBlahWoot.hs
λ> main
outer trace
blah
blah
inner trace
woot
woot
*DebugBlahWoot
λ> main
blah
blah
woot
woot
```
