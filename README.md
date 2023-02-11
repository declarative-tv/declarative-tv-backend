TODO
---

- [x] Configuration which can be overridden by environment variables
  - Used [this library][envy]
- [ ] Add co-log logging
- [ ] Set up postgresql via postgres-simple
- [ ] Set up opentelemetry with honeycomb
- [ ] Handle CSRF
- [ ] Get websockets working

Documentation
---

- [ ] Write up basics for type level components to understand servant
- [ ] Write up basics for employing the "ReaderT pattern"

Random Notes
---

- Add this to `.envrc` (from [Production Haskell][production-haskell]
  - `export GHC_OPTIONS='-j4 +RTS -A128m -n2m -qg -RTS'`

[production-haskell]: https://leanpub.com/production-haskell
[envy]: https://hackage.haskell.org/package/envy
