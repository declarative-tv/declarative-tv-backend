TODO
---

- [x] Configuration which can be overridden by environment variables
  - Used [this library][envy]
- [x] Add [co-log][co-log] logging
  - Used [these directions][co-log-custom]
- [x] Use [annotated-exception][annotated-exception] to make exceptions easier
  - See `src/Api.hs` for an example
- [x] Set up postgresql via postgres-simple
  - [ ] Get more familiar with postgres-simple
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
[co-log]: https://hackage.haskell.org/package/co-log
[co-log-custom]: https://github.com/co-log/co-log/blob/main/tutorials/2-custom/Custom.md
[annotated-exception]: https://hackage.haskell.org/package/annotated-exception
