TODO
---

- [x] Configuration which can be overridden by environment variables
  - Used [envy][this library]
- [x] Add [katip][katip] logging
  - Used [katip-setup][these directions]
- [x] Use [annotated-exception][annotated-exception] to make exceptions easier
  - See `src/Api.hs` for an example
- [x] Using [esqueleto][esqueleto] with [persistent][persistent]
  - [x] Setup a `runDB` function
- [ ] Twitch authentication/webhooks
- [ ] YouTube authentication/webhooks
- [ ] Forward go-live notifications to websocket
- [ ] Handle CSRF
- [ ] Set up opentelemetry with honeycomb

Documentation
---

- [ ] Write up basics for type level components to understand servant
- [ ] Write up basics for employing the "ReaderT pattern"

Random Notes
---

- Add this to `.envrc` (from [Production Haskell][production-haskell]
  - `export GHC_OPTIONS='-j4 +RTS -A128m -n2m -qg -RTS'`
  - `export DECLARATIVE_POSTGRES_USER=$USER` may be needed

[annotated-exception]: https://hackage.haskell.org/package/annotated-exception
[envy]: https://hackage.haskell.org/package/envy
[katip]: https://github.com/Soostone/katip
[katip-setup]: https://github.com/Soostone/katip/blob/master/katip/examples/example.hs
[production-haskell]: https://leanpub.com/production-haskell
[esqueleto]: https://github.com/bitemyapp/esqueleto
[persistent]: https://github.com/yesodweb/persistent
