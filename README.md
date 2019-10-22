# haskell-crawler

## Objective

Implement a web crawler (an http-endpoint, which accepts a list of URLs and returns a list of their titles, where title is something inside html title tag)

Some questions remain unanswered: What to do with errors, empty or absent titles, redirects, duplicate URLs and how many requests will be performed per unit of time. Those details could be implemented in any way.

## Set up

Install [Haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

## Run

```bash
# run and watch files
stack build --exec haskell-crawler-exe

# test it!
curl "http://localhost:8080/"\
  --header "Content-Type: application/json"\
  --request POST\
  --data '[
"https://twitter.com",
"https://google.com",
"https://ya.ru",
"https://yandex.ru",
"https://kremlin.ru",
"https://tutu.ru",
"https://s7.ru",
"https://technocity.ru",
"https://avito.ru",
"https://apple.ru",
"https://interfacelift.com",
"https://haskell.org",
"https://elm-lang.org",
"https://telegram.org",
"https://wikipedia.org",
"https://mozilla.com",
"https://samsung.com",
"https://benq.com",
"https://rambler.ru",
"https://zte.com",
"https://hp.com"
]'



```

## Other useful commands

```bash
# create an potimized build for production
stack build --ghc-options -O2

# locate an executable file after build
stack exec -- whereis haskell-crawler-exe

# copy an executable file to ~/.local/bin
stack build --copy-bins
```