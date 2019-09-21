# haskell-crawler

## Set up

Install [Haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

## Run

```bash
# run and watch files
stack build --file-watch --exec haskell-crawler-exe
```

## Useful commands

```bash
# locate an executable file after build
stack exec -- whereis haskell-crawler-exe

# copy an executable file to ~/.local/bin
stack build --copy-bins
```