# scrapeaming
*Search for videos on the web and sort them by length and quality
(work in progress).*

## Setup
Install the [Nix package manager](https://nixos.org/nix/):
```
curl https://nixos.org/nix/install | sh
```

## Build
```
./build.sh
```

## Run
```
./run.sh "Agent 327: Operation Barbershop"
```

Wait until at least one video is found, and press `CTRL-C` to see the
results.

Beware that this will spawn quite a lot of `chromium` and `ffprobe`
instances in parallel and in the background. Depending on your
hardware configuration this could considerably slows down your
system. You can adjust the level of parallelism by setting the
environment variable `SCRAPEAMING_JOBS` which defaults to
`2 * number of cores`.
