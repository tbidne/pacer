<div align="center">

# Pacer

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/pacer?include_prereleases&sort=semver)](https://github.com/tbidne/pacer/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/pacer/ci.yaml?branch=main)](https://github.com/tbidne/pacer/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/pacer?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

### Table of Contents
- [Introduction](#introduction)
- [Usage](#usage)
  - [Chart](#chart)
  - [Convert](#convert)
  - [Derive](#derive)
  - [Scale](#scale)
- [Building](#building)
  - [Cabal](#cabal)
  - [Nix](#nix)
- [FAQ](#faq)

# Introduction

Pacer is a CLI application that provides commands useful for runners.

# Usage

```
Pacer: A tool for runners.

Usage: pacer COMMAND

  Pacer includes several commands useful for runners, including the ability to
  generate graphic charts based on running data.

Available options:
  -h,--help                Show this help text

Available commands:
  chart                    Generates charts. Requires npm to be installed.

  convert                  Converts a quantity. Requires exactly one quantity
                           and the unit.

  derive                   Given two quantities, derives the third. For
                           instance, given a distance and a duration, derives
                           the pace.

  scale                    Scales a quantity. Requires exactly one quantity and
                           the scale factor.


Version: 0.1
```

## Chart

The `chart` command generates a `build` directory that contains an html file with charts.

```
build/
├── bundle.js
└── index.html

1 directory, 2 files
```

Charts are generated from two inputs:

- A `runs.toml` file, containing a list of all runs the user may want to chart.
- A `chart-requests.toml` file, that determines how to construct the chart(s). The html page will contain a chart for each request in the file.

> [!IMPORTANT]
>
> The `chart` command requires npm (nodejs) to be installed: https://nodejs.org/en/download

### Usage

```
Usage: pacer chart [-c|--clean] [-d|--data PATH] [-j|--json]

  Generates charts. Requires npm to be installed.


Available options:
  -c,--clean               If active, cleans prior build files.

  -d,--data PATH           Path to data directory i.e. where we search for
                           runs.toml and chart-requests.toml. If not given,
                           defaults to the XDG config e.g. ~/.config/pacer/.

  -j,--json                If active, stops after generating the intermediate
                           json file. Primarily used for testing.

  -h,--help                Show this help text
```

### Examples

Building the example here i.e.

```
$ pacer chart -d examples/
```

will generate an html page with several charts like this one:

![example_chart](examples/chart.png)

See the `examples/` director for more information.

## Convert

The `convert` command converts quanities to different units.

### Usage

```
Usage: pacer convert [--distance DIST_STR] [--pace TIME_STR (/UNIT)]
                     (-u|--unit UNIT)

  Converts a quantity. Requires exactly one quantity and the unit.


Available options:
  --distance DIST_STR      A distance with units e.g. '4 km'.

  --pace TIME_STR (/UNIT)  A pace e.g. '4m30s /km', '1h5m /mi'.

  -u,--unit UNIT           Output unit e.g. 'km', 'miles'.

  -h,--help                Show this help text
```

### Examples

```
$ pacer convert --distance 5km --unit mi
3.11 mi

$ pacer convert --pace '5m20s /mi' --unit km
3'19" /km
```

> [!WARNING]
>
> With convert, pace __must__ include units i.e. either /km or /mi. Furthermore, pace cannot be given in meters or converted to meters; only kilometers and meters are allowed.

## Derive

The `derive` command derives a third quantity from two others.

### Usage

```
Usage: pacer derive [--duration TIME_STR] [--pace TIME_STR [/UNIT]]
                    [--distance DIST_STR] [-u|--unit UNIT]

  Given two quantities, derives the third. For instance, given a distance and a
  duration, derives the pace.


Available options:
  --duration TIME_STR      A length of time e.g. '1h2m3s'.

  --pace TIME_STR [/UNIT]  A pace e.g. '4m30s /km', '1h5m /mi', '4m30'. If the
                           units are not given, we use the distance's units.
                           Only kilometers and miles are allowed.

  --distance DIST_STR      A distance with units e.g. '4 km'.

  -u,--unit UNIT           Output unit e.g. 'km', 'miles'.

  -h,--help                Show this help text
```

### Examples

```
# derives the pace from distance and duration
$ pacer derive --distance marathon --duration 3h15m
4'37" /km

# reverse of the above (slight difference due to rounding)
$ pacer derive --distance marathon --pace 4m37s
3h 14'48"

# we can also convert the final result to a different unit
$ pacer derive --distance marathon --duration 3h15m -u mi
7'26" /mi

$ pacer derive --pace '5m45s /mi' --duration 20m
3.48 mi
```

> [!NOTE]
>
> Builtin values like 'marathon' assume unit `kilometers`, if nothing else is given.

## Scale

The `scale` command is used for scaling a single quantity.

### Usage

```
Usage: pacer scale [--duration TIME_STR] [--pace TIME_STR [/UNIT]]
                   [--distance DIST_STR] [-u|--unit UNIT] (-k|--factor POS_INT)

  Scales a quantity. Requires exactly one quantity and the scale factor.


Available options:
  --duration TIME_STR      A length of time e.g. '1h2m3s'.

  --pace TIME_STR [/UNIT]  A pace e.g. '4m30s /km', '1h5m /mi', '4m30'.

  --distance DIST_STR      A distance with units e.g. '4 km'.

  -u,--unit UNIT           Output unit e.g. 'km', 'miles'.

  -k,--factor POS_INT      The scaling factor.

  -h,--help                Show this help text
```

### Examples

```
$ pacer scale --distance marathon -k 0.5
21.10 km

# we can convert the final result
$ pacer scale --distance marathon -k 0.5 -u miles
13.11 mi

$ pacer scale --pace '4m15s /km' -k 1.1
4'40" /km

# pace units are optional
$ pacer scale --pace 4m15s -k 1.1
4'40"
```

> [!WARNING]
>
> With scale, pace __must__ include units if the final result is converted.

```
$ pacer scale --pace '4m15s' -k 1.1 -u mi
Scaling pace with --unit requires that the original units are given e.g. --pace '4m15s /km'.
```

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`cabal 2.4+`](https://www.haskell.org/cabal/download.html)
* One of:
  * [`ghc 9.10`](https://www.haskell.org/ghc/download.html)

The easiest way to install these is generally [`ghcup`](https://www.haskell.org/ghcup/).

The current "blessed" version is `ghc-9.10.1`.

### Build Pacer

Once you have `cabal` and `ghc`, `pacer` can be built locally with `cabal build` or installed globally (e.g. `~/.local/bin/pacer`) with `cabal install`.

## Nix

### Prerequisites

* [nix](https://nixos.org/download.html)

### Manually

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `pacer` can be built with `nix build`, which will compile and run the tests.

### Nix expression

Because `pacer` is a flake, it can be built as part of a nix expression. For instance, if you want to add `pacer` to `NixOS`, your `flake.nix` should have:

```nix
# flake.nix
{
  inputs.pacer.url = "github:tbidne/pacer/main";
}
```

Then include this in the `systemPackages`:

```nix
# wherever your global packages are defined
{
  environment.systemPackages = [
    pacer.packages."${system}".default
  ];
}
```

# FAQ

See [faq.md](documentation/faq.md).
