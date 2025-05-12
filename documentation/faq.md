# FAQ

---

### Table of Contents


- [Charts](#charts)
  - [How does file discovery work?](#how-does-file-discovery-work)
  - [How do I use this with Garmin](#how-do-i-use-this-with-garmin)
  - [How do chart filters work?](#how-do-chart-filters-work)
- [Running](#running)
  - [What is NPM, and why do I need it?](#what-is-npm-and-why-do-i-need-it)
- [Misc](#misc)
  - [What units are available?](#what-units-are-available)

## Charts

### How does file discovery work?

The most explicit way to generate charts is to use the `--chart-requests` and `--runs` arguments. If these are not given, then we search for expected filenames in the following order:

- If `--data <dir>` was given:
  - `<dir>/<expected_filename(s)>`.
- If json config exists (explicit `--config` or found in `<xdg_config>` location):
  - `<config.path_type>`
  - `<config.data>/<expected_filename(s)>`
- `<xdg_config>/<expected_filename(s)>` (e.g. `~/.config/pacer/expected_filename(s)`).

In particular:

#### chart-requests

The only "expected filename" here is `chart-requests.json`, so this works out to be:

- If `--data <dir>` was given:
  - `<dir>/chart-requests.json`.
- If json config exists:
  - `<config.chart-requests>`.
  - `<config.data>/chart-requests.json`.
- `<xdg_config>/chart-requests.json`

#### runs

On the other hand, runs have two possible "expected filenames":

- `runs.json` (custom format)
- `Activities.csv` (garmin)

Therefore this works out to be:

- If `--data <dir>` was given:
  - `<dir>/runs.json`.
  - `<dir>/Activities.csv`.
- If json config exists:
  - `<config.runs>`.
  - `<config.data>/runs.json`.
  - `<config.data>/Activities.csv`.
- `xdg_config/runs.json`
- `xdg_config/Activities.csv`.

> [!TIP]
>
> If `runs.json` and `Activities.csv` exist in the same directory then we will combine them. Note that file discovery is case-insensitive e.g. we will also find `activities.csv`. Also, whenever we search for `<file>.json`, we also search for `<file>.jsonc`.

### How do I use this with Garmin?

In addition to the custom `runs.json` format, we provide integration with garmin's `Activities.csv` file that can be downloaded from the website: https://connect.garmin.com/modern/activities.

There are some caveats:

- The `Activities.csv` file does not specify the units; it uses whatever the setting is on your device (e.g. watch). But we need to know what the units are, so we require this to be set in the `chart-requests.json` file:

    ```json
    {
      "garmin": {
        "unit": "km"
      }
    }
    ```

    Note that garmin only supports kilometers and miles.

- Filtering by `labels` with garmin activities is a bit more involved. That is, with the custom `runs.json` format, we can label a run like:

    ```jsonc
    // runs.json
    {
      "datetime": "2024-10-25T12:00:00",
      "distance": "marathon",
      "duration": "3h20m",
      "labels": ["official", "marathon"] // custom labels
    }
    ```

    Then in the `chart-requests.json` we can filter on this label to take only runs with this label:

    ```jsonc
    // chart-requests.json
    {
      "title": "Marathons",
      "filters": ["label = marathon"],
      "y-axis": "duration"
    }
    ```

    Garmin `Activities.csv` files do not include any fields where we can add our labels, so instead we specify the labels in a separate file:

    ```jsonc
    // run-labels.json
    {
      "datetime": "2024-10-25 12:00:00",
      "labels": ["official", "marathon"]
    }
    ```

    If this matches a `Date` in our `Activities.csv`:

    ```csv
    Activity Type,Date,Favorite,Title,Distance,Calories,Time,Avg HR,Max HR,Aerobic TE,Avg Run Cadence,Max Run Cadence,Avg Pace,Best Pace,Total Ascent,Total Descent,Avg Stride Length,Avg Vertical Ratio,Avg Vertical Oscillation,Avg Ground Contact Time,Avg GAP,Normalized Power® (NP®),Training Stress Score®,Avg Power,Max Power,Steps,Decompression,Best Lap Time,Number of Laps,Moving Time,Elapsed Time,Min Elevation,Max Elevation
    Running,2024-10-25 12:00:00,false,"Wellington Running","3.77","220","00:18:14","151","171","3.5","155","173","4:50","3:18","45","29","1.33","7.9","10.5","263","4:45","412","0.0","406","663","2,862","No","00:03:47.9","4","00:17:48.5","00:20:38","4","48"
    ```

    Then the label will be attached to the run, and we can later filter on it in the `chart-requests`.

- We assume files that contain the name `garmin` or end with `.csv` are garmin activities files. Otherwise we assume `.json` format.

### How do chart filters work?

Chart requests allow us to filter runs based on some criteria. In general, `filters` contains a list of filters, where a run must satisfy all filters to be included. We have the following filter "atoms":

- `label <label_op> <lbl>`: The run must satisfy the `labels` condition (case-sensitive).
- `datetime <op> <val>`: The run's `datetime` field must satisfy the condition e.g. `datetime > 2019`.
- `distance <op> <val>`: The run must have `distance <op> <val>` e.g. `distance > 5 km`.
- `duration <op> <val>`: The run must have `duration <op> <val>` e.g. `duration < 2h`.
- `pace <op> <val>`: The run must have `pace <op> <val>` e.g. `pace <= 4m30s /km`.

`<label_op>` can be one of: `=`, `/=`.

`<op>` can be one of: `<=`, `<`, `=`, `/=`, `>=`, `>`.

Filters also support basic boolean logic i.e.

- `not expr`: `expr` must be false.
- `expr1 or expr2`: `expr1` and/or `expr2` must be true.
- `expr1 and expr2`: `expr1` and `expr2` must be true.
- `expr1 xor expr2`: Exactly one of `expr1` and `expr2` must be true.

where `expr` is either an atom or another expression.

For example, we can have:

```jsonc
{
  "title": "Races and long runs",
  "filters": [
    "label = official_race or label = marathon",
    "label /= casual",
    "distance >= 25 km",
    "datetime > 2024"
  ],
  "y-axis": "distance"
}
```

In this case, we will take all runs that satisfy **all** of the following criteria:

- Has label `official_race` and/or `marathon`.
- Does _not_ have label `casual`.
- Has `distance >= 25 km`.
- Has `datetime > 2024` (i.e. 2025 onwards).

> [!TIP]
>
> Technically, there is no need to have a separate `expr1 and expr2` expression, as we can encode `and` with multiple filters. Nevertheless we include the redundant functionality, as it may make some expressions simpler.

## Running

### What is NPM, and why do I need it?

[NPM](https://en.wikipedia.org/wiki/Npm) is the "node package manager", and the `chart` command requires it to be installed, since we use `node` to build the charts. See https://nodejs.org/en/download for installation instructions.

To test installation, verify that `npm -v` works and returns some version:

```
# The version does not necessarily have to be the same.
$ npm -v
10.9.0
```

## Misc

### What units are available?

Pacer defines several quantities that require the user to specify the units. In general, we have:

- **distance:** `meters`, `kilometers`, and `miles`. Short versions can also be specified: `m`, `km`, and `mi`. There are also several built-in values:

  - `marathon`
  - `half-marathon` or `hmarathon`

  Note that these do not take in a unit. The unit will be based on the context, and if it is ambiguous, defaults to `kilometers`.

- **duration:** "time strings" like `1h2m3s` for `1 hour, 2 minutes, and 3 seconds`. Each component is optional e.g. `1h3s` or `2m`.

- **pace:** `<duration> /<distance>` e.g. `5m30s /km`. Note that `meters` are not allowed with paces; only `kilometers` or `miles`. In cases where the context is clear, the trailing unit is not required. Also note that because paces are always given in terms of `1 km` or `1 mi`, the full versions are *singular*, not plural e.g. `4m /kilometer`.

Parsing is designed to be moderately flexible, so numeric values can optionally be separated from the units by whitespace (quotes are then required) e.g. `'4 km'` or `4km`.

#### Examples

```
--distance 5m
--distance '42 km'
--distance '42km'
--distance marathon
--distance '10 miles'
--distance '10 mi'

--duration 1h2m3s
--duration 4m30s
--duration 300s

--pace '4m20s /km'
--pace 5m/mi

# Pace unit can be omitted when it can be inferred from context e.g. no need
# to specify 'km' twice here:
#
$ pacer derive --distance 42km --pace 5m
3h 30'00"

# The pace unit is also not required when it is irrelevant e.g. when
# scaling:
#
$ pacer scale --pace 5m -k 0.9
4'30"
```
