# FAQ

---

### Table of Contents

- [General](#general)
  - [How are units specified?](#how-are-units-specified)
- [Charts](#charts)
  - [How does file discovery work?](#how-does-file-discovery-work)
  - [How do I use this with Garmin](#how-do-i-use-this-with-garmin)
  - [How do chart filters work?](#how-do-chart-filters-work)

## General

### How are units specified?

Pacer defines several quantities that require the user to specify the units. We have:

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

#### Grammar

A pseudo [formal grammar](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) for our units is:

```
; Distance is a number and unit.
distance : numeric distance-unit

distance-unit
  : meters
  | kilometers
  | miles

meters
  : 'm'
  | 'meters'

kilometers
  : 'km'
  | 'kilometers'

miles
  : 'mi'
  | 'miles'

; Duration is some combination of components, where at least one is required.
; A plain integer without any trailing unit is interpreted as seconds.
duration
  : days    [hours]   [minutes] [seconds]
  | hours   [minutes] [seconds]
  | minutes [seconds]
  | seconds
  | digits

days    : digits 'd'
hours   : digits 'h'
minutes : digits 'm'
seconds : digits 's'

; Pace is a number followed by the unit.
pace
  : numeric '/km'
  | numeric '/kilometer'
  | numeric '/mi'
  | numeric '/mile'

numeric
  : digits
  | digits '.' digits
```

## Charts

### How does file discovery work?

The most explicit way to generate charts is to use the `--chart-requests` and `--activities` arguments. If these are not given, then we search for expected filenames in the following order:

- If `--data <dir>` was given:
  - `<dir>/<expected_filename(s)>`.
- If json config exists (explicit `--config` or found in `<xdg_config>` location):
  - `<config.path_type>`
  - `<config.data>/<expected_filename(s)>`
- `<current_directory>/<expected_filenames(s)`
- `<xdg_config>/<expected_filename(s)>` (e.g. `~/.config/pacer/expected_filename(s)`)

In particular:

#### chart-requests

The only "expected filename" here is `chart-requests.json`, so this works out to be:

- If `--data <dir>` was given:
  - `<dir>/chart-requests.json`.
- If json config exists:
  - `<config.chart-requests>`.
  - `<config.data>/chart-requests.json`.
- `<current_directory>/chart-requests.json`
- `<xdg_config>/chart-requests.json`

#### activities

On the other hand, we search for activities for any file with

- String `activities` (case-insensitive) somewhere in the name.
- File extensions `csv`, `json`, or `jsonc`.

For instance, `activities.json` or `Some Activities Garmin.csv`.

Discovery works out to be:

- If `--data <dir>` was given:
  - `<dir>/<matches>`.
- If json config exists:
  - `<config.activities>`.
  - `<config.data>/<matches>`.
- `<current_directory>/<matches>`
- `<xdg_config>/<matches>`

#### activity-labels

- If `--data <dir>` was given:
  - `<dir>/activity-labels.json`.
- If json config exists:
  - `<config.activity-labels>`.
  - `<config.data>/activity-labels.json`.
- `<current_directory>/activity-labels.json`
- `<xdg_config>/activity-labels.json`

> [!TIP]
>
> - If multiple activity files exist in the same directory (e.g. `activities.json` and `Activities.csv`), then we will combine them.
> - File discovery is case-insensitive e.g. we will also find `activities.csv`.
> - Whenever we search for `<file>.json`, we also search for `<file>.jsonc`.
> - Hyphens and underscores are interchangeable e.g. we search for `chart-requests` and `chart_requests`.

### How do I use this with Garmin?

In addition to the custom `activities.json` format, we provide integration with garmin's `Activities.csv` file that can be downloaded from the website: https://connect.garmin.com/modern/activities.

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

- Filtering by `labels` with garmin activities is a bit more involved. That is, with the custom `activities.json` format, we can label an activity like:

    ```jsonc
    // activities.json
    {
      "datetime": "2024-10-25T12:00:00",
      "distance": "marathon",
      "duration": "3h20m",
      "labels": ["official", "marathon"] // custom labels
    }
    ```

    Then in the `chart-requests.json` we can filter on this label to take only activities with this label:

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
    // activity-labels.json
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

    Then the label will be attached to the activity, and we can later filter on it in the `chart-requests`.

- We assume files that contain the name `garmin` or end with `.csv` are garmin activities files. Otherwise we assume `.json` format.

### How do chart filters work?

Chart requests allow us to filter activities based on some criteria. In general, `filters` contains a list of filters, where an activity must satisfy all filters to be included. For example:

```jsonc
// chart-requests.json
{
  // Global filter, applies to every chart.
  "filters": [
    "type = Running"
  ],
  "charts": [
    {
      "title": "Races and long runs",
      // Local filters, applies only to this chart.
      "filters": [
        "label = official_race",
        "distance >= 25 km",
        "datetime > 2024"
      ],
      "y-axis": "distance"
    },
    {
      "title": "Short runs",
      "filters": [
        "distance < 5 km",
      ],
      "y-axis": "distance"
    }
  ]
}
```

In this case, the first chart request will take all activities that satisfy **all** of the following criteria:

- Has type `Running`.
- Has label `official_race`.
- Has `distance >= 25 km`.
- Has `datetime > 2024` (i.e. 2025 onwards).

For instance, of the following, only `Race 1` will be selected.

```jsonc
// activities.json
{
  "activities": [
    {
      "datetime": "2025-03-20T14:30:00",
      "distance": "20 miles",
      "duration": "2h40m54s",
      "labels": [
        "official_race",
        "another label"
      ],
      "title": "Race 1",
      "type": "Running"
    },
    {
      "datetime": "2025-02-20T14:30:00",
      "distance": "20 miles",
      "duration": "2h40m54s",
      "title": "Race 2",
      "type": "Running"
    },
    {
      "datetime": "2024-03-20T14:30:00",
      "distance": "20 miles",
      "duration": "2h40m54s",
      "labels": [
        "official_race",
        "another label"
      ],
      "title": "Race 3",
      "type": "Running"
    }
  ]
}
```

Race 2 is removed because it does not have label `official_race`. Race 3 is removed because it is not `> 2024`.

#### Advanced filters

We also provide more sophisticated ways to write filters e.g.

- Set operators.
- Boolean logic.
- Unicode versions of some operators.

For example, we can write:

```jsonc
{
  "title": "Some title",
  "filters": [
    "label = official_race or label = marathon",
    "labels ∌ casual",
    "labels ⊇ {evening, night}",
    "distance ≥ 25 km xor distance < 5 km",
    "datetime > 2024",
    "not (pace < 5m /km)",
    "type ∈ {Running, Cycling}"
  ],
  "y-axis": "distance"
}
```

In this case, we will take all activities that satisfy **all** of the following criteria:

- Has label `official_race` and/or `marathon`.
- Does _not_ have label `casual`.
- Has labels `evening` and `night`.
- Has `distance >= 25 km` _or_ `distance < 5 km`, **not** both.
- Has `datetime > 2024` (i.e. 2025 onwards).
- Does _not_ have `pace < 5m /km`.
- Has type `Running` or `Cycling`.

The grammar is below.

##### Atoms

```
; Atoms are the base values.
atom
  : 'datetime' op             datetime
  | 'distance' op             distance
  | 'duration' op             duration
  | 'pace'     op             pace
  | 'label'    elem_ops
  | 'labels'   set_ops
  | 'type'     elem_ops

; E.g. 2024, 2024-08, 2024-08-15, 2024-08-15 14:20:00, 2024-08-15 14:20:00+0800
datetime
  : year
  | year '-' month
  | year '-' month '-' day
  | year '-' month '-' day time_sep hours ':' minutes ':' seconds
  | year '-' month '-' day time_sep hours ':' minutes ':' seconds tz_offset

time_sep
  : ' '
  | 'T'

tz_offset
  : '-' digits
  | '+' digits

; op is general binary operators i.e. equalities and inequalities.
op
  : op_eqs
  | op_gte
  | '>'
  | op_lte
  | '<'

op_eqs
  : '='
  | op_neq

op_neq
  : '≠'
  | '/='  ; alias for ≠

op_gte
  : '≥'
  | '>='  ; alias for ≥

op_lte
  : '≤'
  | '<='  ; alias for ≤

; Operators where the LHS is an individual element and the RHS is either
; an element or a set. For instance:
;
;   type = some_type ; Tests that the activity type is some_type.
;   type ∈ {t1, t2}  ; Tests that the activity type is t1 or t2.
;
; When the LHS refers to an arbitrary element in an actual set (e.g. 'label'
; as an element in 'labels' set), we have the following:
;
;   label =  some_label ; Tests that some_label exists in the labels set.
;   label /= some_label ; Tests that some_label does _not_ exist in the labels set.
;
; In other words, this is equivalent to membership; respectively:
;
;   labels ∋ some_label
;   labels ∌ some_label
;
; We also have
;
;   label ∈ {l1, l2} ; Tests that either l1 or l2 exists in the labels set.
;   label ∉ {l1, l2} ; Tests that l1 _and_ l2 do not exist in the labels set.
;
; These are equivalent to intersection. Respectively:
;
;   labels ∩ {l1, l2} ≠ ∅
;   labels ∩ {l1, l2} = ∅
elem_ops
  : op_eqs       string
  | elem_op_set  set_strings

elem_op_set
  : elem_op_in
  | elem_op_not_in

elem_op_in
  : ∈
  | 'in'  ; alias for ∈

elem_op_not_in
  : ∉
  | 'not_in'  ; alias for ∉


; Operators where LHS is a set (e.g. labels) and the RHS is either an
; individual element or another set.
;
;   labels ∋ some_label ; Tests that labels has an element some_label.
;   labels ∌ some_label ; Tests that labels does _not_ have element some_label.
;
;   labels ⊇ {l1, l2} ; Tests that labels has elements l1 and l2.
set_ops
  : set_op_elem string
  | set_op_set set_strings

; Operators for set membership e.g. 'A ∋ x' means "Set A has member x".
; 'A ∌ x' means "Set A does not have member x".
set_op_elem
  : set_op_contains
  | set_op_not_contains

set_op_contains
  : '∋'
  | 'contains'  ; alias for ∋

set_op_not_contains
  : '∌'
  | 'not_contains'  ; alias for ∌

; Operators for set comparisons.
set_op_set
  : op_eqs
  | set_op_gte
  | set_op_gt
  | set_op_lte
  | set_op_lt

; 'A >= B' and 'A ⊇ B' both mean A is a superset of B i.e. all of B is
; contained within A.
set_op_gte
  : '⊇'
  | '>='  ; alias for ⊇

; 'A > B' and 'A ⊃ B' both mean A is a _proper_ superset of B i.e. all of B is
; contained within A and B _does not equal_ A.
set_op_gt
  : '⊃'
  | '>'  ; alias for ⊃

; 'A <= B' and 'A ⊆ B' both mean A is a subset of B i.e. all of A is
; contained within B.
set_op_lte
  : '⊆'
  | '<='  ; alias for ⊆

; 'A < B' and 'A ⊂ B' both mean A is a _proper_ subset of B i.e. all of A is
; contained within B and A _does not equal_ B.
set_op_lt
  : '⊂'
  | '<'  ; alias for ⊂

; A label_set is either the empty set ({} or ∅) or a set with elements e.g.
; {a, b, c}
set_strings
  : '∅'
  | '{}'             ; alias for ∅
  | '{' strings '}'

strings
  : string
  | string ',' string
```

##### Expressions

Basic boolean expressions are also supported:

```
; An expression is either an atom or a boolean combination of expressions.
expr
  : atom
  | '(' expr ')'
  | not expr
  | expr or expr
  | expr and expr
  | expr xor expr

not
  : 'not'
  | '!'    ; alias for not
  | '¬'    ; alias for not

or
  : 'or'
  | '||'  ; alias for or
  | '∨'   ; alias for or

and
  : 'and'
  | '&&'   ; alias for and
  | '∧'    ; alias for and

xor
  : 'xor'
  | '⊕'    ; alias for xor
```

> [!NOTE]
>
> There is considerable redundancy here, in the sense that there are multiple ways to write the same expression even beyond operator aliases. For instance:
>
> ```
> ["labels ∋ some_label"]
>   == ["label = some_label"]
>
> ["labels ⊇ {evening, night}"]
>   == ["label = evening and label = night"]
>   == ["label = evening", "label = night"]
> ```
>
> The clearest way to write an expression is a matter of taste.
