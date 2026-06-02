# scad-sketch

A keyboard-driven SVG sketch editor for OpenSCAD point-array literals, living
inside Emacs. Supports plain 2D polygons (`kind=2d`) and polyRound-style
rounded polygons (`kind=2d-with-curves`).

The model is always an OpenSCAD array literal. SVG is strictly the view — the
editor writes back valid `.scad` source, not an image.

---

## Requirements

- Emacs 27.1 or later, built with SVG support (`--with-rsvg` or equivalent)
- [`scad-mode`](https://github.com/openscad/emacs-scad-mode) (recommended, not strictly required)
- [`Round-Anything`](https://github.com/Irev-Dev/Round-Anything) if you use `kind=2d-with-curves`

---

## Installation

Copy `scad-sketch.el` somewhere on your `load-path` and add to your init file:

```elisp
(require 'scad-sketch)
```

To activate the minor mode automatically whenever you open a `.scad` file:

```elisp
(add-hook 'scad-mode-hook #'scad-sketch-mode)
```

---

## Entry points

### `scad-sketch-or-insert-at-point` — the main command  `C-c C-.`

Place point anywhere inside an existing array literal or scad-sketch block and
run this command to open the visual editor. If no array is found at point, you
are prompted for a name and a fresh empty array is inserted at point and opened
immediately.

This is the command to bind to muscle memory. It handles every case.

### `scad-sketch-adopt-array-at-point` — annotate a bare array  `C-c C-a`

Place point inside any literal array assignment such as:

```scad
profile = [
  [0, 0], [40, 0], [40, 12], [0, 12]
];
```

Running this command wraps it in `scad-sketch` metadata comments so the editor
can track it across sessions:

```scad
// scad-sketch: name=profile kind=2d closed=true grid=1 units=mm
profile = [
  [0, 0], [40, 0], [40, 12], [0, 12]
];
// end-scad-sketch
```

Kind is inferred automatically: all two-column points → `kind=2d`, any
three-column points → `kind=2d-with-curves`. You are prompted for closed/open,
grid step, and units.

### `scad-sketch-at-point` — open an already-annotated block  `M-x`

Like `scad-sketch-or-insert-at-point` but does not insert anything if no array
is found — signals an error instead. Useful if you want to be sure you are
editing something that already exists.

### `scad-sketch-insert-array-at-point` — insert only  `M-x`

Prompts for a name, inserts an empty annotated array at point, and opens the
editor. This is what `scad-sketch-or-insert-at-point` delegates to when nothing
is found at point.

### `scad-sketch-insert-2d-block` / `scad-sketch-insert-polyround-block`  `M-x`

Insert an empty annotated block of the named kind at point without opening the
editor. Useful for quickly scaffolding several arrays before editing them.

---

## The annotated block format

```scad
// scad-sketch: name=NAME kind=KIND closed=BOOL grid=NUM units=STR
NAME = [
  ...
];
// end-scad-sketch
```

| Key      | Values                          | Default |
|----------|---------------------------------|---------|
| `name`   | any identifier                  | —       |
| `kind`   | `2d` or `2d-with-curves`        | `2d`    |
| `closed` | `true` / `false`                | `true`  |
| `grid`   | any positive number             | `1`     |
| `units`  | any string (display only)       | `mm`    |
| `fine`   | fine-step size (M-arrow)        | `0.1`   |
| `coarse` | coarse-step size (C-arrow)      | `5`     |

---

## Array kinds

### `kind=2d`

Emits plain `[x, y]` pairs. Use this for `polygon()`, `offset()`, path
arguments, etc.

```scad
profile = [
  [0, 0], [40, 0], [40, 12], [0, 12]
];
```

### `kind=2d-with-curves`

Emits `[x, y, r]` triples compatible with the
[Round-Anything `polyRound`](https://github.com/Irev-Dev/Round-Anything)
library. The third value is the rounding radius at that vertex; `0` means a
sharp corner.

```scad
profile = [
  [0, 0, 3], [40, 0, 3], [40, 12, 3], [0, 12, 3]
];
```

The editor renders the actual arc geometry so what you see on the canvas matches
what `polyRound` will produce, including radius capping when a requested radius
is too large for a short edge.

---

## The editor

Opening the editor splits to a new buffer containing:

- an **SVG canvas** showing the polygon, grid, cursor, marks, and vertex labels
- a **live array preview** of the current OpenSCAD literal below the canvas

The canvas status bar (top edge) shows: `name  kind  gridNunits  point=(x,y)  mark=…  sel=N  saved/\*dirty\*`

Press `C-h m` or `?` at any time for key help.

### Movement

| Key | Action |
|-----|--------|
| `←↑→↓` | Move cursor by one grid step; **snaps to grid** |
| `C-←↑→↓` | Move cursor by one coarse step; **snaps to grid** |
| `M-←↑→↓` | Move cursor by one fine step; **intentionally off-grid** |
| `S-←↑→↓` | Move the selected vertex by one grid step |

Arrow and coarse moves always snap the cursor back to the nearest grid
intersection, so using fine moves to reach an off-grid position and then
pressing a plain arrow key will snap back to grid.

### Vertex editing

| Key | Action |
|-----|--------|
| `TAB` / `S-TAB` | Select next / previous vertex; cursor jumps to it |
| `p` | Append cursor position as a new vertex at end of array |
| `i` | Insert after selected vertex; if marks are set, inserts each mark (oldest first) then cursor |
| `k` | Delete the selected vertex |
| `c` | Toggle closed / open polygon |

### Marks

Marks are green reference points used for relative positioning and
multi-segment insertion. The most recently pushed mark is the "current" mark,
shown labelled; older marks appear as smaller unlabelled dots. A dashed line
threads through all marks to the cursor.

| Key | Action |
|-----|--------|
| `m` | Replace all marks with the current cursor position |
| `M` | Push cursor position onto the mark stack (accumulate) |
| `` ` `` | Pop the most recent mark and jump cursor to it |
| `'` | Jump cursor to most recent mark without removing it |
| `C` | Clear all marks |

**Multi-point insertion workflow:** navigate to a vertex with `TAB`, move cursor
to a waypoint, press `M`, continue moving and pressing `M` for each waypoint,
arrive at the final position, press `i`. All accumulated mark positions are
inserted (oldest first) followed by the cursor, all after the originally
selected vertex.

### Geometry

| Key | Action |
|-----|--------|
| `x` / `y` | Set cursor X or Y to an absolute value |
| `X` / `Y` | Set cursor X or Y relative to the most recent mark |
| `d` | Set distance from most recent mark, preserving current angle |
| `a` | Set angle from most recent mark in degrees, preserving distance |
| `l` | Append marks (oldest first) then cursor as new vertices |
| `r` | Append four corners of the rectangle from mark to cursor |
| `g` | Prompt for a new grid step |

### polyRound radii

| Key | Action |
|-----|--------|
| `R` | Set the polyRound radius on the selected vertex |

For `kind=2d-with-curves`, each vertex dot with a non-zero radius shows a
dashed circle whose size reflects the **actual** radius that will be used after
edge-length capping. If the requested radius had to be reduced to fit between
adjacent vertices, the circle and label turn orange and the label reads
`r=REQ→ACT`.

### Session

| Key | Action |
|-----|--------|
| `w` | Write the edited array back to the source buffer |
| `u` | Undo the last editing operation |
| `q` | Quit; offers to write back first if there are unsaved changes |
| `?` | One-line key summary in the echo area |
| `C-h m` | Full mode documentation |

---

## Tips

- **You do not need the metadata comments to use the editor.** Place point
  inside any bare `name = [[x, y], ...]` assignment and press `C-c C-.`.

- **Writing back** (`w`) replaces only the content between the metadata comment
  lines (or the original array bounds for bare arrays). Everything else in the
  file is untouched.

- **Grid step** is a per-session setting. Use `g` to change it while editing;
  it does not affect the stored metadata unless you re-adopt the array.

- **Undo** is scad-sketch's own stack and is separate from the source buffer's
  undo history. It covers all editing operations including mark changes.
