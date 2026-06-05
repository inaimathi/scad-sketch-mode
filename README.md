# scad-sketch-mode

`scad-sketch-mode` is a minimal, keyboard-driven 2D sketch editor for OpenSCAD buffers in Emacs. It lets you open supported 2D SCAD forms at point, edit them in an SVG-backed sketch buffer using cursor movement, hover/selection, marks, primitive handles, and minibuffer commands, then write the result back into the original `.scad` source. The goal is not to replace OpenSCAD’s textual workflow, but to add a lightweight FreeCAD-sketch-like interface for the parts of a model that are easiest to understand visually: points, polygons, rounded polyRound paths, circles, squares, text, simple transforms, and 2D boolean compositions.

## User documentation

### Installation

Put the `scad-sketch-mode` files somewhere in your Emacs `load-path`, then require the top-level module:

```elisp
(add-to-list 'load-path "~/.emacs.d/mine/scad-sketch-mode/")
(require 'scad-sketch)
```

Enable it in OpenSCAD buffers:

```elisp
(add-hook 'openscad-mode-hook #'scad-sketch-mode)
```

The mode expects Emacs to have SVG image support:

```elisp
(image-type-available-p 'svg)
```

If that returns `nil`, the editor buffer cannot render the sketch canvas.

### Starting the editor

In a `.scad` buffer with `scad-sketch-mode` enabled:

```text
C-c C-a   edit supported form at point
C-c C-.   edit supported form at point, or insert a new named array
```

`scad-sketch-at-point` tries to discover the supported 2D form at point and opens it in a separate `SCAD-Sketch` editor buffer. `scad-sketch-or-insert-at-point` does the same, but if there is no edit target at point, it prompts for a new array name and inserts an empty editable array.

Typical examples:

```scad
pts = [
  [0, 0],
  [20, 0],
  [10, 17]
];

polygon(pts);

circle(r=20);

square([80, 40]);

text("hello", size=12);
```

Put point inside or on one of these forms and run `C-c C-a`.

### Supported SCAD forms

The parser intentionally supports a small 2D subset of OpenSCAD.

Supported source forms include:

```scad
name = [[x, y], ...];
name = [[x, y, r], ...];

polygon([[x, y], ...]);
polygon(name);
polygon(polyRound([[x, y, r], ...], fn));
polygon(polyRound(name, fn));

circle(15);
circle(r=15);
circle(d=30);

square([w, h]);
square([w, h], center=true);

text("label");
text("label", size=12);
text("label", size=12, font="Liberation Sans");

translate([x, y]) shape;
rotate(angle) shape;
scale([sx, sy]) shape;
mirror([mx, my]) shape;

union() { shape; shape; ... }
difference() { shape; shape; ... }
intersection() { shape; shape; ... }
```

Module bodies are descended into when discovering edit targets. `include`, `use`, unsupported declarations, and 3D forms are skipped where possible.

Unsupported or only partially preserved details include arbitrary expressions, variables in numeric positions, most optional SCAD arguments, complex font/layout parameters for `text`, and general 3D geometry. The editor is intentionally a visual editor for a direct numeric 2D subset.

### The editor buffer

The editor buffer shows an SVG canvas followed by a live SCAD preview.

The canvas displays:

```text
grid
current cursor point
marks
shape outlines
polygon vertices
primitive handles
hover/attention halo
selected objects
active object
live status information
```

The original source buffer is not modified until you write back.

```text
w   write edited sketch back to source buffer
q   quit editor; prompts to write back if dirty
u   undo source-geometry edits
?   show key summary
```

### Cursor movement

There is a movable editor cursor called `point`. This is separate from Emacs point in the source buffer.

```text
<arrow>       move cursor by one grid step
C-<arrow>     move cursor by one coarse step
M-<arrow>     move cursor by one fine step, intentionally off-grid
g             set grid step
x / y         set cursor X or Y
X / Y         set cursor X or Y relative to most recent mark
d             set distance from most recent mark, preserving angle
a             set angle from most recent mark, preserving distance
```

Grid and coarse movement snap to the grid. Fine movement does not.

### Marks

Marks are temporary construction points.

```text
m   replace all marks with current cursor point
M   push current cursor point onto mark stack
`   pop most recent mark and jump cursor there
'   jump cursor to most recent mark without popping
C   clear marks
```

Marks are useful for constructing lines, rectangles, and relative coordinates.

```text
l   append marks, oldest first, then cursor, as vertices
r   append rectangle from most recent mark to cursor
```

### Hover, focus, attention, and selection

The editor uses a few related but distinct concepts.

`selection` is the explicit set of objects you have selected with `SPC`.

`hover` is the stack of refs currently under or near the cursor point.

`focus` is the global fallback object used when nothing is hovered.

`attention` is the effective current object for commands:

```text
attention = hovered ref if anything is hovered, otherwise focus ref
```

The visible blue halo is hover-only. If the cursor moves away from an object, the halo disappears even if that object remains the fallback focus.

Key bindings:

```text
TAB       cycle through hovered refs under the cursor
S-TAB     cycle backward through hovered refs
.         cycle forward through hovered refs
,         cycle backward through hovered refs

M-TAB     cycle global focus through all selectable refs
M-S-TAB   cycle global focus backward

SPC       toggle current attention ref in selection
s         clear selection
Esc       clear marks, selection, and hover cycling state
```

Use `TAB` when several things overlap under the cursor and you want to choose exactly which one receives attention. Use `M-TAB` when you want to jump globally through every selectable shape or handle in the sketch.

### Moving selected geometry

Selection movement uses shifted arrows:

```text
S-<arrow>       move selected geometry by one grid step
C-S-<arrow>     move selected geometry by one coarse step
M-S-<arrow>     move selected geometry by one fine step
```

When selected geometry moves, the editor cursor moves along with it. This keeps the moved vertex, primitive handle, or shape under the cursor for repeated keyboard edits.

### Polygon and array editing

For arrays and polygons, point refs are polygon vertices.

```text
p   append cursor as a new vertex
i   insert cursor after selected vertex
k   delete selected vertex
c   toggle polygon closed/open
R   set polyRound radius on selected vertex
```

If marks are set, `i` inserts marks oldest-first and then the cursor after the selected vertex.

Polygon point storage uses the editor convention:

```elisp
(x y r)
```

where `r` defaults to `0` and represents a `polyRound`-style corner radius.

### Circle editing

Circles have primitive handles:

```text
center       moves the circle
east radius  changes radius
north radius changes radius
```

The radius handles do not represent independent scaling handles. They are two convenient ways to set the same scalar circle radius.

Commands:

```text
TAB / S-TAB             choose hovered circle handle
S-<arrow>               move selected handle
R                       set circle radius from minibuffer
M-x scad-sketch-set-size also prompts for circle radius
```

### Square editing

Squares are represented as editable rectangle primitives rather than being converted into polygons.

Square handles:

```text
four corners   resize width/height from the opposite corner
center         move the whole square
```

Commands:

```text
TAB / S-TAB              choose hovered square handle
S-<arrow>                move selected handle
M-x scad-sketch-set-size set width and height from minibuffer
```

### Text editing

Text has a point-like origin handle and several minibuffer commands.

```text
M-x scad-sketch-set-text       change the displayed string
M-x scad-sketch-set-size       change text size
M-x scad-sketch-set-text-font  change font with completion
```

Font completion uses `font-family-list`, so available options depend on the fonts Emacs can see on the current system.

### Writing back

Use:

```text
w
```

to write the current sketch back into the original source buffer.

The write-back strategy depends on the original form:

* direct array assignments update the array
* variable-reference polygons update the referenced array when safe
* inline polygons may remain inline if small
* larger inline polygons may be extracted into generated `_sketch_N` arrays
* primitive circles/squares/text emit as primitive SCAD calls
* transformed shapes may be flattened into translated/rotated primitive output
* boolean trees emit as `union`, `difference`, or `intersection` blocks

Use:

```text
q
```

to quit. If the session is dirty, the editor asks whether to write back first.

## Contributor documentation

### High-level architecture

The project is split into small Emacs Lisp modules with deliberately narrow responsibilities.

```text
scad-sketch.el
  Top-level entry point and minor mode for OpenSCAD buffers.

scad-sketch-parse.el
  Recursive-descent parser for the supported 2D OpenSCAD subset.

scad-sketch-session.el
  Session construction, AST-to-editor conversion, target discovery,
  source write-back, preview emission, and core data structures.

scad-sketch-geometry.el
  Pure geometry helpers: points, movement, snapping, rectangles,
  transforms, polyRound arc helpers.

scad-sketch-editor-core.el
  Editor dispatch and undo infrastructure. Owns the clean/dirty
  change triad and editor buffer lifecycle.

scad-sketch-editor--refs.el
  Selection ref constructors and predicates.

scad-sketch-editor--selection.el
  Selection, hover, focus, attention, selectable refs, primitive
  handles, and selected-location expansion.

scad-sketch-editor--cursor.el
  Cursor movement, marks, coordinate setters, distance/angle setters,
  and grid command.

scad-sketch-editor--editing.el
  Source-geometry mutations: append/insert/delete, primitive handle
  movement, shape movement, radius/size/text/font commands, undo,
  selection toggles, and hover/global cycling commands.

scad-sketch-editor--rendering.el
  SVG rendering: canvas bounds, transforms, grid, paths, boolean
  preview, selection highlight, hover-attention halo, handles, HUD,
  and final buffer rendering.

scad-sketch-editor-mode.el
  Major mode assembly: keymap, derived mode definition, write-back,
  quit, and help summary.
```

The intended load direction is:

```text
parse / geometry / session
        ↓
editor refs / selection / core / cursor / editing
        ↓
rendering
        ↓
editor-mode
        ↓
scad-sketch.el
```

`core` declares `scad-sketch--render` as a forward reference. Rendering is loaded later by the top-level editor mode.

### Important data structures

#### `scad-sketch-session`

Defined in `scad-sketch-session.el`.

A session contains:

```text
name
grid / fine-step / coarse-step
current cursor point
marks
selected-index legacy slot
shapes
active-shape-id
boolean/shape tree
targets
root-target-id
selection
focus-ref
hover-index
source buffer markers
AST/path/root-node
dirty flag
undo stack
```

The session is buffer-local in the editor buffer as `scad-sketch--session`.

#### `scad-sketch-shape`

Also defined in `scad-sketch-session.el`.

Current shape kinds:

```text
polygon
circle
square
text
```

Polygon geometry lives in `points`. Primitive shape parameters live in `metadata`.

Typical metadata:

```elisp
;; circle
(:cx 0.0 :cy 0.0 :r 10.0)

;; square
(:x 0.0 :y 0.0 :w 80.0 :h 40.0 :angle 0.0)

;; text
(:str "hello" :x 0.0 :y 0.0 :size 12.0 :font "Liberation Sans" :angle 0.0)
```

#### Refs

Refs are plists identifying selectable/hoverable things.

Defined in `scad-sketch-editor--refs.el`:

```elisp
(:kind shape :shape-id SHAPE-ID)

(:kind point :shape-id SHAPE-ID :index IDX)
```

Boolean refs are also supported in rendering/selection-adjacent code as:

```elisp
(:kind boolean :group-id GROUP-ID)
```

Point refs are used for both polygon vertices and primitive handles.

### Parser entry points

The parser consumes source text and produces plist AST nodes with `:type`, `:beg`, and `:end`.

Important functions:

```elisp
scad-sketch-parse-buffer
scad-sketch-parse-node-at
```

Supported node types include:

```elisp
array
polygon
circle
square
text
union
difference
intersection
translate
rotate
scale
mirror
module
```

When adding a new supported SCAD form, update:

```text
token/grammar support in scad-sketch-parse.el
AST-to-session conversion in scad-sketch-session.el
shape rendering in scad-sketch-editor--rendering.el
selection/handles in scad-sketch-editor--selection.el
editing commands in scad-sketch-editor--editing.el
emission/write-back in scad-sketch-session.el
```

### Session construction and write-back

The main user-facing session constructors are:

```elisp
scad-sketch-session-at-point
scad-sketch-session-insert-array-at-point
```

For direct arrays, the session points at the array assignment.

For shape roots, `scad-sketch-session--session-from-edit-root` converts a parser AST subtree into editor shapes and a tree.

The tree is a plist structure of either:

```elisp
(:kind shape :shape-id ID)
```

or:

```elisp
(:kind boolean :op OP :group-id GROUP-ID :children CHILDREN)
```

The target system tracks what source region can be replaced on write-back. This lets the editor distinguish direct source arrays, inline polygons, variable-reference polygons, transformed roots, and boolean roots.

Primary write-back/preview entry points:

```elisp
scad-sketch-session-preview
scad-sketch-session-write-back
```

### Change dispatch and undo

All editor state changes should go through `scad-sketch-editor-core.el`.

```elisp
scad-sketch--clean-change
```

Use for UI/session-only changes:

```text
cursor movement
marks
hover cycling
focus cycling
selection changes
grid changes
```

```elisp
scad-sketch--edit
```

Use for source-geometry changes:

```text
moving vertices
moving primitive handles
changing radius
changing square size
changing text content
appending/deleting points
```

Dirty edits push an undo snapshot before mutation and mark the session dirty afterward.

Undo snapshots currently include:

```text
points
point
marks
named marks
selected-index
closed
shapes
active-shape-id
targets
root-target-id
selection
focus-ref
```

### Hover/focus/attention model

The intended semantics are:

```text
selection  explicit multi-object set toggled by SPC
hover      refs under/near the cursor point
focus      global fallback ref
attention  hover ref if any exists, otherwise focus ref
halo       hover ref only
```

The important functions are in `scad-sketch-editor--selection.el`:

```elisp
scad-sketch--hover-candidates
scad-sketch--hover-ref
scad-sketch--attention-ref
scad-sketch--normalize-attention
scad-sketch--selectable-refs
scad-sketch--ref-anchor
```

Hover cycling should not move the cursor:

```elisp
scad-sketch--cycle-hovered
```

Global focus cycling does move the cursor to the selected object’s anchor:

```elisp
scad-sketch--cycle-selectable
```

### Primitive handles

Primitive handles are represented as point refs.

Current conventions:

```text
circle:
  0 center
  1 east radius
  2 north radius

square:
  0 lower-left/origin corner
  1 lower-right corner
  2 upper-right corner
  3 upper-left corner
  4 center

text:
  0 origin
```

The central handle functions are:

```elisp
scad-sketch--primitive-handle-count
scad-sketch--primitive-handle-xy
scad-sketch--move-primitive-handle-to
```

The selection layer decides what can be hovered or selected. The editing layer decides what moving the handle actually means.

### Rendering model

Rendering is SVG-backed and owned by `scad-sketch-editor--rendering.el`.

Main entry point:

```elisp
scad-sketch--render
```

Scene rendering:

```elisp
scad-sketch--draw-path
```

Shape path generation:

```elisp
scad-sketch--shape-path-d
```

Per-shape overlay renderers:

```elisp
scad-sketch--draw-one-polygon-shape
scad-sketch--draw-one-circle-shape
scad-sketch--draw-one-square-shape
scad-sketch--draw-one-text-shape
```

Boolean preview uses compound SVG paths rather than performing true geometric boolean operations. The technique is:

1. draw all positive child paths as one compound filled path
2. draw the same compound as a stroked path
3. use SVG masks/clip paths for difference/intersection previews

This produces a good visual approximation for editor use without implementing full polygon clipping.

Selection and attention are visually separate:

```text
orange     selected
blue halo  hover-attention
dark/gray  active/fallback/normal
```

Point attention gets a point halo. Shape or boolean attention gets a geometry-level halo over the whole shape or boolean subtree.

### Keymap ownership

The keymap lives in `scad-sketch-editor-mode.el` as:

```elisp
scad-sketch-editor-mode-map
```

High-level groups:

```text
cursor movement
selected geometry movement
marks and transient clears
vertex/shape editing
hover/focus/selection
coordinate commands
session commands
```

When adding a new command, prefer to put the implementation in the relevant subsystem and only bind it in `scad-sketch-editor-mode.el`.

### Adding a new primitive shape

To add a new primitive, for example `ellipse`, the usual checklist is:

1. Parse it in `scad-sketch-parse.el`.
2. Add a shape constructor in `scad-sketch-session.el`.
3. Convert parser node to shape in `scad-sketch-session--convert-node`.
4. Add shape emission in `scad-sketch-session--emit-shape-with-assignments`.
5. Add bounds/path support in `scad-sketch-editor--rendering.el`.
6. Add a per-shape overlay renderer.
7. Add handle count/positions in `scad-sketch-editor--selection.el`.
8. Add handle movement behavior in `scad-sketch-editor--editing.el`.
9. Add minibuffer parameter commands if useful.
10. Add examples to `test.scad`.

### Development workflow

A simple local workflow is to edit the `.el` files, copy or symlink them into your Emacs load path, then reload the affected modules or restart Emacs.

Useful manual smoke tests:

```scad
triangle = [
  [0, 0],
  [50, 0],
  [25, 43]
];

polygon(triangle);

circle(r=20);

square([80, 40]);

text("hello", size=12, font="Liberation Sans");

difference() {
  union() {
    square([80, 40]);
    translate([80, 0])
      circle(r=20);
    translate([10, 10])
      text("A", size=12);
  }

  circle(r=10);
}
```

Smoke-test sequence:

```text
C-c C-a on direct array
C-c C-a on polygon variable ref
C-c C-a on inline polygon
C-c C-a on circle
C-c C-a on square
C-c C-a on text
C-c C-a inside nested boolean tree

TAB through hovered stack
M-TAB through global selectable refs
SPC select/deselect
S-arrows move selected refs
R set radius
M-x scad-sketch-set-size
M-x scad-sketch-set-text
M-x scad-sketch-set-text-font
w write back
u undo
Esc clear transient state
```

### Project philosophy

`scad-sketch-mode` should stay small, textual, and predictable. The editor is meant to make numeric 2D geometry easier to manipulate, not to hide the underlying OpenSCAD source. When in doubt, prefer:

```text
simple source output
explicit numeric geometry
small parser surface
keyboard-first workflows
clear visual feedback
few dependencies
```

The best user experience is one where the generated SCAD remains readable enough that users can comfortably keep editing it by hand.
