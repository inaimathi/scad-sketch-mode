# scad-sketch-mode

`scad-sketch-mode` is a minimal, keyboard-driven 2D sketch editor for OpenSCAD buffers in Emacs. It opens supported 2D SCAD forms at point, renders them in an SVG-backed sketch buffer, lets you edit geometry with cursor movement, marks, hover/attention, selection, primitive handles, grouping commands, and minibuffer prompts, then writes the result back into the original `.scad` source. The goal is not to replace OpenSCAD’s textual workflow, but to add a lightweight FreeCAD-sketch-like interface for the parts of a model that are easiest to understand visually: arrays, polygons, rounded [`polyRound`](https://github.com/Irev-Dev/Round-Anything) paths, circles, squares, text, simple transforms, mirror nodes, and 2D boolean compositions.

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

The editor expects Emacs to have SVG image support:

```elisp
(image-type-available-p 'svg)
```

If that returns `nil`, the sketch canvas cannot render.

### Starting the editor

In a `.scad` buffer with `scad-sketch-mode` enabled:

```text
C-c C-a   edit supported form at point
C-c C-.   edit supported form at point, or open a new generic sketch block
```

`scad-sketch-at-point` discovers the supported 2D form at point and opens it in a separate `SCAD-Sketch` editor buffer.

`scad-sketch-or-insert-at-point` does the same, but if there is no edit target at point, it opens a blank generic sketch block at point. Drawing into that block writes inline shape/tree source back into the original buffer. It no longer defaults to inserting a named array.

A direct array command still exists for explicit array creation:

```text
M-x scad-sketch-insert-array-at-point
```

Typical editable examples:

```scad
pts = [
  [0, 0],
  [20, 0],
  [10, 17]
];

polygon(pts);

polygon([[0,0], [30,0], [15,26]]);

polygon(polyRound([
  [0, 0, 3],
  [80, 0, 3],
  [80, 50, 3],
  [0, 50, 3]
], 32));

circle(r=20);

square([80, 40]);

text("hello", size=12, font="Liberation Sans");

difference() {
  square([80, 40]);
  circle(r=10);
}

mirror([1, 0])
  polygon([[0,0], [20,0], [10,17]]);
```

Put point inside or on one of these forms and run `C-c C-a`.

### Supported SCAD forms

The parser intentionally supports a small direct-numeric 2D subset of OpenSCAD.

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

Module and function bodies are descended into when discovering edit targets. `include`, `use`, unsupported declarations, scalar assignments, unsupported control flow, and 3D forms are skipped where possible.

Unsupported or only partially preserved details include arbitrary expressions, variables in numeric positions, most optional SCAD arguments, complex text layout parameters, and general 3D geometry. The editor is intentionally a visual editor for a direct numeric 2D subset.

### Source-style preservation

The editor tries to preserve the source style of polygons:

```text
direct array session                 stays an array session
polygon(name)                        keeps referencing name
polygon(polyRound(name, fn))          keeps the polyRound variable reference
inline polygon                        stays inline
large inline polygon                  stays inline
inline polyRound polygon              stays inline polyRound
newly drawn polygon                   emits inline
```

The old `_sketch_N` extraction behavior has been removed. The project has not had a stable public release yet, so there is no legacy extraction format to preserve.

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
mirror axes and mirror handles
boolean group outlines
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
?   native Emacs mode help
```

Use `C-h m` or `?` in the editor buffer for the generated key list from the active keymap.

### Preview mode

Preview mode temporarily renders the sketch closer to what the object will actually look like.

```text
S-SPC   show clean preview until key release / next input
```

Preview mode:

```text
omits points and handles
omits editor labels
omits marks and cursor point
omits boolean group outlines/labels
omits selection and attention effects
omits mirror axes and dashed mirror ghosts
renders mirror output as solid geometry
uses a solid grid-colored background instead of grid lines
keeps actual text shapes visible
```

Boolean preview is intentionally simple and SVG-backed. It is a visual approximation suitable for editor feedback, not a full computational geometry boolean engine.

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

Marks are useful for drawing shapes, constructing relative geometry, and setting coordinates.

### Hover, focus, attention, and selection

The editor uses a few related but distinct concepts.

```text
selection   explicit multi-object set toggled by SPC
hover       stack of refs currently under or near cursor point
focus       global fallback ref used when nothing is hovered
attention   hovered ref if any exists, otherwise focus ref
```

The visible blue halo follows hover attention. If the cursor moves away from an object, its hover halo disappears even if the object remains the fallback focus.

```text
TAB       cycle forward through hovered refs under cursor
S-TAB     cycle backward through hovered refs
.         cycle forward through hovered refs
,         cycle backward through hovered refs

M-TAB     cycle global focus through all focusable refs
C-M-i     same as M-TAB in terminals that encode M-TAB this way
M-S-TAB   cycle global focus backward, when available

SPC       toggle current attention ref in selection
s         clear selection
Esc       clear marks, selection, and hover cycling state
```

Use `TAB` when several things overlap under the cursor and you want to choose exactly which one receives attention. Use `M-TAB` when you want to jump globally through every focusable shape, handle, mirror axis, or group target.

### Group attention and selection

Boolean groups have two distinct attention refs:

```text
:boolean           the group wrapper itself
:boolean-members   all child objects in the group
```

The distinction matters:

```text
attention on :boolean          shows the group-wrapper halo
attention on :boolean-members  shows the child-object compound halo
SPC on either                  selects/toggles the child shapes directly
break group                    only works on :boolean
```

Groups are attention targets, not stored selection targets. Selection stores direct shape refs, not boolean refs.

### Moving selected geometry

Selection movement uses shifted arrows:

```text
S-<arrow>       move selected geometry by one grid step
C-S-<arrow>     move selected geometry by one coarse step
M-S-<arrow>     move selected geometry by one fine step
```

When selected geometry moves, the editor cursor moves along with it. This keeps the moved vertex, primitive handle, shape, or mirror handle under the cursor for repeated keyboard edits.

### Insertion and drawing prefix

Most insertion/drawing commands live under the `i` prefix.

```text
i a   append cursor as a polygon/array point
i i   insert cursor after selected polygon vertex
i l   append marks oldest-first, then cursor, as vertices
i r   append rectangle from most recent mark to cursor

i p   draw polygon from marks plus cursor
i b   draw square/box from marks plus cursor
i s   draw square/box from marks plus cursor
i c   draw circle from mark plus cursor
i o   draw circle from mark plus cursor
i t   draw text at cursor point
```

Drawing behavior:

```text
draw polygon
  uses marks plus point as polygon vertices
  defaults corner radii to zero

draw square/box
  with one mark: mark and point are diagonal corners
  with two marks: marks plus point define three corners

draw circle
  point is the center
  distance from point to most recent mark is the radius

draw text
  prompts for text, size, and optionally font
```

### Group prefix

Boolean and grouping commands live under the `b` prefix.

```text
b u   wrap selected shapes as union
b d   wrap selected shapes as difference
b i   wrap selected shapes as intersection
b m   wrap selected shapes as mirror
b v   wrap selected shapes as mirror

b b   break attentioned group apart
b x   break attentioned group apart
```

Wrapping commands operate on selected whole shapes. Break-apart operates only on the currently attentioned group wrapper, not on child shapes or `:boolean-members`.

### Polygon and array editing

For arrays and polygons, point refs are polygon vertices.

```text
k   delete selected vertex/object
c   toggle polygon closed/open
R   set polyRound radius on selected vertex
```

Point insertion moved under the insertion prefix:

```text
i a   append cursor as a new vertex
i i   insert cursor after selected vertex
```

If marks are set, `i i` inserts marks oldest-first and then the cursor after the selected vertex.

Polygon point storage uses the editor convention:

```elisp
(x y r)
```

where `r` defaults to `0` and represents a `polyRound`-style corner radius.

### Circle editing

Circles have primitive handles:

```text
0 center
1 east radius
2 north radius
```

The radius handles do not represent independent scaling handles. They are two convenient ways to set the same scalar circle radius.

Common operations:

```text
TAB / S-TAB       choose hovered circle handle
SPC               select the handle
S-<arrow>         move selected handle
R                 set radius from minibuffer
M-x scad-sketch-set-size
```

### Square editing

Squares are represented as editable rectangle primitives rather than being converted into polygons.

Square handles:

```text
0 lower-left/origin corner
1 lower-right corner
2 upper-right corner
3 upper-left corner
4 center
```

Common operations:

```text
TAB / S-TAB       choose hovered square handle
SPC               select the handle
S-<arrow>         move selected handle
M-x scad-sketch-set-size
```

### Text editing

Text has a point-like origin handle and several minibuffer commands.

```text
M-x scad-sketch-set-text       change displayed string
M-x scad-sketch-set-size       change text size
M-x scad-sketch-set-text-font  change font with completion
```

Font completion uses `font-family-list`, so available options depend on the fonts Emacs can see on the current system.

Visible text renders as white text with a thin dark outline so it remains legible over both filled shapes and the editor background. Text used as a subtractor in `difference()` is rendered into the boolean mask as glyph text, not as a rough bounding rectangle.

### Mirror editing

Mirror nodes are editable tree wrappers.

A mirror has:

```text
mirror axis
two editable axis handles
solid source-side geometry
dashed secondary mirror output in normal editor mode
solid source + mirrored geometry in preview mode
```

Commands:

```text
A       set mirror axis by minibuffer prompt
SPC     select mirror axis/handle when it has attention
S-arrows move selected mirror handle/axis
```

Mirror wrapping also exists under the group prefix:

```text
b m
b v
```

### Writing back

Use:

```text
w
```

to write the current sketch back into the original source buffer.

The write-back strategy depends on the original form:

```text
direct array assignments update the array
variable-reference polygons update the referenced array when safe
inline polygons stay inline
large inline polygons stay inline
inline polyRound polygons stay inline polyRound
primitive circles/squares/text emit as primitive SCAD calls
mirror roots emit as mirror wrappers
boolean trees emit as union/difference/intersection blocks
generic blank sessions emit inline shape/tree source at the original point
breaking a root group may emit a sequence of adjacent top-level forms
```

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
  Recursive-descent parser and unparser for the supported 2D OpenSCAD subset.

scad-sketch-session.el
  Session construction, AST-to-editor conversion, target discovery,
  source write-back, preview emission, tree helpers, and core data structures.

scad-sketch-geometry.el
  Pure geometry helpers: points, movement, snapping, rectangles,
  transforms, mirror transforms, and polyRound arc helpers.

scad-sketch-editor-core.el
  Editor dispatch and undo infrastructure. Owns the clean/dirty
  change triad and editor buffer lifecycle.

scad-sketch-editor--refs.el
  Ref constructors, accessors, summaries, and structural predicates.

scad-sketch-editor--selection.el
  Selection, hover, focus, attention, selectable refs, primitive
  handles, mirror refs, group refs, and selected-location expansion.

scad-sketch-editor--cursor.el
  Cursor movement, marks, coordinate setters, distance/angle setters,
  and grid command.

scad-sketch-editor--editing.el
  Source-geometry mutations: append/insert/delete, primitive handle
  movement, shape movement, drawing commands, group wrapping/breaking,
  mirror-axis editing, radius/size/text/font commands, undo,
  selection toggles, and hover/global cycling commands.

scad-sketch-editor--rendering.el
  SVG rendering: canvas bounds, transforms, grid, normal rendering,
  preview rendering, boolean masks/clips, mirror rendering, selection
  highlight, hover-attention halo, labels, handles, HUD, and final
  buffer rendering.

scad-sketch-editor-mode.el
  Major mode assembly: prefix maps, top-level keymap, derived mode
  definition, write-back, quit, and native help binding.
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

`core` calls `scad-sketch--render` as a forward reference. Rendering is loaded later by the top-level editor mode.

### Important data structures

#### `scad-sketch-session`

Defined in `scad-sketch-session.el`.

A session contains:

```text
name
grid / fine-step / coarse-step
current cursor point
marks
named marks
selected-index legacy slot
closed flag
shapes
active-shape-id
tree
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

#### Tree nodes

The session tree represents shape, mirror, boolean, and sequence structure.

Typical tree nodes:

```elisp
(:kind shape :shape-id ID)

(:kind boolean :op union :group-id GROUP-ID :children CHILDREN)

(:kind boolean :op difference :group-id GROUP-ID :children CHILDREN)

(:kind boolean :op intersection :group-id GROUP-ID :children CHILDREN)

(:kind mirror :mirror-id MIRROR-ID :mx 1.0 :my 0.0 :child CHILD)

(:kind sequence :children CHILDREN)
```

`sequence` is not an OpenSCAD boolean operation. It represents adjacent emitted top-level forms, mainly after breaking apart a root group.

Tree helpers should generally descend through:

```text
boolean
mirror
sequence
```

#### Refs

Refs are plists identifying hoverable/focusable/selectable things.

Defined in `scad-sketch-editor--refs.el`:

```elisp
(:kind shape :shape-id SHAPE-ID)

(:kind point :shape-id SHAPE-ID :index IDX)

(:kind boolean :group-id GROUP-ID)

(:kind boolean-members :group-id GROUP-ID)

(:kind mirror :mirror-id MIRROR-ID)

(:kind mirror-point :mirror-id MIRROR-ID :index IDX)
```

Point refs are used for both polygon vertices and primitive handles.

Boolean refs are attention/focus targets. Selection expands them to child shapes rather than storing boolean refs directly.

### Parser entry points

The parser consumes source text and produces plist AST nodes with `:type`, `:beg`, and `:end`.

Important functions:

```elisp
scad-sketch-parse
scad-sketch-parse-node-at
scad-sketch-parse--path-to
scad-sketch-parse--walk
scad-sketch-unparse
scad-sketch-unparse-top-level
```

Supported node types include:

```text
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
```

When adding a new supported SCAD form, update:

```text
token/grammar support in scad-sketch-parse.el
AST-to-session conversion in scad-sketch-session.el
shape/tree emission in scad-sketch-session.el
selection/handles in scad-sketch-editor--selection.el
editing commands in scad-sketch-editor--editing.el
rendering in scad-sketch-editor--rendering.el
tests and fixtures
```

### Session construction and write-back

Main session constructors:

```elisp
scad-sketch-session-at-point
scad-sketch-session-insert-block-at-point
scad-sketch-session-insert-array-at-point
```

Direct array sessions edit array assignments.

Shape root sessions convert parser AST subtrees into editor shapes and a session tree.

Generic block sessions start empty and emit inline shape/tree source at the original insertion point.

Primary write-back/preview entry points:

```elisp
scad-sketch-session-preview
scad-sketch-session-write-back
```

The target system tracks what source regions can be replaced on write-back. This lets the editor distinguish direct arrays, inline polygons, variable-reference polygons, transformed roots, mirror roots, boolean roots, and generic insertion blocks.

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
preview state
```

```elisp
scad-sketch--edit
```

Use for source-geometry changes:

```text
moving vertices
moving primitive handles
moving shapes
moving mirror handles
changing mirror axis
changing radius
changing square size
changing text content/font/size
appending/deleting points
drawing shapes
wrapping/breaking groups
```

Dirty edits push an undo snapshot before mutation and mark the session dirty afterward.

Undo snapshots include:

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
tree
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

Important functions in `scad-sketch-editor--selection.el`:

```elisp
scad-sketch--hover-candidates
scad-sketch--hover-ref
scad-sketch--hover-attention-ref
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

Central handle functions:

```elisp
scad-sketch--primitive-handle-count
scad-sketch--primitive-handle-xy
scad-sketch--move-primitive-handle-to
```

The selection layer decides what can be hovered or selected. The editing layer decides what moving the handle means.

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

Preview rendering:

```elisp
scad-sketch--preview-p
scad-sketch-preview-until-next-input
scad-sketch--draw-preview-tree
```

Normal rendering includes editor affordances:

```text
grid
cursor point
marks
labels
handles
boolean boxes
mirror axes
selection styling
attention halos
HUD
```

Preview rendering omits those affordances and draws a cleaner semantic view.

Boolean rendering uses SVG compound paths, masks, clip paths, and simple painter-style preview behavior. The project intentionally avoids implementing a full polygon clipping/boolean engine.

Text rendering has two modes:

```text
visible text     white fill with dark outline
mask text        flat fill only, used for difference/intersection masks
```

### Keymap ownership

The keymap lives in `scad-sketch-editor-mode.el`.

High-level maps:

```text
scad-sketch-editor-mode-map
  top-level movement, marks, attention, selection, coordinates, session commands

scad-sketch-editor-insert-map
  insertion and drawing commands under i

scad-sketch-editor-group-map
  grouping, boolean wrapping, mirror wrapping, and break-apart under b
```

The mode docstring should describe concepts and defer exact key listings to native Emacs help:

```text
C-h m
?
describe-bindings
```

When adding a new command, prefer to put the implementation in the relevant subsystem and only bind it in `scad-sketch-editor-mode.el`.

### Adding a new primitive shape

To add a new primitive, for example `ellipse`, the usual checklist is:

1. Parse it in `scad-sketch-parse.el`.
2. Add a shape constructor in `scad-sketch-session.el`.
3. Convert parser node to shape/tree in the session conversion layer.
4. Add shape/tree emission in `scad-sketch-session.el`.
5. Add bounds/path support in `scad-sketch-editor--rendering.el`.
6. Add a per-shape overlay renderer.
7. Add handle count/positions in `scad-sketch-editor--selection.el`.
8. Add handle movement behavior in `scad-sketch-editor--editing.el`.
9. Add minibuffer parameter commands if useful.
10. Add focused ERT tests.
11. Add examples to `tests/test.scad` or a targeted fixture.

### Tests

The test suite is ERT-based.

Run all tests with:

```sh
bash unittest.sh
```

A direct batch invocation is also possible:

```sh
emacs --batch -Q \
  --eval "(progn
            (dolist (file (directory-files \"tests\" t \"-test\\\\.el\\\\'\"))
              (load-file file))
            (ert-run-tests-batch-and-exit))"
```

Current test layers:

```text
parser/unparser tests
  tokenizer, arrays, primitives, transforms, booleans, node-at, path-to,
  scope-aware variable lookup, unparse, top-level fixture parsing

session/write-back tests
  direct arrays, inline polygons, variable refs, polyRound, primitives,
  generic blank blocks, source-style preservation

selection/group tests
  :boolean vs :boolean-members, attention, focus, selection expansion,
  break target semantics, sequence tree behavior

rendering invariant tests
  preview affordance suppression, mirror preview vs normal mirror rendering,
  text-in-difference mask behavior, visible text styling

complex fixture tests
  dense integration fixture with arrays, transforms, booleans, text,
  mirror, inline polygons, variable refs, and polyRound references
```

GitHub Actions can run the suite by invoking:

```sh
bash unittest.sh
```

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

polygon([
  [0, 0],
  [40, 0],
  [50, 20],
  [40, 40],
  [0, 40]
]);

polygon(polyRound([
  [0, 0, 3],
  [80, 0, 3],
  [80, 50, 3],
  [0, 50, 3]
], 32));

circle(r=20);

square([80, 40]);

text("hello", size=12, font="Liberation Sans");

difference() {
  union() {
    square([80, 40]);
    translate([80, 0])
      circle(r=20);
  }

  circle(r=10);

  translate([40, 20])
    circle(r=5);

  text("CUT", size=10);
}

mirror([1, 0])
  polygon([[0,0], [20,0], [10,17]]);
```

Smoke-test sequence:

```text
C-c C-a on direct array
C-c C-a on polygon variable ref
C-c C-a on inline polygon
C-c C-a on inline polyRound
C-c C-a on circle
C-c C-a on square
C-c C-a on text
C-c C-a inside nested boolean tree
C-c C-a on mirror tree
C-c C-. on blank source location

TAB through hovered stack
M-TAB through global focus refs
SPC select/deselect
S-arrows move selected refs
S-SPC preview
i p draw polygon
i b draw square
i c draw circle
i t draw text
b u wrap union
b d wrap difference
b i wrap intersection
b m wrap mirror
b b break group
R set radius
A set mirror axis
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
tests for bug-shaped behavior
```

The best user experience is one where generated SCAD remains readable enough that users can comfortably keep editing it by hand.

## Changelog

### 2026 06 06

* Added e editor keybinding to toggle polygon point emission between inline points and a local points variable. Inline polygons can now be rewritten as pts = ...; polygon(pts);, and variable-backed polygons can be forced back inline without rewriting the original source array.
* Updated the point-extraction toggle so forcing a variable-backed polygon inline also deletes the now-obsolete source array assignment on write-back.
* Centralized the default `polyRound` segment count as `scad-sketch-default-polyround-fn` in the parser module and reused it from session emission.
* Improved target discovery under unsupported wrappers: supported 2D descendants inside forms like `linear_extrude(5) circle(...)` or `rotate([0, 10, 45]) linear_extrude(5) square(...)` can now be edited directly while unsupported wrappers remain outside the write-back region.
* Added parser support for scalar `square(N)`, canonicalized on write-back as `square([N, N])`.
* Updated undo snapshots to preserve the session dirty flag, so undoable mark changes remain clean while undoing source edits can restore the correct clean/dirty state.

### Previous Changes

* Split editor implementation into focused subsystem files: core, cursor, refs, selection, editing, rendering, and mode assembly.
* Added parser/session support for primitive `circle`, `square`, and `text` nodes.
* Added support for 2D boolean trees: `union`, `difference`, and `intersection`.
* Added support for `mirror([mx, my])` as an editable tree wrapper.
* Added editable primitive handles for circles, squares, text origins, and mirror axes.
* Added hover/focus/attention model with hover cycling and global focus cycling.
* Added `:boolean` and `:boolean-members` group attention refs.
* Added group wrapping commands under the `b` prefix for union, difference, intersection, and mirror.
* Added group break-apart behavior for attentioned group wrappers.
* Added insertion/drawing prefix under `i` for points, polygons, squares, circles, text, lines, and rectangles.
* Changed default blank insertion from “new named array” to “generic editable block.”
* Preserved polygon source style on write-back: inline stays inline, variable refs stay refs, polyRound stays polyRound.
* Removed automatic `_sketch_N` extraction for large inline polygons.
* Added transient clean preview mode on `S-SPC`.
* Improved text rendering: visible text uses white fill with dark outline; text in boolean masks uses glyph text rather than bounding boxes.
* Improved label rendering so selected/attentioned objects color their labels without heavy label boxes.
* Added GitHub Actions workflow support for running `bash unittest.sh` on `master` pushes and pull requests.
* Expanded ERT coverage across parser/unparser, session/write-back, selection/group behavior, rendering invariants, and complex fixture integration.
