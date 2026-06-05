// ── 1. Array assignments ──────────────────────────────────────────────────────

// Simple 2-component points
triangle = [
  [0, 0],
  [50, 0],
  [25, 43]
];

// Points with radii (polyRound-style storage)
rounded_box = [
  [0,   0,   5],
  [100, 0,   5],
  [100, 60,  5],
  [0,   60,  5]
];

// Single-point degenerate case
dot = [
  [10, 20]
];


// ── 2. Standalone 2D primitives ───────────────────────────────────────────────

// circle: bare radius
circle(15);

// circle: r= keyword
circle(r=20);

// circle: d= keyword (parser halves it → r=12.5)
circle(d=25);

// square: plain
square([80, 40]);

// square: centered
square([60, 30], center=true);

// text: bare string
text("hello");

// text: with size
text("OpenSCAD", size=14);

// text: multiple keyword params (font ignored by parser, size kept)
text("hi", size=8, font="Liberation Sans");

// polygon: inline points (≤4 → inlined on unparse)
polygon([[0,0], [30,0], [15,26]]);

// polygon: inline points (>4 → extracted on unparse)
polygon([
  [0,   0],
  [40,  0],
  [50, 20],
  [40, 40],
  [0,  40]
]);

// polygon: variable reference
pts = [
  [0, 0],
  [20, 0],
  [10, 17]
];
polygon(pts);

// polygon: polyRound with inline array
polygon(polyRound([
  [0,   0,  3],
  [80,  0,  3],
  [80, 50,  3],
  [0,  50,  3]
], 32));

// polygon: polyRound with variable reference
polygon(polyRound(rounded_box, 64));


// ── 3. Transforms ─────────────────────────────────────────────────────────────

// translate
translate([10, 20])
  circle(r=5);

// rotate
rotate(45)
  square([20, 20]);

// scale
scale([2, 0.5])
  circle(r=10);

// mirror on X axis
mirror([1, 0])
  polygon([[0,0],[20,0],[10,17]]);

// mirror on Y axis
mirror([0, 1])
  square([30, 15]);

// nested transforms
translate([100, 50])
  rotate(30)
    scale([1.5, 1.5])
      circle(r=8);

// translate with no semicolon on child (brace-free single child)
translate([5, -10])
  square([40, 20], center=true);


// ── 4. Boolean / composition operations ──────────────────────────────────────

// difference: circle punched out of square
difference() {
  square([60, 60], center=true);
  circle(r=20);
}

// union: two shapes merged
union() {
  circle(r=15);
  translate([25, 0])
    circle(r=15);
}

// intersection
intersection() {
  square([40, 40], center=true);
  circle(r=25);
}

// nested booleans
difference() {
  union() {
    square([80, 40]);
    translate([80, 0])
      circle(r=20);
  }
  circle(r=10);
  translate([40, 20])
    circle(r=5);
}

// difference inside a translate
translate([200, 0])
  difference() {
    square([50, 50]);
    circle(r=15);
  }


// ── 5. Module body (should be descended into, harvesting inner arrays/shapes) ─

module profile() {
  body_pts = [
    [0,   0],
    [60,  0],
    [60, 40],
    [0,  40]
  ];
  polygon(body_pts);
  circle(r=5);
}

// include and use directives (should be silently skipped)
include <MCAD/shapes.scad>
use <utils/helpers.scad>

// 3D primitive at top level (should be silently skipped)
linear_extrude(height=10)
  circle(r=30);

// Variable assignment that is NOT an array (should be skipped)
fn = 32;
quality = "high";
