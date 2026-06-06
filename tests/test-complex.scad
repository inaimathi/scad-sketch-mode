// ── Maximally complex simple 2D edit target ────────────────────────────────
//
// This is intentionally dense but still parser-friendly.  It combines:
//   - all currently supported 2D leaf shapes: polygon, square, circle, text
//   - inline and variable-ref polygons
//   - polyRound inline and polyRound variable references
//   - translate / rotate / scale / mirror
//   - nested union / difference / intersection
//
// Try invoking scad-sketch-at-point inside different descendants of this tree.

badge_outline = [
  [0,   0,   8],
  [120, 0,   8],
  [120, 70,  8],
  [0,   70,  8]
];

badge_chevron = [
  [12, 35],
  [34, 58],
  [56, 35],
  [34, 12]
];

badge_starish = [
  [0,  18, 2],
  [16, 18, 2],
  [22,  0, 2],
  [28, 18, 2],
  [44, 18, 2],
  [31, 28, 2],
  [36, 46, 2],
  [22, 35, 2],
  [8,  46, 2],
  [13, 28, 2]
];

translate([300, 140])
  rotate(-8)
    difference() {
      union() {
        // Main rounded badge body
        polygon(polyRound(badge_outline, 48));

        // Left circular lobe merged into the body
        translate([0, 35])
          circle(r=28);

        // Right circular lobe merged into the body
        translate([120, 35])
          circle(d=56);

        // Small centered cap, intentionally overlapping several things
        translate([42, 52])
          square([36, 22]);

        // Variable-ref polygon
        translate([58, 0])
          polygon(badge_chevron);

        // Inline polygon, with transform stack
        translate([60, 35])
          rotate(45)
            scale([1.2, 0.65])
              polygon([
                [-12, -12],
                [12,  -12],
                [12,   12],
                [-12,  12]
              ]);

        // Text as a positive/additive shape
        translate([22, 20])
          text("SCAD", size=13);

        // Rotated text as another positive shape
        translate([72, 14])
          rotate(12)
            text("SKETCH", size=9);
      }

      // First subtraction stage: center holes and label knockout
      union() {
        translate([60, 35])
          circle(r=16);

        translate([60, 35])
          rotate(45)
            square([18, 18], center=true);

        translate([20, 52])
          text("CUT", size=8);
      }

      // Second subtraction stage: clipped star-ish bite in the lower right
      translate([82, 8])
        intersection() {
          polygon(polyRound(badge_starish, 32));

          translate([22, 22])
            circle(r=24);

          mirror([1, 0])
            translate([-44, 0])
              square([44, 44]);
        }

      // Third subtraction stage: nested boolean with transforms
      translate([18, 10])
        rotate(20)
          difference() {
            square([32, 18], center=true);

            translate([-8, 0])
              circle(r=5);

            translate([8, 0])
              circle(r=5);
          }
    }
