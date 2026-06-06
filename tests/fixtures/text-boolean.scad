difference() {
  union() {
    square([80, 40]);
    translate([80, 0])
      circle(r=20);
  }
  circle(r=10);
  translate([40, 20])
    circle(r=5);
  text("OpenSCAD", size=14);
}
