(* Make tests in here as needed *)
open OUnit2
open Game

(** Test Plan: For our testing, we did a combination of glass box and
    black box testing. The black box testing was mostly simple funcitons
    that we could guess the ouputs of based on the name / documentation
    (ie, Vector.x), and we used glass box for the more complex functions
    we tested. Since most of our code worked with abstract data types
    across multiple files, it was difficult to test the more complicated
    parts of our game, but we mostly just tested what they did using
    [make play] and testing in the game.

    The files we tested in OUnit were Vector, Poly, Figure, and Move,
    since these files had functions that returned testable outputs,
    whereas the rest of the files had functions that returned mostly
    abstract values. These modules also formed the backbone of the
    system, so it was important to ensure they were correct using OUnit
    tests. We tested the rest of the modules manually in the game since
    it would have been much more difficult to test them using OUnit.
    Additionally, the effects of these files on the game are very easy
    to see on screen, so it was easier to just figure out if they were
    working manually.

    Our testing suite alone does not prove the full correctness of the
    game, as this would be practically impossible to achieve for such a
    complicated game. Our suite does however reliably test the important
    backbone components, making it easier to test the rest of the user
    interface manually, through game play.*)

let make_test
    (name : string)
    (print_fn : 'a -> string)
    (cmp_fn : 'a -> 'a -> bool)
    (exp : 'a)
    (obs : 'a) =
  name >:: fun _ -> assert_equal ~cmp:cmp_fn ~printer:print_fn exp obs

let v = Vector.init 4. (-3.)

let v2 = Vector.init 1. 1.

let v3 = Vector.init 1. (-1.)

let n = Vector.norm v

let r = Vector.rotate Float.pi v

let abs_val f = if f >= 0. then f else f *. -1.

let a_val_eq a b = if abs_val a = abs_val b then true else false

let vector_tests =
  [
    make_test "Vector test x coord" Bool.to_string Bool.equal true
      (Vector.x v = 4.);
    make_test "Vector test y coord" Bool.to_string Bool.equal true
      (Vector.y v = -3.);
    make_test "Vector test length v" Bool.to_string Bool.equal true
      (Vector.length v = 5.);
    make_test "Vector test length2 v" Bool.to_string Bool.equal true
      (Vector.length2 v = 25.);
    make_test "Vector test to_string v" Bool.to_string Bool.equal true
      (Vector.to_string v = "<4., -3.>");
    make_test "Vector test x coord norm v" Float.to_string Float.equal
      (3. /. 5.) (Vector.x n);
    make_test "Vector test y coord norm v" Bool.to_string Bool.equal
      true
      (Vector.y n = 4. /. 5.);
    make_test "Vector test length norm v" Bool.to_string Bool.equal true
      (Vector.length n = 1.);
    make_test "Vector test x coord rotate v" Float.to_string a_val_eq
      (-4.)
      (Float.round (Vector.x r));
    make_test "Vector test y coord rotate v" Float.to_string a_val_eq 3.
      (Float.round (Vector.y r));
    make_test "Vector test length rotate v" Float.to_string a_val_eq 5.
      (Vector.length r);
    make_test "Vector test dot n n" Float.to_string Float.equal 1.
      (Vector.dot n n);
    make_test "Vector test dot v2 v3" Float.to_string a_val_eq 0.
      (Vector.dot v2 v3);
    make_test "Vector test dot v2 v2" Float.to_string Float.equal 2.
      (Vector.dot v2 v2);
    make_test "Vector test dot v zero" Float.to_string a_val_eq 0.
      (Vector.dot v Vector.zero);
    make_test "Vector test to_ints v" Bool.to_string Bool.equal true
      (Vector.to_ints v = (4, -3));
    make_test "Vector test to_ints n" Bool.to_string Bool.equal true
      (Vector.to_ints n = (0, 0));
    make_test "Vector test to_ints r" Bool.to_string Bool.equal true
      (Vector.to_ints r = (-3, 3));
    make_test "Vector test v =/= v2" Bool.to_string Bool.equal false
      (Vector.equal v v2);
    make_test "Vector test v = v" Bool.to_string Bool.equal true
      (Vector.equal v v);
  ]

let origin = Vector.init 0. 0.

let bndryptA = Vector.init (-30.) 0.

let rectA = RigidPoly.init_rect 30. 30. origin

let rectB = RigidPoly.init_rect 30. 30. (Vector.init 20. 20.)

let rectC = RigidPoly.init_rect 100. 20. (Vector.init ~-.30. 30.)

let rectA' = RigidPoly.move (Vector.init 10. 10.) rectA

let poly_tests =
  [
    make_test "Poly test point (0, 0) in" Bool.to_string Bool.equal true
      (RigidPoly.point_in rectA origin);
    make_test "Poly test point (10, 10) in" Bool.to_string Bool.equal
      true
      (RigidPoly.point_in rectA (Vector.init 10. 10.));
    make_test "Poly test point (-10, -10) in" Bool.to_string Bool.equal
      true
      (RigidPoly.point_in rectA (Vector.init ~-.10. ~-.10.));
    make_test "Poly test point (30, 10) out" Bool.to_string Bool.equal
      false
      (RigidPoly.point_in rectA (Vector.init 30. 10.));
    make_test "Poly test point (30, -30) out" Bool.to_string Bool.equal
      false
      (RigidPoly.point_in rectA (Vector.init 30. ~-.30.));
    make_test "Poly test point (15, -15) in" Bool.to_string Bool.equal
      true
      (RigidPoly.point_in rectA (Vector.init 15. ~-.15.));
    make_test "Poly test overlap A and B" Bool.to_string Bool.equal true
      (RigidPoly.overlap rectA rectB);
    make_test "Poly test overlap B and A" Bool.to_string Bool.equal true
      (RigidPoly.overlap rectB rectA);
    make_test "Poly test overlap A and C" Bool.to_string Bool.equal
      false
      (RigidPoly.overlap rectA rectC);
    make_test "Poly test overlap C and A" Bool.to_string Bool.equal
      false
      (RigidPoly.overlap rectC rectA);
    make_test "Poly test overlap B and C" Bool.to_string Bool.equal true
      (RigidPoly.overlap rectB rectC);
    make_test "Poly test overlap C and B" Bool.to_string Bool.equal true
      (RigidPoly.overlap rectC rectB);
    make_test "Poly test point (0, 0) in A'" Bool.to_string Bool.equal
      true
      (RigidPoly.point_in rectA' origin);
    make_test "Poly test point (-20, 0) not in A'" Bool.to_string
      Bool.equal false
      (RigidPoly.point_in rectA' (Vector.init (-20.) 0.));
    make_test "Poly test overlap A' and A" Bool.to_string Bool.equal
      true
      (RigidPoly.overlap rectA' rectA);
    make_test "Poly test overlap A' and B" Bool.to_string Bool.equal
      true
      (RigidPoly.overlap rectA' rectB);
    make_test "Poly test overalp A' and C" Bool.to_string Bool.equal
      true
      (RigidPoly.overlap rectA' rectC);
  ]

let emptyfig = Figure.init_empty_figure ()

let damagedfig = Figure.add_damage emptyfig 10.

let grav_fig = Figure.add_grav emptyfig

let figure_tests =
  [
    make_test "Figure test direction facing" Bool.to_string Bool.equal
      true
      (Figure.dir_facing emptyfig);
    make_test "Figure test on ground" Bool.to_string Bool.equal false
      (Figure.on_ground emptyfig);
    make_test "Figure test get damage" Float.to_string Float.equal 0.
      (Figure.get_damage emptyfig);
    make_test "Figure test get name" Bool.to_string Bool.equal true
      (Figure.get_name emptyfig = "empty");
    make_test "Figure test add damage" Float.to_string Float.equal 10.
      (Figure.get_damage damagedfig);
    make_test "Figure test get force" Bool.to_string Bool.equal true
      (Figure.get_force emptyfig = Vector.zero);
    make_test "Figure test add gravity" Bool.to_string Bool.equal true
      (Figure.get_force grav_fig = Constants.grav);
    make_test "Figure test empty is not cpu" Bool.to_string Bool.equal
      false (Cpu.is_cpu emptyfig);
  ]

let basic_attack =
  Move.init_from_json
    (Yojson.Basic.from_file "data/moves/basic_attack.json")

let dbl_j =
  Move.init_from_json
    (Yojson.Basic.from_file "data/moves/double_jump.json")

let move_tests =
  [
    make_test "Move test get name" Bool.to_string Bool.equal true
      (Move.get_name basic_attack = "punch");
    make_test "Move test get where" Bool.to_string Bool.equal true
      (Move.get_where basic_attack = "ground");
    make_test "Move test additive force" Bool.to_string Bool.equal true
      (Move.additive_force basic_attack = (true, true));
    make_test "Move test get changeable" Bool.to_string Bool.equal false
      (Move.get_changeable basic_attack);
    make_test "Move test ready" Bool.to_string Bool.equal true
      (Move.ready basic_attack);
    make_test "Move test get controlled" Bool.to_string Bool.equal false
      (Move.get_controlled basic_attack);
    make_test "Move test get where dbl jump" Bool.to_string Bool.equal
      true
      (Move.get_where dbl_j = "air");
    make_test "Move test additive force dbl jump" Bool.to_string
      Bool.equal true
      (Move.additive_force dbl_j = (true, false));
    make_test "Move test jump kb = 0" Bool.to_string Bool.equal true
      (Move.get_kb dbl_j = Vector.zero);
    make_test "Move test dbl jump changeable" Bool.to_string Bool.equal
      false
      (Move.get_changeable dbl_j);
  ]

let suite =
  "test suite for Final Project"
  >::: List.flatten
         [ vector_tests; poly_tests; figure_tests; move_tests ]

let _ = run_test_tt_main suite