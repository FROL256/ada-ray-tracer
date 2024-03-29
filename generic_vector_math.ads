
with Interfaces;
with Ada.Numerics.Float_Random;

use Interfaces;

generic

  type T is private;

  with function "+" (L, R : T) return T is <>;
  with function "-" (L, R : T) return T is <>;
  with function "*" (L, R : T) return T is <>;
  with function "/" (L, R : T) return T is <>;

  with function ">" (L, R : T)  return Boolean is <>;
  with function "<" (L, R : T)  return Boolean is <>;
  with function ">=" (L, R : T) return Boolean is <>;
  with function "<=" (L, R : T) return Boolean is <>;

  --with function ":="(E2 : float) return T is <>;
  --with procedure assign(E1 : T; E2 : flost) is <>;

package Generic_Vector_Math is

  function min(a, b : T) return T;
  function max(a, b : T) return T;

  function min(a, b, c : T) return T;
  function max(a, b, c : T) return T;
  function sqr(x : T) return T;

  type vector2 is record
    x, y : T;
  end record;

  type vector3 is record
    x, y, z : T;
  end record;

  type vector4 is record
    x, y, z, w : T;
  end record;


  function "+"(a, b : vector4) return vector4;


  function "+"(a, b : vector3) return vector3;
  function "*"(a, b : vector3) return vector3;
  function "-"(a, b : vector3) return vector3;
  function "*"(a : vector3; k: T) return vector3;
  function "*"(k: T; a : vector3) return vector3;
  function dot(a, b : vector3) return T;
  function cross(a, b : vector3) return vector3;

  function min(a, b : vector3) return vector3;
  function max(a, b : vector3) return vector3;
  function clamp(x : vector3; a,b : T) return vector3;
  function clamp(x,a,b : vector3) return vector3;

  function clamp(x,a,b : T) return T;

  type Matrix4 is array (0..3, 0..3) of T;

  function "*"(v : vector3; m : Matrix4) return vector3;
  function "*"(v : vector4; m : Matrix4) return vector4;

  function "*"(m : Matrix4; v : vector3) return vector3;
  function "*"(m : Matrix4; v : vector4) return vector4;

  function "*"(m1 : Matrix4; m2 : Matrix4) return Matrix4;

  function GetRow(m : Matrix4; i : integer) return vector4;
  function GetCol(m : Matrix4; i : integer) return vector4;

  procedure SetRow(m : in out Matrix4; i : in integer; v : in vector4);
  procedure SetCol(m : in out Matrix4; i : in integer; v : in vector4);


  pragma Inline ("+");
  pragma Inline ("*");
  pragma Inline ("-");
  pragma Inline (dot);
  pragma Inline (min);
  pragma Inline (max);
  pragma Inline (sqr);
  pragma Inline (clamp);

end Generic_Vector_Math;

