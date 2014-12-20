with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;

use Ada.Numerics;
--use Ada.Numerics.Aux;

package body Generic_Vector_Math is

  package Float_Functions is new Generic_Elementary_Functions (float);
  use Float_Functions;

  function BitCopyToTypeT is new Ada.Unchecked_Conversion(float, T);

  function ZeroT return T is
  begin
    return BitCopyToTypeT(0.0);
  end ZeroT;

  function min (a, b : T) return T is
    begin
      if a < b then
        return a;
      else
        return b;
      end if;
   end;

  function max (a, b : T) return T is
  begin
    if a >= b then
      return a;
    else
      return b;
    end if;
  end;

  function min (a, b, c : T) return T is
    begin
      if a < b and a < c then
         return a;
      elsif b < c and b < a then
         return b;
      else
         return c;
      end if;
   end;

  function max (a, b, c : T) return T is
  begin
      if a >= b and a >= c then
         return a;
      elsif b >= c and b >= a then
         return b;
      else
         return c;
      end if;
  end;


  function clamp(x,a,b : T) return T is
  begin
    return min(max(x,a),b);
  end clamp;

  function sqr(x : T) return T is
  begin
    return x*x;
  end sqr;


  function "+" (a, b : vector4) return vector4 is
    res : vector4;
  begin
    res.x := a.x + b.x;
    res.y := a.y + b.y;
    res.z := a.z + b.z;
    res.w := a.w + b.w;
    return res;
  end;

  function "+" (a, b : vector3) return vector3 is
    res : vector3;
  begin
    res.x := a.x + b.x;
    res.y := a.y + b.y;
    res.z := a.z + b.z;
    return res;
  end;

  function "-" (a, b : vector3) return vector3 is
    res : vector3;
  begin
    res.x := a.x - b.x;
    res.y := a.y - b.y;
    res.z := a.z - b.z;
    return res;
  end;

  function "*" (a, b : vector3) return vector3 is
    res : vector3;
  begin
    res.x := a.x * b.x;
    res.y := a.y * b.y;
    res.z := a.z * b.z;
    return res;
  end;

  function dot(a, b : vector3) return T is
  begin
    return a.x*b.x + a.y*b.y + a.z*b.z;
  end;

  function cross(a, b : vector3) return vector3 is
    res : vector3;
  begin
    res.x := a.y*b.z - b.y*a.z;
    res.y := a.z*b.x - b.z*a.x;
    res.z := a.x*b.y - b.x*a.y;
    return res;
  end;

  function min(a, b : vector3) return vector3 is
    res : vector3;
  begin
    res.x := min(a.x, b.x);
    res.y := min(a.y, b.y);
    res.z := min(a.z, b.z);
    return res;
  end;

  function max(a, b : vector3) return vector3 is
  begin
    return (max(a.x,b.x), max(a.y,b.y), max(a.z, b.z));
  end;

  function clamp(x : vector3; a,b : T) return vector3 is
  begin
    return (clamp(x.x,a,b), clamp(x.y,a,b), clamp(x.z,a,b));
  end;

  function clamp(x,a,b : vector3) return vector3 is
  begin
    return (clamp(x.x,a.x,b.x), clamp(x.y,a.y,b.y), clamp(x.z,a.z,b.z));
  end;

  function "*" (a : vector3; k : T) return vector3 is
  begin
    return (k*a.x, k*a.y, k*a.z);
  end;

  function "*"(k: T; a : vector3) return vector3 is
  begin
    return (k*a.x, k*a.y, k*a.z);
  end;

  function "*"(v : vector3; m : Matrix4) return vector3 is
    res : vector3;
  begin
    res.x := v.x*m(0,0) + v.y*m(1,0) + v.z*m(2,0) + m(3,0);
    res.y := v.x*m(0,1) + v.y*m(1,1) + v.z*m(2,1) + m(3,1);
    res.z := v.x*m(0,2) + v.y*m(1,2) + v.z*m(2,2) + m(3,2);
    return res;
  end;

  function "*"(v : vector4; m : Matrix4) return vector4 is
     res : vector4;
  begin
    res.x := v.x*m(0,0) + v.y*m(1,0) + v.z*m(2,0) + v.w*m(3,0);
    res.y := v.x*m(0,1) + v.y*m(1,1) + v.z*m(2,1) + v.w*m(3,1);
    res.z := v.x*m(0,2) + v.y*m(1,2) + v.z*m(2,2) + v.w*m(3,2);
    res.w := v.x*m(0,3) + v.y*m(1,3) + v.z*m(2,3) + v.w*m(3,3);
    return res;
  end;

  function "*"(m : Matrix4; v : vector3) return vector3 is
    res : vector3;
  begin
    res.x := m(0,0)*v.x + m(0,1)*v.y + m(0,2)*v.z + m(0,3);
    res.y := m(1,0)*v.x + m(1,1)*v.y + m(1,2)*v.z + m(1,3);
    res.z := m(2,0)*v.x + m(2,1)*v.y + m(2,2)*v.z + m(2,3);
    return res;
  end;

  function "*"(m : Matrix4; v : vector4) return vector4 is
    res : vector4;
  begin
    res.x := m(0,0)*v.x + m(0,1)*v.y + m(0,2)*v.z + m(0,3)*v.w;
    res.y := m(1,0)*v.x + m(1,1)*v.y + m(1,2)*v.z + m(1,3)*v.w;
    res.z := m(2,0)*v.x + m(2,1)*v.y + m(2,2)*v.z + m(2,3)*v.w;
    res.w := m(3,0)*v.x + m(3,1)*v.y + m(3,2)*v.z + m(3,3)*v.w;
    return res;
  end;


  function GetRow(m : Matrix4; i : integer) return vector4 is
    res : vector4;
  begin
    res.x := m(i,0);
    res.y := m(i,1);
    res.z := m(i,2);
    res.w := m(i,3);
    return res;
  end GetRow;

  function GetCol(m : Matrix4; i : integer) return vector4 is
    res : vector4;
  begin
    res.x := m(0,i);
    res.y := m(1,i);
    res.z := m(2,i);
    res.w := m(3,i);
    return res;
  end GetCol;

  procedure SetRow(m : in out Matrix4; i : in integer; v : in vector4) is
  begin
     m(i,0) := v.x;
     m(i,1) := v.y;
     m(i,2) := v.z;
     m(i,3) := v.w;
  end SetRow;

  procedure SetCol(m : in out Matrix4; i : in integer; v : in vector4) is
  begin
    m(0,i) := v.x;
    m(1,i) := v.y;
    m(2,i) := v.z;
    m(3,i) := v.w;
  end SetCol;

  function "*"(m1 : Matrix4; m2 : Matrix4) return Matrix4 is
    m   : Matrix4;
  begin

    m(0,0) := m1(0,0) * m2(0,0) + m1(0,1) * m2(1,0) + m1(0,2) * m2(2,0) + m1(0,3) * m2(3,0);
    m(0,1) := m1(0,0) * m2(0,1) + m1(0,1) * m2(1,1) + m1(0,2) * m2(2,1) + m1(0,3) * m2(3,1);
    m(0,2) := m1(0,0) * m2(0,2) + m1(0,1) * m2(1,2) + m1(0,2) * m2(2,2) + m1(0,3) * m2(3,2);
    m(0,3) := m1(0,0) * m2(0,3) + m1(0,1) * m2(1,3) + m1(0,2) * m2(2,3) + m1(0,3) * m2(3,3);
    m(1,0) := m1(1,0) * m2(0,0) + m1(1,1) * m2(1,0) + m1(1,2) * m2(2,0) + m1(1,3) * m2(3,0);
    m(1,1) := m1(1,0) * m2(0,1) + m1(1,1) * m2(1,1) + m1(1,2) * m2(2,1) + m1(1,3) * m2(3,1);
    m(1,2) := m1(1,0) * m2(0,2) + m1(1,1) * m2(1,2) + m1(1,2) * m2(2,2) + m1(1,3) * m2(3,2);
    m(1,3) := m1(1,0) * m2(0,3) + m1(1,1) * m2(1,3) + m1(1,2) * m2(2,3) + m1(1,3) * m2(3,3);
    m(2,0) := m1(2,0) * m2(0,0) + m1(2,1) * m2(1,0) + m1(2,2) * m2(2,0) + m1(2,3) * m2(3,0);
    m(2,1) := m1(2,0) * m2(0,1) + m1(2,1) * m2(1,1) + m1(2,2) * m2(2,1) + m1(2,3) * m2(3,1);
    m(2,2) := m1(2,0) * m2(0,2) + m1(2,1) * m2(1,2) + m1(2,2) * m2(2,2) + m1(2,3) * m2(3,2);
    m(2,3) := m1(2,0) * m2(0,3) + m1(2,1) * m2(1,3) + m1(2,2) * m2(2,3) + m1(2,3) * m2(3,3);
    m(3,0) := m1(3,0) * m2(0,0) + m1(3,1) * m2(1,0) + m1(3,2) * m2(2,0) + m1(3,3) * m2(3,0);
    m(3,1) := m1(3,0) * m2(0,1) + m1(3,1) * m2(1,1) + m1(3,2) * m2(2,1) + m1(3,3) * m2(3,1);
    m(3,2) := m1(3,0) * m2(0,2) + m1(3,1) * m2(1,2) + m1(3,2) * m2(2,2) + m1(3,3) * m2(3,2);
    m(3,3) := m1(3,0) * m2(0,3) + m1(3,1) * m2(1,3) + m1(3,2) * m2(2,3) + m1(3,3) * m2(3,3);

    return m;

  end;


end Generic_Vector_Math;
