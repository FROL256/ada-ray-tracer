with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;


use Ada.Numerics;
use Ada.Text_IO;

package body Vector_Math is

  package Float_Functions is new Generic_Elementary_Functions (float);
  use Float_Functions;

  function safe_tan(x : float) return float is
     Half_Pi: constant float :=  Ada.Numerics.Pi*0.5;
  begin
    if abs(x) = Half_Pi then
      return float'Last;
    else
      return tan(x);
    end if;
  end;

  function pow(a,b : float) return float is
  begin
    return a ** b;
  end pow;

  function normalize (a : float3) return float3 is
    l_inv : float;
  begin
    l_inv := 1.0/sqrt(dot(a,a));
    return (l_inv*a.x, l_inv*a.y, l_inv*a.z);
  end normalize;

  function length(a : float3) return float is
  begin
    return sqrt(dot(a,a));
  end length;

  function reflect (dir : float3; normal: float3) return float3 is
  begin
    return normalize( (normal * dot(dir,normal) * (-2.0)) + dir);
  end reflect;


  function RotationMatrix(angle : float; a_v : float3) return float4x4 is
    M : float4x4;
    v : float3;
    cos_t, sin_t : float;
  begin

    M := IdentityMatrix;
    v := normalize(a_v);

    cos_t := cos(angle);
    sin_t := sin(angle);

    M(0,0) := (1.0-cos_t)*v.x*v.x + cos_t;
    M(0,1) := (1.0-cos_t)*v.x*v.y - sin_t*v.z;
    M(0,2) := (1.0-cos_t)*v.x*v.z + sin_t*v.y;

    M(1,0) := (1.0-cos_t)*v.y*v.x + sin_t*v.z;
    M(1,1) := (1.0-cos_t)*v.y*v.y + cos_t;
    M(1,2) := (1.0-cos_t)*v.y*v.z - sin_t*v.x;

    M(2,0) := (1.0-cos_t)*v.x*v.z - sin_t*v.y;
    M(2,1) := (1.0-cos_t)*v.z*v.y + sin_t*v.x;
    M(2,2) := (1.0-cos_t)*v.z*v.z + cos_t;

    return M;

  end RotationMatrix;

  function LookAtMatrix(eye, center, up: float3) return float4x4 is
    M : float4x4;
    f,s,u : float3;
  begin

    M := IdentityMatrix;

    f := normalize(center - eye);
    s := cross(f,up);
    u := cross(s,f);

    M(0,0) :=  f.x; M(0,1) :=  f.y; M(0,2) := f.z;
    M(1,0) :=  u.x; M(1,1) :=  u.y; M(1,2) := u.z;
    M(2,0) := -f.x; M(2,1) := -f.y; M(2,2) := -f.z;

    M(0,3) := -eye.x;
    M(1,3) := -eye.y;
    M(2,3) := -eye.z;

    return M;

  end LookAtMatrix;


  function "*"(m : float4x4; v : float3) return float3 is
    res : float3;
  begin
    res.x := m(0,0)*v.x + m(0,1)*v.y + m(0,2)*v.z + m(0,3);
    res.y := m(1,0)*v.x + m(1,1)*v.y + m(1,2)*v.z + m(1,3);
    res.z := m(2,0)*v.x + m(2,1)*v.y + m(2,2)*v.z + m(2,3);
    return res;
  end;

  function TransformNormal(m : float4x4; n : float3) return float3 is
    res : float3;
  begin
    res.x := m(0,0)*n.x + m(0,1)*n.y + m(0,2)*n.z;
    res.y := m(1,0)*n.x + m(1,1)*n.y + m(1,2)*n.z;
    res.z := m(2,0)*n.x + m(2,1)*n.y + m(2,2)*n.z;
    return res;
  end;



  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ----------------------------------------------------------------- Monte-Carlo Path Tracing -----------------------------------------------------------------------------------
  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  --

  ---- this function is for path tracing parameters; it can be redefifed in further subclasses
  --

  function rnd_uniform(gen : access RandomGenerator; l,h : float) return float is
    t : float;
  begin
    t := Ada.Numerics.Float_Random.Random(Gen => gen.agen);
    return l + (h-l)*t;
  end rnd_uniform;

  ---- this function is used for simple random and must not be redefined
  --
  function rnd_uniform_simple(gen : RandomGenerator; l,h : float) return float is
    t: float := 0.0;
  begin
    t := Ada.Numerics.Float_Random.Random(Gen => gen.agen);
    return l + (h-l)*t;
  end rnd_uniform_simple;

  ----
  --
  procedure ResetSequenceCounter(gen : in out RandomGenerator) is
  begin
    null;
  end ResetSequenceCounter;

  procedure InitSequence(gen : in out RandomGenerator) is
  begin
    null;
  end InitSequence;

  procedure NextSample(gen           : in out RandomGenerator;
                       I             : in float;
                       oldI          : in out float;
                       totalSamples  : in integer;
                       contrib       : in float3;
                       oldsample     : in out MLTSample;
                       contribsample : out MLTSample) is
  begin
    null;
  end NextSample;

  procedure RestoreSequence(gen : in out RandomGenerator) is
  begin
    null;
  end  RestoreSequence;

  procedure ClearStack(gen : in out RandomGenerator) is
  begin
    null;
  end ClearStack;

  procedure ResetAllModifyCounters(gen : in out RandomGenerator) is
  begin
    null;
  end ResetAllModifyCounters;




  function MapSampleToCosineDist(r1,r2 : float; direction, normal : float3; power : float) return float3 is
   e,sin_phi,cos_phi: float;
   sin_theta,cos_theta : float;
   deviation,nx,ny,nz,tmp,res : float3;
   invSign : float;
  begin

    e := power;
    sin_phi := sin(2.0*r1*M_PI);
    cos_phi := cos(2.0*r1*M_PI);

    cos_theta := (1.0-r2) ** (1.0/(e+1.0));
    sin_theta := sqrt(1.0-cos_theta*cos_theta);

    deviation := (sin_theta*cos_phi, sin_theta*sin_phi, cos_theta);

    ny := direction;
    nx := normalize(cross(ny, (1.04,2.93,-0.6234)));
    nz := normalize(cross(nx, ny));

    tmp := ny; ny := nz; nz := tmp; -- swap(ny,nz);  // depends on the coordinate system

    res := nx*deviation.x + ny*deviation.y + nz*deviation.z;

    if dot(direction, normal) > 0.0 then
      invSign := 1.0;
    else
      invSign := -1.0;
    end if;

    if invSign*dot(res, normal) < 0.0 then
      res := (-1.0)*nx*deviation.x + ny*deviation.y - nz*deviation.z;
    end if;

    return res;

  end MapSampleToCosineDist;

  function RandomCosineVectorOf(gen : RandRef; norm : float3) return float3 is
    r1 : float := gen.rnd_uniform(0.0, 1.0);
    r2 : float := gen.rnd_uniform(0.0, 1.0);
  begin
    return MapSampleToCosineDist(r1,r2,norm,norm,1.0);
  end RandomCosineVectorOf;

  function RandomCosineVectorOf(gen : RandRef; refl : float3;  norm : float3; cosPower : float) return float3 is
    r1 : float := gen.rnd_uniform(0.0, 1.0);
    r2 : float := gen.rnd_uniform(0.0, 1.0);
  begin
    return MapSampleToCosineDist(r1,r2,refl,norm,cosPower);
  end RandomCosineVectorOf;


end Vector_Math;
