with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Vector_Math;

use Interfaces;
use Vector_Math;


package body Lights is

  package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(float);
  use Float_Functions;


  function GetIntensity(l : LightRef) return float3 is
  begin
    return GetIntensity(l.all);
  end GetIntensity;

  function Sample(l : LightRef; gen : RandRef; lluminatingPoint : float3) return ShadowSample is
  begin
    return Sample(l.all, gen, lluminatingPoint);
  end Sample;

  function EvalPDF(l : LightRef; lluminatingPoint : float3; rayDir : float3; hitDist : float) return float is
  begin
    return EvalPDF(l.all, lluminatingPoint, rayDir, hitDist);
  end EvalPDF;

  function GetShapeType(l : LightRef) return LightShapes is
  begin
    return GetShapeType(l.all);
  end GetShapeType;


  -- explicit light sampling utils
  --
  epsilonDiv : constant float := 1.0e-20; -- small value for bsdf/pdf divisions


  function PdfAtoW(aPdfA : in float; aDist : in float; aCosThere : in float) return float is
  begin
    return aPdfA*aDist*aDist/max(aCosThere, epsilonDiv);
  end PdfAtoW;


  ---- Area Light
  ----
  function AreaPDF(l : AreaLight) return float is
  begin
    return 1.0/l.surfaceArea;
  end AreaPDF;


  function Sample(l : AreaLight; gen : RandRef; lluminatingPoint : float3) return ShadowSample is
    r1  : float := gen.rnd_uniform(0.0, 1.0);
    r2  : float := gen.rnd_uniform(0.0, 1.0);
    cosTheta : float; -- := max(dot(sdir, (-1.0)*lsam.norm), 0.0);
    rayDir   : float3;
    d   : float;
    res : ShadowSample;
  begin
    res.pos.x := l.boxMin.x + r1*(l.boxMax.x - l.boxMin.x);
    res.pos.y := l.boxMin.y;
    res.pos.z := l.boxMin.z + r2*(l.boxMax.z - l.boxMin.z);
    res.dir   := l.normal;

    rayDir    := res.pos - lluminatingPoint;
    d         := length(rayDir);
    rayDir    := rayDir*(1.0/d);
    cosTheta  := max(dot(rayDir, (-1.0)*l.normal), 0.0);

    res.pdf       := PdfAtoW(AreaPDF(l), d, cosTheta);
    res.intensity := l.intensity;

    return res;

  end  Sample;

  function EvalPDF(l : AreaLight; lluminatingPoint : float3; rayDir : float3; hitDist : float) return float is
   cosTheta : float := max(dot(rayDir, (-1.0)*l.normal), 0.0);
  begin
   return PdfAtoW(AreaPDF(l), hitDist, cosTheta);
  end EvalPDF;


  function GetIntensity(l : AreaLight) return float3 is
  begin
    return l.intensity;
  end GetIntensity;

  function GetShapeType(l : AreaLight) return LightShapes is
  begin
    return Light_Shape_Rect;
  end GetShapeType;


  ---- Sphere Light
  ----
  function AreaPDF(l : SphereLight) return float is
  begin
    return 1.0/l.surfaceArea;
  end AreaPDF;


  procedure CoordinateSystem(v1 : in float3; v2 : out float3; v3 : out float3) is
    invLen : float;
  begin

    if abs(v1.x) > abs(v1.y) then
      invLen := 1.0 / sqrt(v1.x*v1.x + v1.z*v1.z);
      v2     := (-v1.z * invLen, 0.0, v1.x * invLen);
    else
      invLen := 1.0 / sqrt(v1.y*v1.y + v1.z*v1.z);
      v2     := (0.0, v1.z * invLen, -v1.y * invLen);
    end if;

    v3 := cross(v1, v2);

  end CoordinateSystem;

  function DistanceSquared(a : float3; b : float3) return float is
    diff : float3 := (b - a);
  begin
    return dot(diff, diff);
    exception                  -- floating point overflow may happen due to diff.x*diff.x may be too large
      when Constraint_Error =>
      return float'Last;
  end DistanceSquared;


  function UniformSampleSphere(u1 : float; u2 : float) return float3 is
    x,y,z,r,phi : float;
  begin
    z   := 1.0 - 2.0 * u1;
    r   := sqrt(max(0.0, 1.0 - z*z));
    phi := 2.0 * M_PI * u2;
    x   := r * cos(phi);
    y   := r * sin(phi);
    return (x,y,z);
  end UniformSampleSphere;

  function UniformSampleCone(u1 : float; u2 : float; costhetamax : float; x : float3; y : float3; z : float3) return float3 is
    phi,costheta,sintheta : float;
  begin
    costheta := lerp(u1, costhetamax, 1.0);
    sintheta := sqrt(1.0 - costheta*costheta);
    phi      := u2 * 2.0 * M_PI;
    return cos(phi) * sintheta * x + sin(phi) * sintheta * y +  costheta * z;
  end UniformSampleCone;

  function UniformConePdf(cosThetaMax : float) return float is
  begin
    return 1.0 / (2.0 * M_PI * (1.0 - cosThetaMax));
    exception
    when Constraint_Error =>
    return 0.0;                     -- #NOTE: may be need return 1 ??
  end UniformConePdf;

  function RaySphereIntersect(rayPos : float3; rayDir : float3; sphPos : float3; radius : float) return float2 is
    t1, t2 : float;
    k      : float3;
    b, c, d, sqrtd : float;
    res : float2;
  begin

    k := rayPos - sphPos;
    b := dot(k,rayDir);
    c := dot(k,k) - radius*radius;
    d := b * b - c;

    if d >= 0.0 then

      sqrtd := sqrt(d);
      t1 := -b - sqrtd;
      t2 := -b + sqrtd;

      res.x := min(t1,t2);
      res.y := max(t1,t2);

    else

     res.x := -infinity;
     res.y := -infinity;

    end if;

    return res;

  exception
    when Constraint_Error =>
    return (-infinity,-infinity);
  end RaySphereIntersect;


  function Sample(l : SphereLight; gen : RandRef; lluminatingPoint : float3) return ShadowSample is
    u1  : float := gen.rnd_uniform(0.0, 1.0);
    u2  : float := gen.rnd_uniform(0.0, 1.0);

    wc, wcX, wcY : float3;
    sinThetaMax2,cosThetaMax,thit : float;
    rpos, rdir : float3;
    hitMinMax : float2;

    res : ShadowSample;
  begin
    res.intensity := l.intensity;

    if DistanceSquared(lluminatingPoint, l.center) - l.radius*l.radius < 1.0e-4 then
      res.pos  := l.center + l.radius*UniformSampleSphere(u1, u2);
      res.dir  := normalize(res.pos - l.center);
      return res;
    end if;

    wc := normalize(l.center - lluminatingPoint);
    CoordinateSystem(wc, v2 => wcX, v3 => wcY);

    sinThetaMax2 := l.radius*l.radius / DistanceSquared(lluminatingPoint, l.center);
    cosThetaMax  := sqrt(max(0.0, 1.0 - sinThetaMax2));

    rdir := UniformSampleCone(u1, u2, cosThetaMax, wcX, wcY, wc);
    rpos := lluminatingPoint + rdir*(1.0e-3);

    -- calc ray sphere intersection and store hit distance in thit
    --
    hitMinMax := RaySphereIntersect(rpos, rdir, l.center, l.radius);

    if hitMinMax.x < 0.0 then -- !Intersect(r, &thit, &rayEpsilon, &dgSphere)
      thit := dot(l.center - lluminatingPoint, normalize(rdir));
    else
      thit := hitMinMax.x;
    end if;

    res.pos := rpos + thit*rdir;
    res.dir := normalize(res.pos - l.center);
    res.pdf := EvalPDF(l, lluminatingPoint, rdir, thit);

    return res;
  end  Sample;

  function EvalPDF(l : SphereLight; lluminatingPoint : float3; rayDir : float3; hitDist : float) return float is
    sinThetaMax2, cosThetaMax : float;
  begin

    if DistanceSquared(lluminatingPoint, l.center) - l.radius*l.radius < 1.0e-4 then
      return 1.0/l.surfaceArea;
    end if;

    sinThetaMax2 := l.radius*l.radius / DistanceSquared(lluminatingPoint, l.center);
    cosThetaMax  := sqrt(max(0.0, 1.0 - sinThetaMax2));

    return UniformConePdf(cosThetaMax);

  end EvalPDF;


  function GetIntensity(l : SphereLight) return float3 is
  begin
    return l.intensity;
  end GetIntensity;

  function GetShapeType(l : SphereLight) return LightShapes is
  begin
    return Light_Shape_Sphere;
  end GetShapeType;

end Lights;


