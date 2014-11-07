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

  function Sample(l : LightRef; gen : RandRef; lluminatingPoint : float3) return LightSample is
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


  function Sample(l : AreaLight; gen : RandRef; lluminatingPoint : float3) return LightSample is
    r1  : float := gen.rnd_uniform(0.0, 1.0);
    r2  : float := gen.rnd_uniform(0.0, 1.0);
    cosTheta : float; -- := max(dot(sdir, (-1.0)*lsam.norm), 0.0);
    rayDir   : float3;
    d   : float;
    res : LightSample;
  begin
    res.pos.x := l.boxMin.x + r1*(l.boxMax.x - l.boxMin.x);
    res.pos.y := l.boxMin.y;
    res.pos.z := l.boxMin.z + r2*(l.boxMax.z - l.boxMin.z);
    res.norm  := l.normal;

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


  function Sample(l : SphereLight; gen : RandRef; lluminatingPoint : float3) return LightSample is
    r1  : float := gen.rnd_uniform(0.0, 1.0);
    r2  : float := gen.rnd_uniform(0.0, 1.0);
    res : LightSample;
  begin
    return res;
  end  Sample;

  function EvalPDF(l : SphereLight; lluminatingPoint : float3; rayDir : float3; hitDist : float) return float is
  begin
   return 1.0/l.surfaceArea;
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


