with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Vector_Math;

use Interfaces;
use Vector_Math;


package body Lights is

  package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(float);
  use Float_Functions;

  function Sample(l : LightRef; gen : RandRef)  return LightSample is
  begin
    return Sample(l.all, gen);
  end Sample;

  function AreaPDF(l : LightRef) return float is
  begin
    return AreaPDF(l.all);
  end AreaPDF;

  function GetIntensity(l : LightRef) return float3 is
  begin
    return GetIntensity(l.all);
  end GetIntensity;

  ----------------------------------------------------------------------------------------------------------------------------------------
  ----
  function Sample(l : AreaLight; gen : RandRef)  return LightSample is
    r1  : float := gen.rnd_uniform(0.0, 1.0);
    r2  : float := gen.rnd_uniform(0.0, 1.0);
    res : LightSample;
  begin

    res.pos.x := l.boxMin.x + r1*(l.boxMax.x - l.boxMin.x);
    res.pos.y := l.boxMin.y;
    res.pos.z := l.boxMin.z + r2*(l.boxMax.z - l.boxMin.z);
    res.norm  := l.normal;
    res.intensity := l.intensity;

    return res;
  end Sample;

  function AreaPDF(l : AreaLight) return float is
  begin
    return 1.0/l.surfaceArea;
  end AreaPDF;

  function GetIntensity(l : AreaLight) return float3 is
  begin
    return l.intensity;
  end GetIntensity;

  ----------------------------------------------------------------------------------------------------------------------------------------
  ----
  function Sample(l : SphereLight; gen : RandRef)  return LightSample is
    --r1  : float := gen.rnd_uniform(0.0, 1.0);
    --r2  : float := gen.rnd_uniform(0.0, 1.0);
    res : LightSample;
  begin

    --res.pos.x := l.boxMin.x + r1*(l.boxMax.x - l.boxMin.x);
    --res.pos.y := l.boxMin.y;
    --res.pos.z := l.boxMin.z + r2*(l.boxMax.z - l.boxMin.z);
    return res;
  end Sample;

  function AreaPDF(l : SphereLight) return float is
  begin
    return 1.0/l.surfaceArea;
  end AreaPDF;

  function GetIntensity(l : SphereLight) return float3 is
  begin
    return l.intensity;
  end GetIntensity;

end Lights;


