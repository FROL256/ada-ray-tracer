with Interfaces;
with Ada.Numerics.Float_Random;
with Vector_Math;
--with Ray_Tracer.Intersections;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

use Interfaces;
use Vector_Math;
--use Ray_Tracer.Intersections;
use Ada.Text_IO;

package Lights is

  type LightShapes is (Light_Shape_Rect, Light_Shape_Sphere);

  type LightSample is record
    pos       : float3 := (0.0, 0.0, 0.0);
    norm      : float3 := (0.0, 0.0, 0.0);
    intensity : float3 := (0.0, 0.0, 0.0);
    pdf       : float  := 1.0;
  end record;

  type Light is abstract tagged null record;
  type LightRef is access Light'Class;

  function Sample(l : Light; gen : RandRef; lluminatingPoint : float3) return LightSample is abstract;
  function EvalPDF(l : Light; lluminatingPoint : float3; rayDir : float3; hitDist : float) return float is abstract;
  function GetIntensity(l : Light) return float3 is abstract;
  function GetShapeType(l : Light) return LightShapes is abstract; -- this is for geom intersection only


  function Sample(l : LightRef; gen : RandRef; lluminatingPoint : float3) return LightSample;
  function EvalPDF(l : LightRef; lluminatingPoint : float3; rayDir : float3; hitDist : float) return float;
  function GetIntensity(l : LightRef) return float3;
  function GetShapeType(l : LightRef) return LightShapes;

  type AreaLight is new Light with record
    boxMin : float3;
    boxMax : float3;
    normal : float3;
    intensity   : float3;
    surfaceArea : float;
  end record;

  function AreaPDF(l : AreaLight) return float;
  function Sample(l : AreaLight; gen : RandRef; lluminatingPoint : float3) return LightSample;
  function EvalPDF(l : AreaLight; lluminatingPoint : float3; rayDir : float3; hitDist : float) return float;
  function GetIntensity(l : AreaLight) return float3;
  function GetShapeType(l : AreaLight) return LightShapes;

  type SphereLight is new Light with record
    center : float3;
    radius : float;
    intensity   : float3;
    surfaceArea : float;
  end record;

  function AreaPDF(l : SphereLight) return float;
  function Sample(l : SphereLight; gen : RandRef; lluminatingPoint : float3) return LightSample;
  function EvalPDF(l : SphereLight; lluminatingPoint : float3; rayDir : float3; hitDist : float) return float;
  function GetIntensity(l : SphereLight) return float3;
  function GetShapeType(l : SphereLight) return LightShapes;

end Lights;

