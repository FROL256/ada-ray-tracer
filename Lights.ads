with Interfaces;
with Ada.Numerics.Float_Random;
with Vector_Math;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

use Interfaces;
use Vector_Math;
use Ada.Text_IO;

package Lights is

  type LightSample is record
    pos       : float3 := (0.0, 0.0, 0.0);
    norm      : float3 := (0.0, 0.0, 0.0);
    intensity : float3 := (0.0, 0.0, 0.0);
    apdf      : float  := 1.0;
  end record;

  type Light is abstract tagged null record;
  type LightRef is access Light'Class;

  function Sample(l : Light; gen : RandRef)  return LightSample is abstract;
  function AreaPDF(l : Light) return float is abstract;
  function GetIntensity(l : Light) return float3 is abstract;


  function Sample(l : LightRef; gen : RandRef)  return LightSample;
  function AreaPDF(l : LightRef) return float;
  function GetIntensity(l : LightRef) return float3;

  type AreaLight is new Light with record
    boxMin : float3;
    boxMax : float3;
    normal : float3;
    intensity   : float3;
    surfaceArea : float;
  end record;

  function Sample(l : AreaLight; gen : RandRef)  return LightSample;
  function AreaPDF(l : AreaLight) return float;
  function GetIntensity(l : AreaLight) return float3;


  type SphereLight is new Light with record
    center : float3;
    radius : float;
    intensity   : float3;
    surfaceArea : float;
  end record;

  function Sample(l : SphereLight; gen : RandRef)  return LightSample;
  function AreaPDF(l : SphereLight) return float;
  function GetIntensity(l : SphereLight) return float3;

end Lights;

