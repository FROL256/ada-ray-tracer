with Interfaces;
with Ada.Numerics.Float_Random;
with Vector_Math;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

use Interfaces;
use Vector_Math;
use Ada.Text_IO;

package Materials is

  type LegacyMaterial is record
    ka : float3 := (0.0, 0.0, 0.0);
    kd : float3 := (0.0, 0.0, 0.0);
    ks : float3 := (0.0, 0.0, 0.0);
    roughness    : float  := 0.5;             -- for Cook-Torrance model
    reflection   : float3 := (0.0, 0.0, 0.0); -- use separate (from ks) reflection
    transparency : float3 := (0.0, 0.0, 0.0);
    ior          : float  := 1.0;
    fresnel      : Boolean := false;
  end record;


  function EvalCookTorranceBRDF(mat: in LegacyMaterial; l,v,normal: float3) return float3;

  function TotalInternalReflection(ior: in float; rayDir, normal : float3) return boolean;
  procedure refract(ior: in float; rayDir, normal : in float3; wt : out float3);

  function GetPlaneTextureColor (x, y : float) return float3;

  -----------------------------------------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------------------
  type MaterialRef is access all LegacyMaterial;
  procedure delete is new Ada.Unchecked_Deallocation(Object => LegacyMaterial, Name => MaterialRef);

  procedure ApplyFresnel(mat: in MaterialRef; cosTheta : float; ks : in out float3; kt : in out float3);

  function IsLight(mat : MaterialRef) return Boolean;
  function Emittance(mat : MaterialRef) return float3;


end Materials;

