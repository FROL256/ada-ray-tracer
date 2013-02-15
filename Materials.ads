with Interfaces;
with Ada.Numerics.Float_Random;
with Vector_Math;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

use Interfaces;
use Vector_Math;
use Ada.Text_IO;

package Materials is

  type Material is tagged record
    ka : float3 := (0.0, 0.0, 0.0);
    kd : float3 := (0.0, 0.0, 0.0);
    ks : float3 := (0.0, 0.0, 0.0);
    roughness : float := 0.5; -- for Cook-Torrance model
    reflection : float3 := (0.0, 0.0, 0.0); -- use separate (from ks) reflection
    --fresnel_reflection : boolean;

    transparency : float3 := (0.0, 0.0, 0.0);
    ior : float := 1.0;
  end record;


  function EvalCookTorranceBRDF(mat: in Material; l,v,normal: float3) return float3;
  function AdjustShadedColor(mat : in Material; color: float3; x,y : float) return float3;

  function TotalInternalReflection(mat: in Material; rayDir, normal : float3) return boolean;
  procedure refract(mat: in Material; rayDir, normal : in float3; color, wt : out float3);

  type MaterialRef is access all Material'Class;
  procedure delete is new Ada.Unchecked_Deallocation(Object => Material'Class, Name => MaterialRef);


  type MaterialWithMultiplyedTex is new Material with null record;

  function GetPlaneTextureColor (x, y : float) return float3;
  function AdjustShadedColor(mat : in MaterialWithMultiplyedTex; color: float3; x,y : float) return float3;

  --type SimpleRefractiveMaterial is new Material with null record;

  type DielectricMaterial is new Material with record
     eta_in  : float := 1.5;
     eta_out : float := 1.0;
     fog_attenuation : float3 := (1.0, 1.0, 1.0);
  end record;

  procedure SetIndexOfRefraction(mat : in out DielectricMaterial; eta_in, eta_out : float);


end Materials;
