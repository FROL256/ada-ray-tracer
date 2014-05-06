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
  type MaterialLegacyRef is access all LegacyMaterial;
  procedure delete is new Ada.Unchecked_Deallocation(Object => LegacyMaterial, Name => MaterialLegacyRef);

  procedure ApplyFresnel(mat: in MaterialLegacyRef; cosTheta : float; ks : in out float3; kt : in out float3);

  function IsLight(mat : MaterialLegacyRef) return Boolean;
  function Emittance(mat : MaterialLegacyRef) return float3;


  -----------------------------------------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------------------



  -----------------------------
  ---- Moderm Material API ----
  -----------------------------
  type MatSample is record -- sample materials with Monte-Carlo
    color        : float3  := (0.0, 0.0, 0.0);
    direction    : float3  := (0.0, 0.0, 0.0);
    cosTheta     : float   := 0.0;
    pdf          : float   := 1.0;
    pureSpecular : boolean := false;
  end record;

  ----------------------------
  ---- Base Material Type ----
  ----------------------------
  type Material is abstract tagged null record;


  function IsLight(mat : Material) return Boolean is abstract;                     				    -- indicate the materias is light
  function Emittance(mat : Material) return float3 is abstract;                    				    -- get light intensity
  function SampleAndEvalBxDF(mat : Material; gen : RandRef; ray_dir, normal : float3) return MatSample is abstract; -- simultaniously create brdf/btdf sample and eval brdf/btdf value
  function EvalBxDF(mat : Material; l,v,n : float3) return float3 is abstract;     				    -- eval brdf/btdf value for direct light sampling


  -----------------------------------------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------------------

  type MaterialRef is access all Material;
  --procedure delete is new Ada.Unchecked_Deallocation(Object => Material, Name => MaterialRef);

  ------------------------------------
  ---- Simple Area Light Material ----
  ------------------------------------
  type MaterialAreaLight is new Material with record
    emission : float3;
  end record;


  function IsLight(mat : MaterialAreaLight) return Boolean;
  function Emittance(mat : MaterialAreaLight) return float3;
  function SampleAndEvalBxDF(mat : MaterialAreaLight; gen : RandRef; ray_dir, normal : float3) return MatSample;
  function EvalBxDF(mat : MaterialAreaLight; l,v,n : float3) return float3;

  type MaterialAreaLightRef is access MaterialAreaLight;

  --------------------------
  ---- Diffuse material ----
  --------------------------
  type MaterialLambert is new Material with record
    kd : float3;
  end record;

  function IsLight(mat : MaterialLambert) return Boolean;
  function Emittance(mat : MaterialLambert) return float3;
  function SampleAndEvalBxDF(mat : MaterialLambert; gen : RandRef; ray_dir, normal : float3) return MatSample;
  function EvalBxDF(mat : MaterialLambert; l,v,n : float3) return float3;

  type MaterialLambertRef is access MaterialLambert;

  ----------------------
  ---- Ideal Mirror ----
  ----------------------
  type MaterialMirror is new Material with record
    reflection : float3;
  end record;

  function IsLight(mat : MaterialMirror) return Boolean;
  function Emittance(mat : MaterialMirror) return float3;
  function SampleAndEvalBxDF(mat : MaterialMirror; gen : RandRef; ray_dir, normal : float3) return MatSample;
  function EvalBxDF(mat : MaterialMirror; l,v,n : float3) return float3;

  type MaterialMirrorRef is access MaterialMirror;

  -----------------------------
  ---- Ideal Fresnel Glass ----
  -----------------------------
  type MaterialFresnelDielectric is new Material with record
    reflection   : float3;
    transparency : float3;
    ior          : float;
  end record;

  function IsLight(mat : MaterialFresnelDielectric) return Boolean;
  function Emittance(mat : MaterialFresnelDielectric) return float3;
  function SampleAndEvalBxDF(mat : MaterialFresnelDielectric; gen : RandRef; ray_dir, normal : float3) return MatSample;
  function EvalBxDF(mat : MaterialFresnelDielectric; l,v,n : float3) return float3;

  procedure ApplyFresnel(mat: in MaterialFresnelDielectric; cosTheta : float; ks : in out float3; kt : in out float3);

  type MaterialFresnelDielectricRef is access MaterialFresnelDielectric;

  -----------------------------
  ---- 'Fixed' Phong Model ----
  -----------------------------
  type MaterialPhong is new Material with record
    reflection : float3;
    cosPower   : float;
  end record;

  function IsLight(mat : MaterialPhong) return Boolean;
  function Emittance(mat : MaterialPhong) return float3;
  function SampleAndEvalBxDF(mat : MaterialPhong; gen : RandRef; ray_dir, normal : float3) return MatSample;
  function EvalBxDF(mat : MaterialPhong; l,v,n : float3) return float3;

  type MaterialPhongRef is access MaterialPhong;

end Materials;

