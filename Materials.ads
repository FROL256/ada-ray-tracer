with Interfaces;
with Ada.Numerics.Float_Random;
with Vector_Math;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Lights;

use Interfaces;
use Vector_Math;
use Ada.Text_IO;
use Lights;

package Materials is

  -----------------------------
  ---- Moderm Material API ----
  -----------------------------
  type MatSample is record -- sample materials with Monte-Carlo
    color        : float3  := (0.0, 0.0, 0.0);
    direction    : float3  := (0.0, 0.0, 0.0);
    pdf          : float   := 1.0;
    pureSpecular : boolean := false;
  end record;

  StartSample : constant MatSample := ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), 1.0, true); -- for rays starting their path from screen of other places

  ----------------------------
  ---- Base Material Type ----
  ----------------------------
  type Material is abstract tagged null record;
  type MaterialRef is access Material'Class;

  function IsLight(mat : Material) return Boolean is abstract;                     				                    -- indicate the materias is light
  function Emittance(mat : Material) return float3 is abstract;                    				                    -- get light intensity
  function GetLightRef(mat : Material) return LightRef is abstract;                                                                 -- if material is light, return associated light reference, else return null
  function SampleAndEvalBxDF(mat : Material; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample is abstract;  -- simultaniously create brdf/btdf sample and eval brdf/btdf value
  function EvalBxDF(mat : Material; l,v,n : float3; tx,ty : float) return float3 is abstract;     				    -- eval brdf/btdf value for direct light sampling
  function EvalPDF(mat : Material; l,v,n : float3; tx,ty : float) return float is abstract;                                         -- eval pdf for MIS with direct light sampling


  -- This will simplify dispatching syntax.
  -- We cas do this any time we use abstact types because
  -- all of their functions are dispatching (virtual)
  --
  function IsLight(mat : MaterialRef) return Boolean;
  function Emittance(mat : MaterialRef) return float3;
  function GetLightRef(mat : MaterialRef) return LightRef;
  function SampleAndEvalBxDF(mat : MaterialRef; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample;
  function EvalBxDF(mat : MaterialRef; l,v,n : float3; tx,ty : float) return float3;
  function EvalPDF(mat : MaterialRef; l,v,n : float3; tx,ty : float) return float;

  -----------------------------------------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------------------

  ------------------------------------
  ---- Simple Area Light Material ----
  ------------------------------------
  type MaterialLight is new Material with record
    lref : LightRef := null;
  end record;


  function IsLight(mat : MaterialLight) return Boolean;
  function Emittance(mat : MaterialLight) return float3;
  function GetLightRef(mat : MaterialLight) return LightRef;
  function SampleAndEvalBxDF(mat : MaterialLight; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample;
  function EvalBxDF(mat : MaterialLight; l,v,n : float3; tx,ty : float) return float3;
  function EvalPDF(mat : MaterialLight; l,v,n : float3; tx,ty : float) return float;

  type MaterialLightRef is access MaterialLight;

  --------------------------
  ---- Diffuse material ----
  --------------------------
  type MaterialLambert is new Material with record
    kd : float3;
  end record;

  function IsLight(mat : MaterialLambert) return Boolean;
  function Emittance(mat : MaterialLambert) return float3;
  function GetLightRef(mat : MaterialLambert) return LightRef;
  function SampleAndEvalBxDF(mat : MaterialLambert; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample;
  function EvalBxDF(mat : MaterialLambert; l,v,n : float3; tx,ty : float) return float3;
  function EvalPDF(mat : MaterialLambert; l,v,n : float3; tx,ty : float) return float;

  type MaterialLambertRef is access MaterialLambert;

  ----------------------
  ---- Ideal Mirror ----
  ----------------------
  type MaterialMirror is new Material with record
    reflection : float3;
  end record;

  function IsLight(mat : MaterialMirror) return Boolean;
  function Emittance(mat : MaterialMirror) return float3;
  function GetLightRef(mat : MaterialMirror) return LightRef;
  function SampleAndEvalBxDF(mat : MaterialMirror; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample;
  function EvalBxDF(mat : MaterialMirror; l,v,n : float3; tx,ty : float) return float3;
  function EvalPDF(mat : MaterialMirror; l,v,n : float3; tx,ty : float) return float;

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
  function GetLightRef(mat : MaterialFresnelDielectric) return LightRef;
  function SampleAndEvalBxDF(mat : MaterialFresnelDielectric; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample;
  function EvalBxDF(mat : MaterialFresnelDielectric; l,v,n : float3; tx,ty : float) return float3;
  function EvalPDF(mat : MaterialFresnelDielectric; l,v,n : float3; tx,ty : float) return float;

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
  function GetLightRef(mat : MaterialPhong) return LightRef;
  function SampleAndEvalBxDF(mat : MaterialPhong; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample;
  function EvalBxDF(mat : MaterialPhong; l,v,n : float3; tx,ty : float) return float3;
  function EvalPDF(mat : MaterialPhong; l,v,n : float3; tx,ty : float) return float;

  type MaterialPhongRef is access MaterialPhong;

end Materials;

