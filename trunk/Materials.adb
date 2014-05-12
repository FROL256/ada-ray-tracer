with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Vector_Math;

use Interfaces;
use Vector_Math;


package body Materials is

  package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(float);
  use Float_Functions;


   function TotalInternalReflection(ior: in float; rayDir, normal : float3) return boolean is
     cos_thetai : float;
     eta : float;
   begin

     cos_thetai := dot((-1.0)*rayDir, normal);
     eta        := ior;

     if cos_thetai < 0.0 then
       eta := 1.0/eta;
     end if;

     return (1.0 - (1.0 - cos_thetai*cos_thetai)/(eta*eta)) < 0.0;

   end TotalInternalReflection;

   procedure refract(ior: float; rayDir, normal : in float3; wt : out float3) is
     cos_thetai, cos_theta2, eta : float;
     n  : float3 := normal;
     wo : float3 := (-1.0)*rayDir;
   begin

     cos_thetai := dot((-1.0)*rayDir, normal);
     eta        := ior;

     if cos_thetai < 0.0 then
       eta := 1.0/eta;
       cos_thetai := -cos_thetai;
       n := (-1.0)*n;
     end if;

     cos_theta2 := sqrt(1.0 - (1.0 - cos_thetai*cos_thetai)/(eta*eta) );
     wt := normalize( (-1.0)*wo*(1.0/eta) - (cos_theta2 - cos_thetai/eta)*n );

   end refract;


   function GetPlaneTextureColor (x, y : float) return float3 is
     ix : Integer := abs(Integer(0.25*x));
     iy : Integer := abs(Integer(0.25*y));
   begin

     if ((ix rem 2) = 0 and (iy rem 2) = 0) or
        ((ix rem 2) = 1 and (iy rem 2) = 1) then
       return (1.0, 1.0, 1.0);
     else
       return (0.0, 0.0, 0.0);
     end if;

   end GetPlaneTextureColor;


  function fresnelDielectric(cosTheta1,cosTheta2,etaExt,etaInt : float) return float is
    Rs,Rp : float;
  begin
    Rs := (etaExt * cosTheta1 - etaInt * cosTheta2) / (etaExt * cosTheta1 + etaInt * cosTheta2);
    Rp := (etaInt * cosTheta1 - etaExt * cosTheta2) / (etaInt * cosTheta1 + etaExt * cosTheta2);
    return (Rs * Rs + Rp * Rp) / 2.0;
  end fresnelDielectric;


  function fresnel(cosTheta1 : float; a_etaExt, a_etaInt : float) return float is
    tmp, sinTheta2, cosTheta2 : float;
    etaExt : float := a_etaExt;
    etaInt : float := a_etaInt;
  begin

    if cosTheta1 < 0.0 then
      tmp := etaExt; etaExt := etaInt; etaInt := tmp; -- swap(etaExt, etaInt)
    end if;

    sinTheta2 := (etaExt/etaInt)*sqrt(max(0.0,1.0 - cosTheta1*cosTheta1));

    if sinTheta2 > 1.0 then
     return 1.0;
    end if;

    cosTheta2 := sqrt(max(0.0, 1.0 - sinTheta2*sinTheta2));

    return fresnelDielectric(abs(cosTheta1), cosTheta2, etaInt, etaExt);

  end fresnel;

  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  -- simplify dispatching call syntax
  --
  function IsLight(mat : MaterialRef) return Boolean is
  begin
    return IsLight(mat.all);
  end IsLight;

  function Emittance(mat : MaterialRef) return float3 is
  begin
    return Emittance(mat.all);
  end Emittance;


  function SampleAndEvalBxDF(mat : MaterialRef; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample is
  begin
    return SampleAndEvalBxDF(mat.all, gen, ray_dir, normal, tx, ty);
  end  SampleAndEvalBxDF;

  function EvalBxDF(mat : MaterialRef; l,v,n : float3; tx,ty : float) return float3 is
  begin
    return EvalBxDF(mat.all, l, v, n, tx, ty);
  end EvalBxDF;

  function EvalPDF(mat : MaterialRef; l,v,n : float3; tx,ty : float) return float is
  begin
    return EvalPDF(mat.all, l, v, n, tx, ty);
  end EvalPDF;


  ----------------------------------
  ---- Simple Emissive Material ----
  ----------------------------------

  function IsLight(mat : MaterialAreaLight) return Boolean is
  begin
    return true;
  end IsLight;

  function Emittance(mat : MaterialAreaLight) return float3 is
  begin
    return mat.emission;
  end Emittance;

  function SampleAndEvalBxDF(mat : MaterialAreaLight; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample is
  begin
    return ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), 1.0, false);
  end SampleAndEvalBxDF;

  function EvalBxDF(mat : MaterialAreaLight; l,v,n : float3; tx,ty : float) return float3 is
  begin
    return (0.0, 0.0, 0.0);
  end EvalBxDF;

  function EvalPDF(mat : MaterialAreaLight; l,v,n : float3; tx,ty : float) return float is
  begin
    return 1.0;
  end EvalPDF;

  --------------------------
  ---- Diffuse Material ----
  --------------------------

  function IsLight(mat : MaterialLambert) return Boolean is
  begin
    return false;
  end IsLight;

  function Emittance(mat : MaterialLambert) return float3 is
  begin
    return (0.0, 0.0, 0.0);
  end Emittance;

  function SampleAndEvalBxDF(mat : MaterialLambert; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample is
    pdf      : float;
    cosTheta : float;
    newDir   : float3;
    color    : float3;
  begin

    newDir   := RandomCosineVectorOf(gen, normal);
    cosTheta := max(dot(newDir, normal), 0.0);
    pdf      := cosTheta*INV_PI;
    color    := mat.kd*cosTheta*INV_PI;

    return (color, newDir, pdf, false);

  end SampleAndEvalBxDF;

  function EvalBxDF(mat : MaterialLambert; l,v,n : float3; tx,ty : float) return float3 is
    cosTheta : float := max(dot(n,l), 0.0);
  begin
    return mat.kd*cosTheta*INV_PI;
  end EvalBxDF;

  function EvalPDF(mat : MaterialLambert; l,v,n : float3; tx,ty : float) return float is
    cosTheta : float := max(dot(n,l), 0.0);
  begin
    return cosTheta*INV_PI;
  end EvalPDF;

  ----------------------
  ---- Ideal Mirror ----
  ----------------------

  function IsLight(mat : MaterialMirror) return Boolean is
  begin
    return false;
  end IsLight;

  function Emittance(mat : MaterialMirror) return float3 is
  begin
    return (0.0, 0.0, 0.0);
  end Emittance;

  function SampleAndEvalBxDF(mat : MaterialMirror; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample is
  begin
    return (mat.reflection, reflect(ray_dir, normal), 1.0, true);
  end SampleAndEvalBxDF;

  function EvalBxDF(mat : MaterialMirror; l,v,n : float3; tx,ty : float) return float3 is
  begin
    return (0.0, 0.0, 0.0); -- no direct sampling for perfect mirrors
  end EvalBxDF;

  function EvalPDF(mat : MaterialMirror; l,v,n : float3; tx,ty : float) return float is
  begin
    return 1.0;             -- no direct sampling for perfect mirrors
  end EvalPDF;

  -----------------------------
  ---- Ideal Fresnel Glass ----
  -----------------------------

  function IsLight(mat : MaterialFresnelDielectric) return Boolean is
  begin
    return false;
  end IsLight;

  function Emittance(mat : MaterialFresnelDielectric) return float3 is
  begin
    return (0.0, 0.0, 0.0);
  end Emittance;


  procedure ApplyFresnel(mat: in MaterialFresnelDielectric; cosTheta : float; ks : in out float3; kt : in out float3) is
    etaInt : float := 1.0;
    etaExt : float := mat.ior;
    f      : float;
  begin
    f  := fresnel(cosTheta, etaExt, etaInt);
    ks := f*ks;
    kt := (1.0-f)*kt;
  end ApplyFresnel;

  function SampleAndEvalBxDF(mat : MaterialFresnelDielectric; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample is
    refl, trans : float3;
    ksitrans, ksirefl, ksi : float;
    nextDirection, bxdf : float3;
  begin

    refl  := mat.reflection;
    trans := mat.transparency;

    ApplyFresnel(mat, dot(ray_dir, normal), refl, trans);

    ksitrans := length(trans) / (length(refl) + length(trans));
    ksirefl  := length(refl) / (length(refl) + length(trans));

    ksi := gen.rnd_uniform(0.0, 1.0);

    if ksi > ksitrans then
      nextDirection := reflect(ray_dir, normal);
      bxdf          := refl*(1.0/ksirefl);
    else

      bxdf := trans*(1.0/ksitrans);

      if not TotalInternalReflection(mat.ior, ray_dir, normal) then
    	refract(mat.ior, ray_dir, normal, wt => nextDirection);
      else
        nextDirection := reflect(ray_dir, normal);
      end if;

    end if;

    return (bxdf, nextDirection, 1.0, true);

  end SampleAndEvalBxDF;

  function EvalBxDF(mat : MaterialFresnelDielectric; l,v,n : float3; tx,ty : float) return float3 is
  begin
    return (0.0, 0.0, 0.0); -- no direct sampling for perfect mirrors
  end EvalBxDF;

  function EvalPDF(mat : MaterialFresnelDielectric; l,v,n : float3; tx,ty : float) return float is
  begin
    return 1.0;             -- no direct sampling for perfect mirrors
  end EvalPDF;


  -----------------------------
  ---- 'Fixed' Phong Model ----
  -----------------------------

  function IsLight(mat : MaterialPhong) return Boolean is
  begin
    return false;
  end IsLight;

  function Emittance(mat : MaterialPhong) return float3 is
  begin
    return (0.0, 0.0, 0.0);
  end Emittance;

  function SampleAndEvalBxDF(mat : MaterialPhong; gen : RandRef; ray_dir, normal : float3; tx,ty : float) return MatSample is
    pdf, cosTheta : float;
    nextDir, r    : float3;
    color         : float3;
  begin

    r        := reflect(ray_dir, normal);
    nextDir  := RandomCosineVectorOf(gen, r, normal, mat.cosPower);

    cosTheta := clamp(dot(nextDir, r), 0.0, M_PI*0.499995);
    color    := mat.reflection*(mat.cosPower + 2.0)*0.5*INV_PI*pow(cosTheta, mat.cosPower);
    pdf      := pow(cosTheta, mat.cosPower) * (mat.cosPower + 1.0) * (0.5 * INV_PI);

    return (color, nextDir, pdf, false);

  end SampleAndEvalBxDF;

  function EvalBxDF(mat : MaterialPhong; l,v,n : float3; tx,ty : float) return float3 is
    r        : float3;
    cosTheta : float;
  begin
    r        := reflect((-1.0)*v, n);
    cosTheta := clamp(dot(l, r), 0.0, M_PI*0.499995);
    return mat.reflection*(mat.cosPower + 2.0)*0.5*INV_PI*pow(cosTheta, mat.cosPower);
  end EvalBxDF;


  function EvalPDF(mat : MaterialPhong; l,v,n : float3; tx,ty : float) return float is
    r        : float3;
    cosTheta : float;
    pdf      : float;
  begin
    r        := reflect((-1.0)*v, n);
    cosTheta := clamp(dot(l, r), 0.0, M_PI*0.499995);
    pdf      := pow(cosTheta, mat.cosPower) * (mat.cosPower + 1.0) * (0.5 * INV_PI);
    return pdf;
  end EvalPDF;



end Materials;


