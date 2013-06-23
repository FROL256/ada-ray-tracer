with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Vector_Math;

use Interfaces;
use Vector_Math;


package body Materials is

  package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(float);
  use Float_Functions;

  -- Cook-Torrance model
  --
  function EvalCookTorranceBRDF(mat: in LegacyMaterial; l,v,normal: float3) return float3 is

    vHalf : float3;
    NormalDotHalf : float;
    ViewDotHalf : float;
    NormalDotView : float;
    NormalDotLight : float;
    G1,G2,G : float;
    F,A,B,R : float;
    NDotHSquare,RSquare : float;

  begin

    vHalf            := normalize(l+v);
    NormalDotHalf    := dot(normal, vHalf);
    ViewDotHalf      := dot(vHalf, v);
    NormalDotView    := dot(normal, v);
    NormalDotLight   := dot(normal, l);

    -- Compute the geometric term
    --
    G1 := ( 2.0 * NormalDotHalf * NormalDotView ) / ViewDotHalf;
    G2 := ( 2.0 * NormalDotHalf * NormalDotLight ) / ViewDotHalf;
    G  := min( 1.0, max( 0.0, min( G1, G2 ) ) );

    -- Compute the fresnel term
    --
    F := 1.0/(1.0 + dot(v,normal));

    RSquare     := mat.roughness * mat.roughness;
    NDotHSquare := NormalDotHalf * NormalDotHalf;
    A           := 1.0 / ( 4.0 * RSquare * NDotHSquare * NDotHSquare );
    B           := exp( -( 1.0 - NDotHSquare ) / ( RSquare * NDotHSquare ) );
    R           := A * B;

    -- Compute the final term
    --
    return mat.kd*max(dot(normal,l),0.0) +
           0.5*mat.ks*( ( G * F * R ) / ( NormalDotLight * NormalDotView ) );

   end EvalCookTorranceBRDF;


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



  procedure ApplyFresnel(mat: in MaterialRef; cosTheta : float; ks : in out float3; kt : in out float3) is
    etaInt : float := 1.0;
    etaExt : float := mat.ior;
    f      : float;
  begin
    f  := fresnel(cosTheta, etaExt, etaInt);
    ks := f*ks;
    kt := (1.0-f)*kt;
  end ApplyFresnel;


  function IsLight(mat : MaterialRef) return Boolean is
  begin
    return length(mat.ka) > 0.0;
  end IsLight;

  function Emittance(mat : MaterialRef) return float3 is
  begin
    return mat.ka;
  end Emittance;

end Materials;


