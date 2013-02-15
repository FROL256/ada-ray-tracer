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
  function EvalCookTorranceBRDF(mat: in Material; l,v,normal: float3) return float3 is

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

   --function GetReflection(mat : in Material; n,r : float3) return float3 is
     --x,y,z,k : float;
   --begin
     --if mat.fresnel_reflection then
     --  k := 1.5*FresnelReflectionCoeff(1.0, 2.5, dot(r,n));
     --  x := min(mat.reflection.x, k);
     --  y := min(mat.reflection.y, k);
     --  z := min(mat.reflection.z, k);
     --  return (x,y,z);
     --else
  --     return mat.reflection;
     --end if;
  -- end;


   function TotalInternalReflection(mat: in Material; rayDir, normal : float3) return boolean is
     cos_thetai : float;
     eta : float;
   begin

     cos_thetai := dot((-1.0)*rayDir, normal);
     eta        := mat.ior;

     if cos_thetai < 0.0 then
       eta := 1.0/eta;
     end if;

     return (1.0 - (1.0 - cos_thetai*cos_thetai)/(eta*eta)) < 0.0;

   end TotalInternalReflection;

   procedure refract(mat: in Material; rayDir, normal : in float3; color, wt : out float3) is
     cos_thetai, cos_theta2, eta : float;
     n  : float3 := normal;
     wo : float3 := (-1.0)*rayDir;
   begin

     cos_thetai := dot((-1.0)*rayDir, normal);
     eta        := mat.ior;

     if cos_thetai < 0.0 then
       eta := 1.0/eta;
       cos_thetai := -cos_thetai;
       n := (-1.0)*n;
     end if;

     cos_theta2 := sqrt(1.0 - (1.0 - cos_thetai*cos_thetai)/(eta*eta) );
     wt := normalize( (-1.0)*wo*(1.0/eta) - (cos_theta2 - cos_thetai/eta)*n );
     color := mat.transparency*( 1.0/(eta*eta)) * (1.0/abs(dot(normal, wt)));

   end refract;

   function AdjustShadedColor(mat : in Material; color: float3; x,y : float) return float3 is
   begin
     return color;
   end;

   function AdjustShadedColor(mat : in MaterialWithMultiplyedTex; color: float3; x,y : float) return float3 is
   begin
     return color*GetPlaneTextureColor(x,y);
   end;


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


   function FresnelReflectionCoeff(n1,n2,n_dot_dir : float) return float;
   function FresnelReflectionCoeff(n1,n2,n_dot_dir : float) return float is
     e1,c1,e2 : float;
   begin

    if abs(n2-n1) < 0.001 then
      return 0.0;
    end if;

    e1 := arccos(abs(n_dot_dir));

    if e1 < 0.01 then
      return sqr((n1-n2)/(n1+n2));
    end if;

    c1 := (sin(e1)*n1)/n2;
    if c1 < 1.0 then
      e2 := arcsin(c1);
      return 0.5*( sqr(sin(e1-e2))/sqr(sin(e1+e2)) + sqr(safe_tan(e1-e2))/sqr(safe_tan(e1+e2)));
    else
      return 1.0;
    end if;

   end FresnelReflectionCoeff;

   function FresnelReflectionCoeff2(eta_in,eta_out,n_dot_dir : float) return float;
   function FresnelReflectionCoeff2(eta_in, eta_out, n_dot_dir : float) return float is
      eta,cos_theta_i,cos_theta_t, temp : float;
      r_parallel,r_perpendicular  : float;
   begin

     --Put_Line("Here");
     --Put_Line(float'Image(n_dot_dir));

     if n_dot_dir < 0.0 then
       eta := eta_out/eta_in;
       cos_theta_i := n_dot_dir*(-1.0);
     else
       eta := eta_in/eta_out;
       cos_theta_i := n_dot_dir;
     end if;

     --Put_Line("Here 2");
     --Put_Line(float'Image( 1.0 - (1.0 - sqr(cos_theta_i))/sqr(eta) ));

     temp := 1.0 - (1.0 - sqr(cos_theta_i))/sqr(eta);
     if temp < 0.0 then
       return 1.0; -- total internal reflection ?
     end if;

     cos_theta_t := sqrt(temp);
     r_parallel      := (eta*cos_theta_i - cos_theta_t)/(eta*cos_theta_i + cos_theta_t);
     r_perpendicular := (cos_theta_i - eta*cos_theta_i)/(cos_theta_i + eta*cos_theta_i);

     --Put_Line("Here 3");

     return 0.5*(sqr(r_parallel) + sqr(r_perpendicular));

   end FresnelReflectionCoeff2;

   --function GetReflection(mat : in DielectricMaterial; n,r : float3) return float3 is
   --  x,y,z,k : float;
   --begin
   --  k := FresnelReflectionCoeff2(mat.eta_in, mat.eta_out, dot(n,r));
   --  x := max(mat.reflection.x, k);
   --  y := max(mat.reflection.y, k);
   --  z := max(mat.reflection.z, k);
   --  return (x,y,z);
     --return mat.reflection;
   --end GetReflection;

   procedure SetIndexOfRefraction(mat : in out DielectricMaterial; eta_in, eta_out : float) is
   begin
     mat.ior     := eta_in/eta_out;
     mat.eta_in  := eta_in;
     mat.eta_out := eta_out;
   end SetIndexOfRefraction;

end Materials;


