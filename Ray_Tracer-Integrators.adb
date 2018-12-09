with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Materials;
with Lights;
with Ada.Exceptions;
with Ray_Tracer;

use Ada.Numerics;
use Ada.Text_IO;
use Materials;
use Lights;
use Ray_Tracer;

package body Ray_Tracer.Integrators is

  package Float_Functions is new Generic_Elementary_Functions (float);
  use Float_Functions;


  ---- basic path tracing evaluation
  --
  procedure DoPass(self : in out Integrator; colBuff : AccumBuffRef) is
    r       : Ray;
    rayDirs : RayDirPack;
    color   : float3;
  begin

      for y in 0 .. height - 1 loop
        for x in 0 .. width - 1 loop

          r.x      := x; r.y := y;
          r.origin := g_cam.pos;

          if anti_aliasing_on then

            color := background_color;
            Generate4RayDirections(x,y,rayDirs);

            for i in 0 .. 3 loop
              r.direction := normalize(g_cam.matrix*rayDirs(i));
              color := color + PathTrace(Integrator'Class(self), r, StartSample, g_max_depth);
            end loop;

            colBuff(x,y) := color*0.25;

          else

            r.x := x; r.y := y;
            r.direction  := EyeRayDirection(x,y);
            r.direction  := normalize(g_cam.matrix*r.direction);
            colBuff(x,y) := PathTrace(Integrator'Class(self), r, StartSample, g_max_depth);

          end if;

        end loop;
    end loop;

  end DoPass;


  -- very basic path tracing
  --

  procedure Init(self : in out SimplePathTracer) is
  begin
    null;
  end Init;

  function PathTrace(self : SimplePathTracer; r : Ray; prevSample : MatSample; recursion_level : Integer) return float3 is
    bxdfVal : float3;
    h       : Hit;
    matSam  : MatSample;
    nextRay : Ray := r;
    --rayDeep : integer := g_max_depth - recursion_level;
    --pabsorb : float   := 1.0 - 1.0/(float(rayDeep)*0.25 + 1.0); -- for russian roulette
  begin

    if recursion_level = 0 then
      return (0.0, 0.0, 0.0);
    end if;

    h := FindClosestHit(r);

    if not h.is_hit then
      return (0.0, 0.0, 0.0);
    end if;

    if IsLight(h.mat) then
      if dot((-1.0)*r.direction, h.normal) < 0.0 then
        return (0.0, 0.0, 0.0);
      else
        return Emittance(h.mat);
      end if;
    end if;

    -- russian roulette
    --
    --if self.gen.rnd_uniform(0.0, 1.0) < pabsorb then
    --  return (0.0, 0.0, 0.0);
    --end if;

    matSam  := SampleAndEvalBxDF(h.mat, self.gen, r.direction, h.normal, tx => h.tx, ty => h.ty);
    bxdfVal := matSam.color * (1.0/max(matSam.pdf, epsilonDiv));

    nextRay.origin    := r.origin + r.direction*h.t;
    nextRay.direction := matSam.direction;
    nextRay.origin    := nextRay.origin + sign(dot(nextRay.direction, h.normal))*h.normal*epsilon; -- add small offset to ray position

    return bxdfVal*self.PathTrace(nextRay, matSam, recursion_level-1); --*(1.0/(1.0-pabsorb));

  end PathTrace;


  -- path tracing with shadow rays
  --
  procedure Init(self : in out PathTracerWithShadowRays) is
  begin
    null;
  end Init;

  function PathTrace(self : PathTracerWithShadowRays; r : Ray; prevSample : MatSample; recursion_level : Integer) return float3 is
    explicitColor : float3 := (0.0, 0.0, 0.0);
    bxdfVal : float3;
    h : Hit;
    nextRay : Ray := r;
    matSam  : MatSample;
  begin

    if recursion_level = 0 then
      return (0.0, 0.0, 0.0);
    end if;

    h := FindClosestHit(r);

    if not h.is_hit then
      return (0.0, 0.0, 0.0);
    end if;

    if IsLight(h.mat) then
      return (0.0, 0.0, 0.0);
    end if;

    -- explicit sampling
    --
    declare

      hpos    : float3      := (r.origin + r.direction*h.t);      
      lsam    : ShadowSample := Sample(g_lightRef, self.gen, hpos);
      sdir    : float3      := normalize(lsam.pos - hpos);

      bxdfVal : float3      := EvalBxDF(h.mat, l => sdir, v => (-1.0)*r.direction, n => h.normal, tx => h.tx, ty => h.ty);

    begin

      if not ComputeShadow(hpos, lsam.pos).in_shadow then
        explicitColor := lsam.intensity*bxdfVal*(1.0/max(lsam.pdf, epsilonDiv));
      end if;

    end;


    -- pick up next ray
    --
    matSam  := SampleAndEvalBxDF(h.mat, self.gen, r.direction, h.normal, tx => h.tx, ty => h.ty);
    bxdfVal := matSam.color * (1.0/max(matSam.pdf, epsilonDiv));

    nextRay.origin    := r.origin + r.direction*h.t;
    nextRay.direction := matSam.direction;
    nextRay.origin    := nextRay.origin + sign(dot(nextRay.direction, h.normal))*h.normal*epsilon; -- add small offset to ray position

    return explicitColor + bxdfVal*self.PathTrace(nextRay, matSam, recursion_level-1);

  end PathTrace;


  -- path tracing with Multiple Importance Sampling (MIS)
  --
  procedure Init(self : in out PathTracerMIS) is
  begin
    null;
  end Init;

  function PathTrace(self : PathTracerMIS; r : Ray; prevSample : MatSample; recursion_level : Integer) return float3 is
    explicitColor : float3 := (0.0, 0.0, 0.0);
    bxdfVal : float3;
    h       : Hit;
    nextRay : Ray := r;
    matSam  : MatSample;
  begin

    if recursion_level = 0 then
      return (0.0, 0.0, 0.0);
    end if;

    h := FindClosestHit(r);

    if not h.is_hit then
      return (0.0, 0.0, 0.0);
    end if;

    if IsLight(h.mat) then

      if dot((-1.0)*r.direction, h.normal) < 0.0 then
        return (0.0, 0.0, 0.0);
      else

        -- calculatimg MIS weight when hit light
        --
        declare
           lgtPdf    : float  := EvalPDF(GetLightRef(h.mat), r.origin, r.direction, h.t);
           bsdfPdf   : float  := prevSample.pdf;
           misWeight : float;
        begin

          if prevSample.pureSpecular then
            misWeight := 1.0;
          else
            misWeight := bsdfPdf*bsdfPdf/(lgtPdf*lgtPdf + bsdfPdf*bsdfPdf);
          end if;

          return Emittance(h.mat)*misWeight;
        end;

      end if;

    end if;

    -- explicit sampling
    --
    declare

      hpos  : float3      := (r.origin + r.direction*h.t);
      lsam  : ShadowSample := Sample(g_lightRef, self.gen, hpos);

      sdir    : float3 := normalize(lsam.pos - hpos);
      lgtPdf  : float  := lsam.pdf;
      bsdfPdf : float  := EvalPDF (h.mat, l => sdir, v => (-1.0)*r.direction, n => h.normal, tx => h.tx, ty => h.ty);
      bxdfVal : float3 := EvalBxDF(h.mat, l => sdir, v => (-1.0)*r.direction, n => h.normal, tx => h.tx, ty => h.ty);

      misWeight : float := lgtPdf*lgtPdf/(lgtPdf*lgtPdf + bsdfPdf*bsdfPdf);

    begin

      if not ComputeShadow(hpos, lsam.pos).in_shadow then
        explicitColor := lsam.intensity*bxdfVal*(1.0/max(lgtPdf, epsilonDiv))*misWeight;
      end if;

    end;

    -- pick up next ray
    --
    matSam  := SampleAndEvalBxDF(h.mat, self.gen, r.direction, h.normal, tx => h.tx, ty => h.ty);
    bxdfVal := matSam.color * (1.0/max(matSam.pdf, epsilonDiv));

    nextRay.origin    := r.origin + r.direction*h.t;
    nextRay.direction := matSam.direction;
    nextRay.origin    := nextRay.origin + sign(dot(nextRay.direction, h.normal))*h.normal*epsilon; -- add small offset to ray position

    return explicitColor + bxdfVal*self.PathTrace(nextRay, matSam, recursion_level-1);


  end PathTrace;


  


end Ray_Tracer.Integrators;
