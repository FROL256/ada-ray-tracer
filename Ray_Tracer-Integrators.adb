with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Materials;
with Ada.Exceptions;
with Ray_Tracer;
with Ray_Tracer.Intersections;


use Ada.Numerics;
use Ada.Text_IO;
use Materials;
use Ray_Tracer;
use Ray_Tracer.Intersections;


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
              color := color + PathTrace(Integrator'Class(self), r, max_depth).color;
            end loop;

            colBuff(x,y) := color*0.25;

          else

            r.x := x; r.y := y;
            r.direction  := EyeRayDirection(x,y);
            r.direction  := normalize(g_cam.matrix*r.direction);
            colBuff(x,y) := PathTrace(Integrator'Class(self), r, max_depth).color;

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

  function PathTrace(self : SimplePathTracer; r : Ray; recursion_level : Integer) return PathResult is
    res_color : float3 := (0.0, 0.0, 0.0);
    hit_pos,bxdf,refl,trans : float3;
    h : Hit;
    nextRay : Ray := r;
    ksi : float;
    sign : float := 1.0;
    ksitrans,ksirefl : float;
    --rayDeep : integer := max_depth - recursion_level;
    --pabsorb : float   := 1.0 - 1.0/(float(rayDeep) + 1.0);
  begin

    if recursion_level = 0 then
      return ((0.0, 0.0, 0.0), 0.0, 0.0, false);
    end if;


    h := Intersections.FindClosestHit(r);

    if not h.is_hit then
      return ((0.0, 0.0, 0.0), 0.0, 0.0, false);
    elsif IsLight(h.mat) then
      if dot(r.direction, (0.0,1.0,0.0)) < 0.0 then
        return ((0.0, 0.0, 0.0), h.t, 0.0, false);
      else
        return (Emittance(h.mat), h.t, dot(h.normal, (-1.0)*r.direction), true);
      end if;
    end if;

    -- russian roulette
    --
    --if self.gen.rnd_uniform(0.0, 1.0) < pabsorb then
    --  return ((0.0, 0.0, 0.0), false);
    --end if;

    hit_pos := (r.origin + r.direction*h.t);

    -- pick up next ray
    --
    if length(h.mat.reflection) > 0.0 and not h.mat.fresnel then -- specular reflection
      nextRay.direction := reflect(r.direction, h.normal);
      nextRay.origin    := hit_pos + epsilon*h.normal;
      bxdf              := h.mat.reflection;
    elsif h.mat.fresnel then -- reflection or transmittion

      refl  := h.mat.reflection;
      trans := h.mat.transparency;
      ApplyFresnel(h.mat, dot(r.direction, h.normal), refl, trans);

      ksitrans := length(trans) / (length(refl) + length(trans));
      ksirefl  := length(refl) / (length(refl) + length(trans));

      ksi := self.gen.rnd_uniform(0.0, 1.0);

      if ksi > ksitrans then
        nextRay.direction := reflect(r.direction, h.normal);
        nextRay.origin    := hit_pos + epsilon*h.normal;
        bxdf              := refl*(1.0/ksirefl);
      else
        bxdf              := trans*(1.0/ksitrans);
        if not TotalInternalReflection(h.mat.ior, r.direction, h.normal) then
          if dot(h.normal, r.direction) > 0.0 then
            sign := -1.0;
          end if;
	  refract(h.mat.ior, r.direction, h.normal, wt => nextRay.direction);
          nextRay.origin := hit_pos - epsilon*sign*h.normal;
        else
          nextRay.direction := reflect(r.direction, h.normal);
          nextRay.origin    := hit_pos + epsilon*h.normal;
        end if;

      end if;

    else  -- diffuse reflection
      nextRay.direction := RandomCosineVectorOf(self.gen, h.normal);
      nextRay.origin    := hit_pos + epsilon*h.normal;
      bxdf := h.mat.kd;
    end if;

    return (bxdf*self.PathTrace(nextRay, recursion_level-1).color, h.t, dot(h.normal, (-1.0)*r.direction), false); -- (1.0/(1.0-pabsorb))

  end PathTrace;


  function LightSample(gen : RandRef; lightGeom : FlatLight) return float3 is
    r1 : float := gen.rnd_uniform(0.0, 1.0);
    r2 : float := gen.rnd_uniform(0.0, 1.0);
    x,y,z : float;
  begin
    x := lightGeom.boxMin.x + r1*(lightGeom.boxMax.x - lightGeom.boxMin.x);
    y := lightGeom.boxMin.y;
    z := lightGeom.boxMin.z + r2*(lightGeom.boxMax.z - lightGeom.boxMin.z);
    return (x, y, z);
  end LightSample;


  -- path tracing with shadow rays
  --
  procedure Init(self : in out PathTracerWithShadowRays) is
  begin
    null;
  end Init;

  function PathTrace(self : PathTracerWithShadowRays; r : Ray; recursion_level : Integer) return PathResult is
    res_color : float3 := (0.0, 0.0, 0.0);
    hit_pos,bxdf,refl,trans : float3;
    h : Hit;
    nextRay : Ray := r;
    ksi : float;
    sign : float := 1.0;
    ksitrans,ksirefl : float;
  begin

    if recursion_level = 0 then
      return ((0.0, 0.0, 0.0), 0.0, 0.0, false);
    end if;

    h := Intersections.FindClosestHit(r);

    if not h.is_hit then
      return ((0.0, 0.0, 0.0), 0.0, 0.0, false);
    elsif IsLight(h.mat) then
      return ((0.0, 0.0, 0.0), h.t, dot(h.normal, (-1.0)*r.direction), true);
    end if;

    hit_pos := (r.origin + r.direction*h.t);

    -- explicit sampling
    --
    declare
      lpos : float3 := LightSample(self.gen, g_light);
      r    : float  := length(hit_pos - lpos);
      sdir : float3 := normalize(lpos - hit_pos);
      cos_theta1 : float := max(dot(sdir, h.normal), 0.0);
      cos_theta2 : float := max(dot(sdir,(0.0,1.0,0.0)), 0.0);
      impP : float := g_light.surfaceArea*cos_theta1*cos_theta2/(max(3.1415926535*r*r, 1.0e-37));
    begin

      if not ComputeShadow(hit_pos, lpos).in_shadow then
        res_color := res_color + (h.mat.kd*g_light.intensity)*impP;
      end if;

    end;


    -- pick up next ray
    --
    if length(h.mat.reflection) > 0.0 and not h.mat.fresnel then -- specular reflection
      nextRay.direction := reflect(r.direction, h.normal);
      nextRay.origin    := hit_pos + epsilon*h.normal;
      bxdf              := h.mat.reflection;
    elsif h.mat.fresnel then -- reflection or transmittion

      refl  := h.mat.reflection;
      trans := h.mat.transparency;
      ApplyFresnel(h.mat, dot(r.direction, h.normal), refl, trans);

      ksitrans := length(trans) / (length(refl) + length(trans));
      ksirefl  := length(refl) / (length(refl) + length(trans));

      ksi := self.gen.rnd_uniform(0.0, 1.0);

      if ksi > ksitrans then
        nextRay.direction := reflect(r.direction, h.normal);
        nextRay.origin    := hit_pos + epsilon*h.normal;
        bxdf              := refl*(1.0/ksirefl);
      else
        bxdf              := trans*(1.0/ksitrans);
        if not TotalInternalReflection(h.mat.ior, r.direction, h.normal) then
          if dot(h.normal, r.direction) > 0.0 then
            sign := -1.0;
          end if;
	  refract(h.mat.ior, r.direction, h.normal, wt => nextRay.direction);
          nextRay.origin := hit_pos - epsilon*sign*h.normal;
        else
          nextRay.direction := reflect(r.direction, h.normal);
          nextRay.origin    := hit_pos + 0.00001*h.normal;
        end if;

      end if;

    else  -- diffuse reflection
      nextRay.direction := RandomCosineVectorOf(self.gen, h.normal);
      nextRay.origin    := hit_pos + epsilon*h.normal;
      bxdf := h.mat.kd;
    end if;

    return (res_color + bxdf*self.PathTrace(nextRay, recursion_level-1).color, h.t, dot(h.normal, (-1.0)*r.direction), false);

  end PathTrace;


  function PdfAtoW(aPdfA : in float; aDist : in float; aCosThere : in float) return float is
  begin
    return aPdfA*aDist*aDist/max(aCosThere, 1.0e-28);
  end PdfAtoW;

  -- path tracing with multiple importance sampling
  --
  procedure Init(self : in out PathTracerMIS) is
  begin
    null;
  end Init;

  function PathTrace(self : PathTracerMIS; r : Ray; recursion_level : Integer) return PathResult is
    res_color : float3 := (0.0, 0.0, 0.0);
    hit_pos,bxdf,refl,trans : float3;
    h : Hit;
    nextRay : Ray := r;
    ksi : float;
    sign : float := 1.0;
    ksitrans,ksirefl : float;
    explicitColor : float3 := (0.0, 0.0, 0.0);
    bsdfColor : float3 := (0.0, 0.0, 0.0);
    ret : PathResult;

    specularBounce  : boolean := false;

    cos_theta1 : float := 0.0;
    cos_theta2 : float := 0.0;
    cos_theta  : float := 0.0;

    bsdf_pdf : float := 0.0;
    lgt_pdf  : float := 0.0;
    w        : float := 0.0;
    pdfInv   : float  := 1.0;

    sHit     : Shadow_Hit;

  begin

    if recursion_level = 0 then
      return ((0.0, 0.0, 0.0), 0.0, 0.0, false);
    end if;

    h := Intersections.FindClosestHit(r);

    if not h.is_hit then
      return ((0.0, 0.0, 0.0), 0.0, 0.0, false);
    elsif IsLight(h.mat) then
      if dot(r.direction, (0.0,1.0,0.0)) < 0.0 then
        return ((0.0, 0.0, 0.0), h.t, abs(dot(h.normal, (-1.0)*r.direction)), false);
      else
        return (Emittance(h.mat), h.t, abs(dot(h.normal, (-1.0)*r.direction)), true);
      end if;
    end if;

    hit_pos := (r.origin + r.direction*h.t);


    -- explicit sampling
    --
    declare

      lpos  : float3 := LightSample(self.gen, g_light);
      Ldist : float  := length(hit_pos - lpos);
      Ldir  : float3 := normalize(lpos - hit_pos);

    begin

      cos_theta1 := max(dot(Ldir, h.normal), 0.0);
      cos_theta2 := max(dot(Ldir, (0.0,1.0,0.0)), 0.0); -- minus na munus daet plus

      bsdf_pdf   := cos_theta1 / 3.1415926536;
      lgt_pdf    := Ldist*Ldist /(g_light.surfaceArea*cos_theta2);

      w          := sqr(lgt_pdf)/(sqr(bsdf_pdf) + sqr(lgt_pdf));
      pdfInv     := 1.0/max(lgt_pdf, 1.0e-28);

      sHit       := ComputeShadow(hit_pos, lpos);

      if not sHit.in_shadow then
        explicitColor := (h.mat.kd*(1.0/3.1415926535)*g_light.intensity)*cos_theta1*pdfInv; -- impP := g_light.surfaceArea*cos_theta1*cos_theta2/(max(3.1415926535*Ldist*Ldist, 1.0e-37));
        res_color     := res_color + explicitColor*w;
      end if;

    end;


    -- pick up next ray
    --
    if length(h.mat.reflection) > 0.0 and not h.mat.fresnel then -- specular reflection
      nextRay.direction := reflect(r.direction, h.normal);
      nextRay.origin    := hit_pos + epsilon*h.normal;
      bxdf              := h.mat.reflection;
      specularBounce    := true;
    elsif h.mat.fresnel then -- reflection or transmittion

      refl  := h.mat.reflection;
      trans := h.mat.transparency;
      ApplyFresnel(h.mat, dot(r.direction, h.normal), refl, trans);

      ksitrans := length(trans) / (length(refl) + length(trans));
      ksirefl  := length(refl) / (length(refl) + length(trans));

      ksi := self.gen.rnd_uniform(0.0, 1.0);

      if ksi > ksitrans then
        nextRay.direction := reflect(r.direction, h.normal);
        nextRay.origin    := hit_pos + epsilon*h.normal;
        bxdf              := refl*(1.0/ksirefl);
      else
        bxdf              := trans*(1.0/ksitrans);
        if not TotalInternalReflection(h.mat.ior, r.direction, h.normal) then
          if dot(h.normal, r.direction) > 0.0 then
            sign := -1.0;
          end if;
	  refract(h.mat.ior, r.direction, h.normal, wt => nextRay.direction);
          nextRay.origin := hit_pos - epsilon*sign*h.normal;
        else
          nextRay.direction := reflect(r.direction, h.normal);
          nextRay.origin    := hit_pos + epsilon*h.normal;
        end if;

      end if;

      specularBounce    := true;

    else  -- diffuse reflection
      nextRay.direction := RandomCosineVectorOf(self.gen, h.normal);
      nextRay.origin    := hit_pos + epsilon*h.normal;
      bxdf              := h.mat.kd*(1.0/3.1415926536);
      specularBounce    := false;
    end if;

    ret := self.PathTrace(nextRay, recursion_level-1);

    if specularBounce then

      bsdfColor := bxdf*ret.color;
      res_color := bsdfColor;

    else

      cos_theta := dot(nextRay.direction, h.normal);
      bsdf_pdf  := max(cos_theta / 3.1415926536, 1.0e-28);
      bsdfColor := ret.color*bxdf*cos_theta*(1.0/bsdf_pdf);

      if ret.hitLight then

        lgt_pdf    := ret.dist*ret.dist /(g_light.surfaceArea*ret.lightCos); -- (new_prd.t * new_prd.t / (LnDl * A)
        w          := sqr(bsdf_pdf)/(sqr(bsdf_pdf) + sqr(lgt_pdf));

        --res_color  := explicitColor;
        res_color  := res_color + bsdfColor*(1.0/3.1415926536)*w;

      else

        res_color := bsdfColor + explicitColor;

      end if;

    end if;

    return (res_color, h.t, abs(dot(h.normal, (-1.0)*r.direction)), false);

  end PathTrace;


  -- simple MLT test for copying image
  --
  procedure Init(self : in out MLTCopyImage) is
  begin

    self.mltHist := new AccumBuff(0..width-1, 0..height-1);

  end Init;


  function PathTrace(self : MLTCopyImage; r : Ray; recursion_level : Integer) return PathResult is
  begin
    return ((0.0, 0.0, 0.0), 0.0, 0.0, false); -- not used
  end PathTrace;


  procedure DoPass(self : in out MLTCopyImage; colBuff : AccumBuffRef) is
    r1 : Ray;
    x0,x1,y0,y1 : integer;
    Fx, Fy, Txy, Tyx, Axy : float;
    colorX, colorY : float3;
  begin

    if g_mltTestImage = null then
      Put_Line("MLTCopyImage::MLTSample, src image is null");
    end if;

    -- initialisation part
    --
    self.brightnessEstim := 0.0;

    for y in 0 .. height-1 loop
      for x in 0 .. width-1 loop
        self.mltHist(x,y) := (0.0, 0.0, 0.0);
        self.brightnessEstim := self.brightnessEstim + Luminance(g_mltTestImage(x,y));
      end loop;
    end loop;

    self.brightnessEstim := self.brightnessEstim/( float(width*height) );


    ----------------------------------------------------------------------------------
    ----------------------------------------------------------------------------------

    -- MLT algorithm begin
    --
    x0 := integer(self.gen.rnd_uniform(0.0, float(width-1)));
    x1 := integer(self.gen.rnd_uniform(0.0, float(height-1)));

    -- Create an initial sample point
    --
    colorX := g_mltTestImage(x0, x1);
    Fx     := Luminance(colorX);
    colorX := colorX*(1.0/Fx);

    -- In this example, the tentative transition function T simply chooses
    -- a random pixel location, so Txy and Tyx are always equal.
    --
    Txy := 1.0/(float(width)*float(height));
    Tyx := 1.0/(float(width)*float(height));

    -- Create a histogram of values using Metropolis sampling.
    --
    for i in 1 .. (width*height*self.mutationsPerPixel) loop

      y0 := integer(self.gen.rnd_uniform(0.0, float(width-1)));
      y1 := integer(self.gen.rnd_uniform(0.0, float(height-1)));

      colorY := g_mltTestImage(y0, y1);
      Fy     := Luminance(colorY);
      colorY := colorY*(1.0/Fy);

      Axy := min(1.0, (Fy * Txy) / (Fx * Tyx));

      if self.gen.rnd_uniform(0.0, 1.0) < Axy then
        x0 := y0; x1 := y1;
        Fx := Fy;
        colorX := colorY;
      end if;

      self.mltHist(x0,x1) := self.mltHist(x0,x1) + colorX;

    end loop;
    --
    -- MLT algorithm end

    ----------------------------------------------------------------------------------
    ----------------------------------------------------------------------------------



    -- now scale histogram to obtain final image
    --
    for y in 0 .. height-1 loop
      for x in 0 .. width-1 loop
        colBuff(x,y) := self.mltHist(x,y)*(1.0/float(self.mutationsPerPixel))*self.brightnessEstim;
      end loop;
    end loop;

  end DoPass;


  ----- MLTSimple
  --

  procedure Init(self : in out MLTSimple) is
  begin

    self.mltHist := new AccumBuff(0..width-1, 0..height-1);

  end Init;

  -- attempt to make simple MLT working
  --
  function PathTrace(self : MLTSimple; r : Ray; recursion_level : Integer) return PathResult is
  begin
    return PathTrace(SimplePathTracer(self), r, recursion_level);
  end PathTrace;


  procedure DoPass(self : in out MLTSimple; colBuff : AccumBuffRef) is
    r : Ray;
    x0,x1,y0,y1 : integer;
    Fx, Fy, Txy, Tyx, Axy : float;
    colorX, colorY : float3;
  begin

    if g_mltTestImage = null then
      Put_Line("MLTCopyImage::MLTSample, src image is null");
    end if;

    -- initialisation part
    --
    self.brightnessEstim := 0.0;

    for y in 0 .. height-1 loop
      for x in 0 .. width-1 loop
        self.mltHist(x,y) := (0.0, 0.0, 0.0);
        --self.brightnessEstim := self.brightnessEstim + Luminance(g_mltTestImage(x,y));
      end loop;
    end loop;

    self.brightnessEstim := 0.25; --self.brightnessEstim/( float(width*height) );

    r.origin := g_cam.pos;

    ----------------------------------------------------------------------------------
    ----------------------------------------------------------------------------------

    -- MLT algorithm begin
    --
    x0 := integer(self.gen.rnd_uniform(0.0, float(width-1)));
    x1 := integer(self.gen.rnd_uniform(0.0, float(height-1)));

    r.direction  := EyeRayDirection(x0,x1);
    r.direction  := normalize(g_cam.matrix*r.direction);

    -- Create an initial sample point
    --
    colorX := self.PathTrace(r, max_depth).color;
    Fx     := Luminance(colorX);
    colorX := colorX*(1.0/Fx);

    -- In this example, the tentative transition function T simply chooses
    -- a random pixel location, so Txy and Tyx are always equal.
    --
    Txy := 1.0/(float(width)*float(height));
    Tyx := 1.0/(float(width)*float(height));

    -- Create a histogram of values using Metropolis sampling.
    --
    for i in 1 .. (width*height*self.mutationsPerPixel) loop

      y0 := integer(self.gen.rnd_uniform(0.0, float(width-1)));
      y1 := integer(self.gen.rnd_uniform(0.0, float(height-1)));

      r.direction  := EyeRayDirection(y0,y1);
      r.direction  := normalize(g_cam.matrix*r.direction);

      colorY := self.PathTrace(r, max_depth).color;
      Fy     := Luminance(colorY);
      colorY := colorY*(1.0/Fy);

      Axy := min(1.0, (Fy * Txy) / (Fx * Tyx));

      if self.gen.rnd_uniform(0.0, 1.0) < Axy then
        x0 := y0; x1 := y1;
        Fx := Fy;
        colorX := colorY;
      end if;

      self.mltHist(x0,x1) := self.mltHist(x0,x1) + colorX;

    end loop;
    --
    -- MLT algorithm end

    ----------------------------------------------------------------------------------
    ----------------------------------------------------------------------------------



    -- now scale histogram to obtain final image
    --
    for y in 0 .. height-1 loop
      for x in 0 .. width-1 loop
        colBuff(x,y) := self.mltHist(x,y)*(1.0/float(self.mutationsPerPixel))*self.brightnessEstim; -- (1.0/float(g_mltMutations))
      end loop;
    end loop;

  end DoPass;

















  ----- Simple Kelmen Style MLT; no shadow rays, no MIS.
  --

  procedure Init(self : in out MLTKelmenSimple) is
  begin

    self.mltHist := new AccumBuff(0..width-1, 0..height-1);
    self.gen := new KMLT_Generator;

    --Put_Line("MLTKelmenSimple.Init");

  end Init;


  procedure Push(gen : in out KMLT_Generator; i : in integer; val : in float) is
  begin

    if gen.top < QMC_KMLT_MAXRANDS then
      gen.indices_stack(gen.top) := i;
      gen.values_stack(gen.top)  := val;
      gen.top              := gen.top + 1;
    else
      Put_Line("KMLT_Generator, Stack overflow");
      raise Constraint_Error;
    end if;

  end Push;


  procedure Pop(gen : in out KMLT_Generator; i : out integer; val : out float) is
  begin

    if gen.top > 0 then
      gen.top := gen.top - 1;
      i       := gen.indices_stack(gen.top);
      val     := gen.values_stack(gen.top);
      gen.indices_stack(gen.top) := 0;
      gen.values_stack(gen.top)  := 0.0;
    else
      Put_Line("KMLT_Generator, Stack underflow");
      raise Constraint_Error;
    end if;
  end Pop;

  function  IsStackEmpty(gen : in KMLT_Generator) return boolean is
  begin
    return (gen.top = 0);
  end IsStackEmpty;

  procedure ClearStack(gen : in out KMLT_Generator) is
  begin
    for i in 0 .. gen.top - 1 loop
      gen.indices_stack(i) := 0;
      gen.values_stack(i)  := 0.0;
    end loop;
    gen.top := 0;
  end ClearStack;


  function Mutate(gen : in KMLT_Generator; a_value : float) return float is
    s1,s2,dv,value : float;
  begin

    s1 := 1.0/1024.0;
    s2 := 1.0/64.0;
    dv := s2*exp(-log(s2/s1)*gen.rnd_uniform_simple(0.0, 1.0));
    value := a_value;

    if gen.rnd_uniform_simple(0.0, 1.0) < 0.5 then
      value := value + dv;
      if value > 1.0 then
        value := value - 1.0;
      end if;
    else
      value := value - dv;
      if value < 0.0 then
        value := value + 1.0;
      end if;
    end if;

    return value;
  end Mutate;

  -- Let us define a counter called 'time' for the global time of
  -- the process which counts the number of accepted mutations.
  -- Each coordinate is associated with a time-stamp called 'modify'
  -- that stores the global time when this coordinate was modified
  -- most recently. The time of the last accepted large step is
  -- stored in variable large_step_time. When a new coordinate is
  -- needed, it is checked whether this coordinate has been used
  -- before. If not, it is initialized as a random number and its
  -- time stamp is set to large_step_time. If it has already been
  -- used, the value of the coordinate was set at time modify. Then
  -- the coordinate is perturbed by time modify times.
  --
  procedure PrimarySample(gen : in out KMLT_Generator; i : in integer; time : in integer; res : out float) is
  begin

    -- copy-paste from Mitsuba
    --
    -- while (i >= m_u.size())
    --   m_u.push_back(SampleStruct(m_random->nextFloat()));

    gen.values(i) := gen.rnd_uniform_simple(0.0, 1.0);

    if gen.modify(i) < time then

      if gen.large_step = 1 then    -- large srep
        gen.Push(i, gen.values(i)); -- save state
        gen.modify(i) := time;
        gen.values(i) := gen.rnd_uniform_simple(0.0, 1.0);
      else                          -- small step

        if gen.modify(i) < gen.large_step_time then
          gen.modify(i) := gen.large_step_time;
          gen.values(i) := gen.rnd_uniform_simple(0.0, 1.0);
        end if;

        -- lazy evaluation of mutations
        --
        while gen.modify(i) < time-1 loop
          gen.values(i) := Mutate(gen, gen.values(i));
          gen.modify(i) := gen.modify(i) + 1;
        end loop;

        -- save state
        --
       gen.Push(i, gen.values(i));
       gen.values(i) := Mutate(gen, gen.values(i));
       gen.modify(i) := gen.modify(i) + 1;

      end if;
    end if;

    res := gen.values(i);

  end PrimarySample;

  -- override our rnd_uniform used for sample evaluation
  --
  function rnd_uniform(gen : access KMLT_Generator; l,h : float) return float is
    t: float := 0.0;
  begin
    --t := Ada.Numerics.Float_Random.Random(Gen => gen.agen);
    PrimarySample(gen.all, gen.u_id, gen.time, t);
    gen.u_id := gen.u_id + 1;
    return l + (h-l)*t;
  end rnd_uniform;

  procedure ResetSequenceCounter(gen : in out KMLT_Generator) is
  begin
    gen.u_id := 0;
  end ResetSequenceCounter;

  procedure InitSequence(gen : in out KMLT_Generator) is
  begin

    for i in 0 .. gen.u_id loop
      gen.values(i) := 0.0;
    end loop;

    gen.u_id := 0;
    gen.ClearStack;

    --
    --
    for i in 0 .. QMC_KMLT_MAXRANDS-1 loop
      gen.modify(i)  := 0;
    end loop;

    gen.time := 0;

  end  InitSequence;

  procedure ResetAllModifyCounters(gen : in out KMLT_Generator) is
  begin

    for i in 0 .. QMC_KMLT_MAXRANDS-1 loop
      gen.modify(i) := 0;
    end loop;

    gen.time := 0; -- ???

  end ResetAllModifyCounters;

  procedure RestoreSequence(gen : in out KMLT_Generator) is
    ix : integer := 0;   -- temp
    ui : float   := 0.0; -- temp
  begin

    while not IsStackEmpty(gen) loop   -- restore state
      gen.Pop(ix, ui);
      gen.values(ix) := ui;
    end loop;

  end  RestoreSequence;


  -- The following Next function
  -- handles both rejected and accepted samples, but returns
  -- only one of them to be contributed to the affected pixel. If
  -- rejection happens, the returned sample is the rejected one,
  -- since it will be invalid in the next cycle. However, if the sample
  -- is accepted, then the contributed sample is the old sample
  -- while the weight of the new sample will be increased in the
  -- next cycle. The function gets the affected pixel and its contribution
  -- F*(u) in variable contrib, and the transformed
  -- scalar contribution I*(u) in variable I
  --

  procedure NextSample(gen           : in out KMLT_Generator;
                       I             : in float;
                       oldI          : in out float;
                       totalSamples  : in integer;
                       contrib       : in float3;
                       oldsample     : in out Sample;
                       contribsample : out Sample) is

    a         : float;
    newsample : Sample;

    b         : float := 0.25;
    plarge    : float := 1.0/4.0;
    M         : float := float(totalSamples);

    ix : integer := 0;   -- temp
    ui : float   := 0.0; -- temp

  begin

    if oldI = 0.0 then
      a := 1.0;
    else
      a := min(1.0, I/oldI);
    end if;

    newsample.contrib := contrib;
    newsample.w := (a + float(gen.large_step))/( (I/b + plarge)*M); --  (a + float(gen.large_step))/(I/b+plarge)/M;

    oldsample.w := oldsample.w + (1.0-a)/((oldI/b + plarge)*M);       -- oldsample.w + (1.0-a)/(oldI/b+plarge)/M; -- cumulate weight

    if gen.rnd_uniform_simple(0.0, 1.0) < a then -- accept

      oldI          := I;
      contribsample := oldsample;
      oldsample     := newsample;

      if gen.large_step = 1 then
        gen.large_step_time := gen.time;
      end if;

      gen.time := gen.time + 1;
      ClearStack(gen);                   -- no state restoration

    else

      contribsample := newsample;
      gen.RestoreSequence;

    end if;

    -- decide wherther 'next after this-next' sample will be large or not
    --
    if gen.rnd_uniform_simple(0.0, 1.0) < plarge then
      gen.large_step := 1;
    else
      gen.large_step := 0;
    end if;

  end NextSample;





  -- just call SimplePathTracer's implementation
  --
  function PathTrace(self : MLTKelmenSimple; r : Ray; recursion_level : Integer) return PathResult is
  begin
    return PathTrace(PathTracerMIS(self), r, recursion_level);
  end PathTrace;


  procedure DoPass(self : in out MLTKelmenSimple; colBuff : AccumBuffRef) is
    r : Ray;
    x0,x1,y0,y1 : integer;
    Fx, Fy : float  := 0.0;
    colorX : float3 := (0.0, 0.0, 0.0);
    colorY : float3 := (0.0, 0.0, 0.0);
    oldsample : Sample := ((0.0, 0.0, 0.0), 0.0, 0, 0);
    newsample : Sample := ((0.0, 0.0, 0.0), 0.0, 0, 0);

    test1 : float   := self.gen.values(0);
    test2 : float   := self.gen.values_stack(0);
    test3 : integer := self.gen.indices_stack(0);
    test4 : integer := self.gen.u_id;
    test5 : integer := self.gen.top;
    test6 : integer := self.gen.time;
    test7 : integer := self.gen.large_step;
    test8 : integer := self.gen.large_step_time;
    test9 : integer := self.gen.modify(0);

  begin

    -- initialisation part
    --
    self.brightnessEstim := 0.0;

    for y in 0 .. height-1 loop
      for x in 0 .. width-1 loop
        self.mltHist(x,y) := (0.0, 0.0, 0.0);
        --self.brightnessEstim := self.brightnessEstim + Luminance(g_mltTestImage(x,y));
      end loop;
    end loop;

    self.brightnessEstim := 0.25; --self.brightnessEstim/( float(width*height) );

    r.origin := g_cam.pos;



    ----------------------------------------------------------------------------------
    ----------------------------------------------------------------------------------

    -- Create an initial sample point
    --
    Fx := 0.0;

    while (Fx <= 0.0) or (Fx >= 20.0) loop

      self.gen.InitSequence;

      x0 := integer(self.gen.rnd_uniform(0.0, float(width-1)));
      x1 := integer(self.gen.rnd_uniform(0.0, float(height-1)));

      r.direction  := EyeRayDirection(x0,x1);
      r.direction  := normalize(g_cam.matrix*r.direction);

      colorX := self.PathTrace(r, max_depth).color;
      Fx     := Luminance(colorX);

    end loop;

    if Fx > 0.0 then
      colorX := colorX*(1.0/Fx);
    else
      colorX := (0.0, 0.0, 0.0);
    end if;


    oldsample.contrib := colorX;
    oldsample.w       := 1.0;
    oldsample.x       := x0;
    oldsample.y       := x1;

    newsample.contrib := (0.0, 0.0, 0.0);
    newsample.w       := 0.0;


    -- prepare for main part
    --
    self.gen.ClearStack;
    self.gen.ResetAllModifyCounters;

    -- Create a histogram of values using Metropolis sampling.
    --
    for i in 1 .. (width*height*self.mutationsPerPixel) loop

      self.gen.ResetSequenceCounter;

      y0 := integer(self.gen.rnd_uniform(0.0, float(width-1)));
      y1 := integer(self.gen.rnd_uniform(0.0, float(height-1)));

      newsample.x := y0;
      newsample.y := y1;

      r.direction  := EyeRayDirection(y0,y1);
      r.direction  := normalize(g_cam.matrix*r.direction);

      colorY := self.PathTrace(r, max_depth).color;
      Fy     := Luminance(colorY);
      if Fy > 0.0 then
        colorY := colorY*(1.0/Fy);
      else
        colorY := (0.0, 0.0, 0.0);
      end if;

      NextSample(gen           => RandomGenerator'Class(self.gen.all), -- don't fuck your mind with Ada83 and Ada95 dynamic dispatch syntax,
                 I             => Fy,                                  -- it is the explicit form of Ada2005 'self.gen.NextSample(....)';
                 oldI          => Fx,                                  -- just a call of 'virtual function'.
                 totalSamples  => i,                                   -- i decided to put this 'old-fashioned' form here cause it is more explicit
                 contrib       => colorY,                              --
                 oldsample     => oldsample,
                 contribsample => newsample);

      self.mltHist(newsample.x, newsample.y) := self.mltHist(newsample.x, newsample.y) + newsample.contrib*newsample.w;

    end loop;
    --
    -- MLT algorithm end

    ----------------------------------------------------------------------------------
    ----------------------------------------------------------------------------------



    -- now scale histogram to obtain final image
    --
    for y in 0 .. height-1 loop
      for x in 0 .. width-1 loop
        colBuff(x,y) := self.mltHist(x,y)*(1.0/float(self.mutationsPerPixel)); --*self.brightnessEstim; -- (1.0/float(g_mltMutations))
      end loop;
    end loop;

  end DoPass;



end Ray_Tracer.Integrators;
