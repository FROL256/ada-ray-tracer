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
      return ((0.0, 0.0, 0.0), false);
    end if;


    h := Intersections.FindClosestHit(r);

    if not h.is_hit then
      return ((0.0, 0.0, 0.0), false);
    elsif IsLight(h.mat) then
      if dot(r.direction, (0.0,1.0,0.0)) < 0.0 then
        return ((0.0, 0.0, 0.0), false);
      else
        return (Emittance(h.mat), true);
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
      nextRay.direction := self.gen.RandomCosineVectorOf(h.normal);
      nextRay.origin    := hit_pos + epsilon*h.normal;
      bxdf := h.mat.kd;
    end if;

    return (bxdf*self.PathTrace(nextRay, recursion_level-1).color, false); -- (1.0/(1.0-pabsorb))

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
      return ((0.0, 0.0, 0.0), false);
    end if;

    h := Intersections.FindClosestHit(r);

    if not h.is_hit then
      return ((0.0, 0.0, 0.0), false);
    elsif IsLight(h.mat) then
      return ((0.0, 0.0, 0.0), true);
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
      impP : float := g_light.surfaceArea*cos_theta1*cos_theta2/(max(3.1415926535*r*r, epsilon));
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
      nextRay.direction := self.gen.RandomCosineVectorOf(h.normal);
      nextRay.origin    := hit_pos + epsilon*h.normal;
      bxdf := h.mat.kd;
    end if;

    return (res_color + bxdf*self.PathTrace(nextRay, recursion_level-1).color, false);

  end PathTrace;




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
    implicitColor : float3 := (0.0, 0.0, 0.0);
    ret : PathResult;
    pi,pe,wi,we : float;
    specularBounce : boolean := false;
  begin

    if recursion_level = 0 then
      return ((0.0, 0.0, 0.0), false);
    end if;

    h := Intersections.FindClosestHit(r);

    if not h.is_hit then
      return ((0.0, 0.0, 0.0), false);
    elsif IsLight(h.mat) then
      if dot(r.direction, (0.0,1.0,0.0)) < 0.0 then
        return ((0.0, 0.0, 0.0), false);
      else
        return (Emittance(h.mat), true);
      end if;
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
      impP : float  := g_light.surfaceArea*cos_theta1*cos_theta2/(max(3.1415926535*r*r, epsilon));
    begin

      if not ComputeShadow(hit_pos, lpos).in_shadow then
        explicitColor := (h.mat.kd*g_light.intensity)*impP;
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
      nextRay.direction := self.gen.RandomCosineVectorOf(h.normal);
      nextRay.origin    := hit_pos + epsilon*h.normal;
      bxdf              := h.mat.kd;
      specularBounce    := false;
    end if;

    ret := self.PathTrace(nextRay, recursion_level-1);
    implicitColor := bxdf*ret.color;

    -- Multiple importance Sampling (MIS)
    --
    if ret.hitLight then
      pe := 1.0/g_light.surfaceArea;
      pi := 3.1415926535/max(dot(nextRay.direction, h.normal), epsilon);

      if specularBounce then
        res_color := implicitColor;
      else
        wi := sqr(pi)/(sqr(pe) + sqr(pi));
        we := sqr(pe)/(sqr(pe) + sqr(pi));
        res_color := implicitColor*(wi/pi) + explicitColor*(we/pe);
      end if;
    else
      res_color := implicitColor + explicitColor;
    end if;

    return (res_color, false);

  end PathTrace;


  -- simple MLT test for copying image
  --
  procedure Init(self : in out MLTCopyImage) is
  begin

    self.mltHist := new AccumBuff(0..width-1, 0..height-1);

  end Init;


  function PathTrace(self : MLTCopyImage; r : Ray; recursion_level : Integer) return PathResult is
  begin
    return ((0.0, 0.0, 0.0), false); -- not used
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


    self.qmcGen := new QMC_KMLT_Generator;

    if self.gen /= null then
      delete(self.gen);
    end if;
    self.gen := RandRef( self.qmcGen );

  end Init;

  function rnd_uniform_dispatch(gen : access QMC_KMLT_Generator; l,h : float) return float is
    t: float := 0.0;
  begin
    t := Ada.Numerics.Float_Random.Random(Gen => gen.agen);
    gen.top := gen.top+1;
    Put("self.top = ");Put_Line(integer'Image(gen.top));
    return l + (h-l)*t;
  end rnd_uniform_dispatch;


  procedure Push(self : in QMC_KMLT_Generator; i : in integer; val : in float) is
  begin
    null;
  end Push;


  procedure Pop(self : in QMC_KMLT_Generator; i : out integer; val : out float) is
  begin
    null;
  end Pop;


  procedure PrimarySample(self : in QMC_KMLT_Generator; res : out float) is
  begin
    null;
  end PrimarySample;

  function Mutate(self : in MLTKelmenSimple; a_value : float) return float is
    s1,s2,dv,value : float;
  begin

    s1 := 1.0/1024.0;
    s2 := 1.0/64.0;
    dv := s2*exp(-log(s2/s1)*self.gen.rnd_uniform_simple(0.0, 1.0));
    value := a_value;

    if self.gen.rnd_uniform_simple(0.0, 1.0) < 0.5 then
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



  --
  --
  function PathTrace(self : MLTKelmenSimple; r : Ray; recursion_level : Integer) return PathResult is
  begin
    return PathTrace(SimplePathTracer(self), r, recursion_level);
  end PathTrace;


  procedure DoPass(self : in out MLTKelmenSimple; colBuff : AccumBuffRef) is
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



end Ray_Tracer.Integrators;

