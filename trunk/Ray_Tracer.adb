with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Materials;
with Ada.Exceptions;
with Ray_Tracer.Intersections;


use Ada.Numerics;
use Ada.Text_IO;
use Materials;

package body Ray_Tracer is

  package Float_Functions is new Generic_Elementary_Functions (float);
  use Float_Functions;


  function ToneMapping(v : float3) return Color is
    max_val : float;
   begin

    max_val := max(v.x, max(v.y, v.z));
    if max_val > 0.0 then

      if max_val > 1.0 then
        return (v.x/max_val, v.y/max_val, v.z/max_val);
      else
        return (v.x, v.y, v.z);
      end if;

    else
      return (0.0, 0.0, 0.0);
    end if;

  end;

  function Luminance(c : float3) return float is
  begin
    return c.x*0.299 + c.y*0.587 + c.z*0.114;
  end Luminance;


  function ColorToUnsigned_32(c : Color) return Unsigned_32 is
    res : Unsigned_32 := 0;
    r, g, b : Float;
    red, green, blue : Unsigned_32;
  begin

    r := c.red*255.0;
    g := c.green*255.0;
    b := c.blue*255.0;

    red := Unsigned_32(r);
    green := Unsigned_32(g);
    blue := Unsigned_32 (b);

    return red or Shift_Left(green, 8) or Shift_Left(blue, 16);

  end ColorToUnsigned_32;



  function EyeRayDirection (x, y : Natural) return float3 is
    res : float3;
    fov : float := Pi / (2.0);
  begin
    res.x := float(x) + 0.5 - (float(width) / 2.0);
    res.y := float(y) + 0.5 - (float(height) / 2.0);
    res.z := -float(width) / safe_tan(fov / 2.0);
    return normalize(res);
  end;


  procedure Generate4RayDirections (x, y : in Natural; arr : out RayDirPack) is
    fov : float := Pi / (2.0);
  begin

    arr(0).x := float(x) + 1.0/3.0 - (float(width) / 2.0);
    arr(0).y := float(y) + 1.0/3.0 - (float(height) / 2.0);
    arr(0).z := -float (width) / safe_tan (fov / 2.0);

    arr(1).x := float(x) + 1.0/3.0 - (float(width) / 2.0);
    arr(1).y := float(y) + 2.0/3.0 - (float(height) / 2.0);
    arr(1).z := -float (width) / safe_tan (fov / 2.0);

    arr(2).x := float(x) + 2.0/3.0 - (float(width) / 2.0);
    arr(2).y := float(y) + 1.0/3.0 - (float(height) / 2.0);
    arr(2).z := -float (width) / safe_tan (fov / 2.0);

    arr(3).x := float(x) + 2.0/3.0 - (float(width) / 2.0);
    arr(3).y := float(y) + 2.0/3.0 - (float(height) / 2.0);
    arr(3).z := -float (width) / safe_tan (fov / 2.0);

    arr(0) := normalize(arr(0));
    arr(1) := normalize(arr(1));
    arr(2) := normalize(arr(2));
    arr(3) := normalize(arr(3));

  end;


  function ComputeShadow(hit_pos : float3; lpos : float3) return Shadow_Hit is
    res : Shadow_Hit;
    shadowRay : Ray;
      h : Hit;
      epsilon : float;
      epsilon2 : float;
      maxDist : float;
  begin

    epsilon := max(abs(hit_pos.x), abs(hit_pos.y), abs(hit_pos.z))*0.00001;
    epsilon := max(epsilon, 0.000001);

    shadowRay.direction := normalize(lpos-hit_pos);
    shadowRay.origin    := hit_pos + shadowRay.direction*epsilon;

    h := Intersections.FindClosestHit(shadowRay);

    maxDist  := length(hit_pos - lpos);
    epsilon2 := max(maxDist*0.0001, 0.00001);

    if h.is_hit and then (h.t < maxDist-epsilon2 and h.t > 10.0*epsilon) then
      res.in_shadow        := true;
      res.percentageCloser := (1.0, 1.0, 1.0);
    else
      res.in_shadow        := false;
      res.percentageCloser := (0.0, 0.0, 0.0);
    end if;

    return res;

  end ComputeShadow;


  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ----------------------------------------------------------------- Monte-Carlo Path Tracing -----------------------------------------------------------------------------------
  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  --
  function rnd_uniform(gen : RandomGenerator; l,h : float) return float is
    t: float := 0.0;
  begin
    t := Ada.Numerics.Float_Random.Random(Gen => gen.agen);
    return l + (h-l)*t;
  end rnd_uniform;

  procedure regenerateSequence(gen : RandomGenerator) is
  begin
    null;
  end regenerateSequence;


  function MapSampleToCosineDist(r1,r2 : float; direction, normal : float3; power : float) return float3 is
   e,sin_phi,cos_phi: float;
   sin_theta,cos_theta : float;
   deviation,nx,ny,nz,tmp,res : float3;
   invSign : float;
  begin

    e := power;
    sin_phi := sin(2.0*r1*3.141592654);
    cos_phi := cos(2.0*r1*3.141592654);

    cos_theta := (1.0-r2) ** (1.0/(e+1.0));
    sin_theta := sqrt(1.0-cos_theta*cos_theta);

    deviation := (sin_theta*cos_phi, sin_theta*sin_phi, cos_theta);

    ny := direction;
    nx := normalize(cross(ny, (1.04,2.93,-0.6234)));
    nz := normalize(cross(nx, ny));

    tmp := ny; ny := nz; nz := tmp; -- swap(ny,nz);  // depends on the coordinate system

    res := nx*deviation.x + ny*deviation.y + nz*deviation.z;

    if dot(direction, normal) > 0.0 then
      invSign := 1.0;
    else
      invSign := -1.0;
    end if;

    if invSign*dot(res, normal) < 0.0 then
      res := (-1.0)*nx*deviation.x + ny*deviation.y - nz*deviation.z;
    end if;

    return res;

  end MapSampleToCosineDist;

  function RandomCosineVectorOf(gen : RandRef; norm : float3) return float3 is
    r1 : float := gen.rnd_uniform(0.0, 1.0);
    r2 : float := gen.rnd_uniform(0.0, 1.0);
  begin
    return MapSampleToCosineDist(r1,r2,norm,norm,1.0);
  end RandomCosineVectorOf;



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
              color := color + PathTrace(Integrator'Class(self), r,max_depth).color;
            end loop;

            colBuff(x,y) := color*0.25;

          else

            r.x := x; r.y := y;
            r.direction  := EyeRayDirection(x,y);
            r.direction  := normalize(g_cam.matrix*r.direction);
            colBuff(x,y) := PathTrace(Integrator'Class(self), r,max_depth).color;

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

    return (bxdf*self.PathTrace(nextRay, recursion_level-1).color, false);

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
      nextRay.direction := RandomCosineVectorOf(self.gen, h.normal);
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
      nextRay.direction := RandomCosineVectorOf(self.gen, h.normal);
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



  -- attempt to make simple MLT working
  --
  function PathTrace(self : MLTSimple; r : Ray; recursion_level : Integer) return PathResult is
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
      if dot(r.direction, (0.0,1.0,0.0)) < 0.0 then
        return ((0.0, 0.0, 0.0), false);
      else
        return (Emittance(h.mat), true);
      end if;
    end if;

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

    return (bxdf*self.PathTrace(nextRay, recursion_level-1).color, false);

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







  -- multithread stuff
  --
  task body Path_Trace_Thread is
    colBuff : AccumBuffRef;
    mygen   : RandRef := new RandomGenerator;
    tracer  : IntegratorRef := null;
  begin

    colBuff := new AccumBuff(0..width-1, 0..height-1);
    Ada.Numerics.Float_Random.Reset(Gen => mygen.agen, Initiator => threadId*7 + threadId*threadId*13);

    -- select integrator
    --

    --tracer := new SimplePathTracer;
    --tracer := new PathTracerWithShadowRays;
    --tracer := new PathTracerMIS;
    --tracer := new MLTCopyImage;
    tracer := new MLTSimple;

    tracer.gen := mygen;
    tracer.Init;

    while true loop

      accept Resume;

      tracer.DoPass(colBuff);

      accept Finish (accBuff : AccumBuffRef; spp : IntRef) do

        declare
          c1 : float := float(spp.all) / (float(spp.all) + 1.0);
          c2 : float := 1.0 / (float(spp.all) + 1.0);
          r,g,b: float;
        begin

          for y in 0 .. height - 1 loop
            for x in 0 .. width - 1 loop
              accBuff(x,y) := c1*accBuff(x,y) + c2*colBuff(x,y);
              r := accBuff(x,y).x; g := accBuff(x,y).y; b := accBuff(x,y).z;
              r := r ** (1.0/g_gamma); -- gamma correction
              g := g ** (1.0/g_gamma);
              b := b ** (1.0/g_gamma);
              screen_buffer(x,y) := ColorToUnsigned_32(ToneMapping((r,g,b)));
            end loop;
          end loop;

          spp.all := spp.all + 1;

        end;

      end Finish;

    end loop;

    delete(colBuff); colBuff := null;
    delete(mygen); mygen := null;

    exception
      when The_Error : others =>

      Put_Line("Error raised in the thread:");
        Put_Line(Ada.Exceptions.Exception_Name(The_Error));
        Put_Line(Ada.Exceptions.Exception_Message(The_Error));
        Put_Line("");

        delete(colBuff);
        delete(mygen);

  end Path_Trace_Thread;

  procedure MultiThreadedPathTracing is
    --threads : array(0..threads_num-1) of Path_Trace_Thread_Ptr;
  begin

    if not g_threadsCreated then
      for i in 0..threads_num-1 loop
        g_threads(i) := new Path_Trace_Thread(i+1);
      end loop;
      g_threadsCreated := true;
    end if;

    for i in 0..threads_num-1 loop
      g_threads(i).Resume;
    end loop;

    for i in 0..threads_num-1 loop
      g_threads(i).Finish(g_accBuff, g_spp);
    end loop;

  end MultiThreadedPathTracing;



  procedure InitCornellBoxScene is
  begin

    -- init materials
    --
    for i in 0 .. g_scn.materials'Last loop
      if g_scn.materials(i) = null then
        g_scn.materials(i) := new LegacyMaterial;
      end if;
    end loop;

    -- glass material
    --
    g_scn.materials(0).kd           := (0.0, 0.0, 0.0);
    g_scn.materials(0).reflection   := (1.0,1.0,1.0);
    g_scn.materials(0).roughness    := 0.25;
    g_scn.materials(0).transparency := (1.0,1.0,1.0);
    g_scn.materials(0).ior          := 1.75;
    g_scn.materials(0).fresnel      := true;


    -- floor material
    --
    g_scn.materials(1).ka := (0.0, 0.0, 0.0);
    g_scn.materials(1).kd := (0.5, 0.5, 0.5);

    -- Left wall
    --
    g_scn.materials(2).ka := (0.0, 0.0, 0.0);
    g_scn.materials(2).kd := (0.25, 0.65, 0.0);

    -- Right wall
    --
    g_scn.materials(3).ka := (0.0, 0.0, 0.0);
    g_scn.materials(3).kd := (0.5, 0.0, 0.0);

    -- Light material
    --
    g_scn.materials(4).kd     := (0.0, 0.0, 0.0);
    g_scn.materials(4).ka     := (20.0, 20.0, 20.0);
    g_light.intensity   := g_scn.materials(4).ka;
    g_light.surfaceArea := (g_light.boxMax.x - g_light.boxMin.x)*(g_light.boxMax.z - g_light.boxMin.z);

    -- Mirror
    --
    g_scn.materials(5).kd           := (0.0, 0.0, 0.0);
    g_scn.materials(5).ks           := (0.5,0.5,0.5);
    g_scn.materials(5).reflection   := (0.5,0.5,0.5);
    g_scn.materials(5).roughness    := 0.25;
    g_scn.materials(5).transparency := (0.0,0.0,0.0);
    g_scn.materials(5).ior          := 1.5;

    -- Glass 2
    --
    g_scn.materials(6).kd := (0.0, 0.0, 0.0);
    g_scn.materials(6).ks := (0.0, 0.0, 0.0);
    g_scn.materials(6).reflection := (0.0,0.0,0.0);
    g_scn.materials(6).roughness  := 0.5;
    g_scn.materials(6).transparency := (0.85,0.85,0.85);
    g_scn.materials(6).ior := 1.75;

    -- blue
    --
    g_scn.materials(7).ka := (0.0, 0.0, 0.0);
    g_scn.materials(7).kd := (0.0, 0.0, 0.5);

    -- init spheres geometry
    --
    if g_scn.spheres /= null then
      delete(g_scn.spheres);
    end if;

    g_scn.spheres := new Spheres_Array(0..1);

    g_scn.spheres(0).pos := (-1.5,1.0,1.5);
    g_scn.spheres(0).r   := 1.0;
    g_scn.spheres(0).mat := g_scn.materials(5); -- 5, 3

    g_scn.spheres(1).pos := (1.4,1.0,3.0);
    g_scn.spheres(1).r   := 1.0;
    g_scn.spheres(1).mat := g_scn.materials(0); -- 0, 1

    g_scn.lights(0).color := (2.5, 2.5, 2.5);
    g_scn.lights(0).pos   := (0.0, 4.97, 2.25);

    -- setup camera
    --
    g_cam.pos    := (0.0, 2.5, 12.0);
    g_cam.lookAt := (0.0, 0.0, 0.0);
    g_cam.up     := (0.0, 1.0, 0.0);
    g_cam.matrix := IdentityMatrix;

  end InitCornellBoxScene;


  procedure ResizeViewport(size_x, size_y : integer) is

  begin
    width  := Positive(size_x);
    height := Positive(size_y);

    delete(screen_buffer);
    screen_buffer := new ScreenBufferData(0..width-1, 0..height-1);

    delete(g_spp);
    delete(g_accBuff);

    g_spp     := new Integer;
    g_accBuff := new AccumBuff(0..width-1, 0..height-1);
    g_spp.all := 0;

    for y in 0 .. height - 1 loop
      for x in 0 .. width - 1 loop
        g_accBuff(x,y) := (0.0, 0.0, 0.0);
      end loop;
    end loop;

  end ResizeViewport;

  function GetSPP return integer is
  begin

   if anti_aliasing_on then
     return g_spp.all*4;
   else
     return g_spp.all;
   end if;

  end GetSPP;


end Ray_Tracer;
