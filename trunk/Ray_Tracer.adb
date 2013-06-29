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

    epsilon := max(abs(hit_pos.x), abs(hit_pos.y), abs(hit_pos.z))*0.0001;
    epsilon := max(epsilon, 0.0001);

    shadowRay.direction := normalize(lpos-hit_pos);
    shadowRay.origin    := hit_pos + shadowRay.direction*epsilon;

    h := Intersections.FindClosestHit(shadowRay);

    maxDist  := length(hit_pos - lpos);
    epsilon2 := max(maxDist*0.001, 0.0001);

    if h.is_hit and then (h.t < maxDist-epsilon2 and h.t > 10.0*epsilon) then
      res.in_shadow        := true;
      res.percentageCloser := (1.0, 1.0, 1.0);
    else
      res.in_shadow        := false;
      res.percentageCloser := (0.0, 0.0, 0.0);
    end if;

    return res;

  end ComputeShadow;

   function GetAttenuation(l_pos : in float3; p : in float3) return float is
    d : float;
  begin
    d := length(l_pos-p);
    return 1.0/(1.0 + d*0.01 + d*d*0.001);
    --return 1.0;
  end GetAttenuation;

  function Shade (in_ray : Ray; h : Hit; a_light : light) return float3 is
      l,v,hit_pos,S : float3;
      NormalDotLight: float;
  begin

    hit_pos := in_ray.origin + h.t*in_ray.direction;
    l := normalize(a_light.pos - hit_pos);
    v := normalize(in_ray.origin - hit_pos);
    NormalDotLight := dot(h.normal, l);

    S := EvalCookTorranceBRDF(h.mat.all, l, v, h.normal);
    return a_light.color*(h.mat.ka + S*GetAttenuation(a_light.pos, hit_pos));

  end Shade;


  -- Whitted Ray Tracing
  --
  function RayTrace (r : Ray; recursion_level : Integer) return float3 is
    res_color : float3 := background_color;
    h : Hit;
    hit_pos : float3;
    refl, trans : float3;
    ior : float;
  begin

    if recursion_level = 0 then
      return res_color;
    end if;

    h := Intersections.FindClosestHit(r);

    if not h.is_hit then
      return res_color;
    elsif IsLight(h.mat) then
      return Emittance(h.mat);
    end if;

    hit_pos := (r.origin + r.direction*h.t);

    if compute_shadows then
      if not ComputeShadow(hit_pos, g_scn.lights(0).pos).in_shadow then
        res_color := res_color + Shade(r,h, g_scn.lights(0));
      end if;
    else
      res_color := res_color + Shade(r,h, g_scn.lights(0));
    end if;

    refl  := h.mat.reflection;
    trans := h.mat.transparency;

    if h.mat.fresnel then
      ApplyFresnel(h.mat, dot(r.direction, h.normal), refl, trans);
    end if;

    ior := h.mat.ior;

    if length(trans) > 0.0 then

      if not TotalInternalReflection(ior, r.direction, h.normal) then

	declare
          refractedRay : Ray;
          sign : float := 1.0;
        begin

          if dot(h.normal, r.direction) > 0.0 then
            sign := -1.0;
            --ior  := 1.0/ior;
          end if;

	  refract(ior, r.direction, h.normal, wt => refractedRay.direction);

          refractedRay.origin := hit_pos - 0.00001*sign*h.normal;
          res_color := res_color + trans*RayTrace(refractedRay, recursion_level-1);
        end;
     end if;
    end if;

    if length(refl) > 0.0 then

      declare
        reflectedRay : Ray;
      begin

        reflectedRay.direction := reflect(r.direction, h.normal);
        reflectedRay.origin    := hit_pos + 0.00001*h.normal;
        res_color := res_color + refl*RayTrace(reflectedRay, recursion_level-1);

      end;
    end if;

    return res_color;

  end RayTrace;


  task body Ray_Trace_Thread is
    r : Ray;
    rayDirs : RayDirPack;
    color : float3;
  begin

    for y in yBegin .. yEnd loop
      for x in 0 .. width - 1 loop

	r.origin := g_cam.pos;

        if anti_aliasing_on then

	  color := background_color;
          Generate4RayDirections(x,y,rayDirs);

          for i in 0 .. 3 loop
            r.direction := normalize(g_cam.matrix*rayDirs(i));
            color := color + RayTrace(r,max_depth);
          end loop;

          screen_buffer(x,y) := ColorToUnsigned_32(ToneMapping(color*0.25));

	else

	  r.direction := EyeRayDirection(x,y);
          r.direction := normalize(g_cam.matrix*r.direction);
	  screen_buffer(x,y) := ColorToUnsigned_32(ToneMapping(RayTrace(r,max_depth)));

	end if;

      end loop;
    end loop;

    accept Finish;

    exception
      when The_Error : others =>
        Put_Line("Error raised in the thread:");
        Put_Line(Ada.Exceptions.Exception_Name(The_Error));
        Put_Line(Ada.Exceptions.Exception_Message(The_Error));
	Put_Line("");

  end Ray_Trace_Thread;

  procedure MultiThreadedRayTracing is

    threads : array (0..threads_num) of Ray_Trace_Thread_Ptr;
    stepY : integer := height/threads_num;

  begin

    Put("ray trace threads num: ");
    Put_Line(integer'Image(threads_num));

    for i in 0..threads_num-1 loop
      threads(i) := new Ray_Trace_Thread(i*stepY, (i+1)*stepY-1);
    end loop;

    for i in 0..threads_num-1 loop
      threads(i).Finish;
    end loop;

  end MultiThreadedRayTracing;

  -- end of Whitted Ray Tracing


  -- Monte-Carlo Path Tracing
  --
  function rnd_uniform(gen : RandRef; l,h : float) return float is
    t: float := 0.0;
  begin
    t := Ada.Numerics.Float_Random.Random(Gen => agen);
    return l + (h-l)*t;
  end rnd_uniform;


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
    r1 : float := rnd_uniform(gen, 0.0, 1.0);
    r2 : float := rnd_uniform(gen, 0.0, 1.0);
  begin
    return MapSampleToCosineDist(r1,r2,norm,norm,1.0);
  end RandomCosineVectorOf;



  -- very basic path tracing
  --
  function PathTrace(self : SimplePathTracer; r : Ray; recursion_level : Integer) return PathResult is
    res_color : float3 := (0.0, 0.0, 0.0);
    hit_pos,bxdf,refl,trans : float3;
    h : Hit;
    nextRay : Ray;
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
      nextRay.origin    := hit_pos + 0.00001*h.normal;
      bxdf              := h.mat.reflection;
    elsif h.mat.fresnel then -- reflection or transmittion

      refl  := h.mat.reflection;
      trans := h.mat.transparency;
      ApplyFresnel(h.mat, dot(r.direction, h.normal), refl, trans);

      ksitrans := length(trans) / (length(refl) + length(trans));
      ksirefl  := length(refl) / (length(refl) + length(trans));

      ksi := rnd_uniform(r.gen, 0.0, 1.0);

      if ksi > ksitrans then
        nextRay.direction := reflect(r.direction, h.normal);
        nextRay.origin    := hit_pos + 0.00001*h.normal;
        bxdf              := refl*(1.0/ksirefl);
      else
        bxdf              := trans*(1.0/ksitrans);
        if not TotalInternalReflection(h.mat.ior, r.direction, h.normal) then
          if dot(h.normal, r.direction) > 0.0 then
            sign := -1.0;
          end if;
	  refract(h.mat.ior, r.direction, h.normal, wt => nextRay.direction);
          nextRay.origin := hit_pos - 0.00001*sign*h.normal;
        else
          nextRay.direction := reflect(r.direction, h.normal);
          nextRay.origin    := hit_pos + 0.00001*h.normal;
        end if;

      end if;

    else  -- diffuse reflection
      nextRay.direction := RandomCosineVectorOf(r.gen, h.normal);
      nextRay.origin    := hit_pos + 0.00001*h.normal;
      bxdf := h.mat.kd;
    end if;

    return (bxdf*self.PathTrace(nextRay, recursion_level-1).color, false);

  end PathTrace;


  function LightSample(gen : RandRef; lightGeom : FlatLight) return float3 is
    r1 : float := rnd_uniform(gen, 0.0, 1.0);
    r2 : float := rnd_uniform(gen, 0.0, 1.0);
    x,y,z : float;
  begin
    x := lightGeom.boxMin.x + r1*(lightGeom.boxMax.x - lightGeom.boxMin.x);
    y := lightGeom.boxMin.y;
    z := lightGeom.boxMin.z + r2*(lightGeom.boxMax.z - lightGeom.boxMin.z);
    return (x, y, z);
  end LightSample;


  -- path tracing with shadow rays
  --
  function PathTrace(self : PathTracerWithShadowRays; r : Ray; recursion_level : Integer) return PathResult is
    res_color : float3 := (0.0, 0.0, 0.0);
    hit_pos,bxdf,refl,trans : float3;
    h : Hit;
    nextRay : Ray;
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
      lpos : float3 := LightSample(r.gen, g_light);
      r    : float  := length(hit_pos - lpos);
      sdir : float3 := normalize(lpos - hit_pos);
      cos_theta1 : float := max(dot(sdir, h.normal), 0.0);
      cos_theta2 : float := max(dot(sdir,(0.0,1.0,0.0)), 0.0);
      impP : float  := g_light.surfaceArea*cos_theta1*cos_theta2/(3.1415926535*r*r + 1.0e-5);
    begin

      if not ComputeShadow(hit_pos, lpos).in_shadow then
        res_color := res_color + (h.mat.kd*g_light.intensity)*impP;
      end if;

    end;


    -- pick up next ray
    --
    if length(h.mat.reflection) > 0.0 and not h.mat.fresnel then -- specular reflection
      nextRay.direction := reflect(r.direction, h.normal);
      nextRay.origin    := hit_pos + 0.00001*h.normal;
      bxdf              := h.mat.reflection;
    elsif h.mat.fresnel then -- reflection or transmittion

      refl  := h.mat.reflection;
      trans := h.mat.transparency;
      ApplyFresnel(h.mat, dot(r.direction, h.normal), refl, trans);

      ksitrans := length(trans) / (length(refl) + length(trans));
      ksirefl  := length(refl) / (length(refl) + length(trans));

      ksi := rnd_uniform(r.gen, 0.0, 1.0);

      if ksi > ksitrans then
        nextRay.direction := reflect(r.direction, h.normal);
        nextRay.origin    := hit_pos + 0.00001*h.normal;
        bxdf              := refl*(1.0/ksirefl);
      else
        bxdf              := trans*(1.0/ksitrans);
        if not TotalInternalReflection(h.mat.ior, r.direction, h.normal) then
          if dot(h.normal, r.direction) > 0.0 then
            sign := -1.0;
          end if;
	  refract(h.mat.ior, r.direction, h.normal, wt => nextRay.direction);
          nextRay.origin := hit_pos - 0.00001*sign*h.normal;
        else
          nextRay.direction := reflect(r.direction, h.normal);
          nextRay.origin    := hit_pos + 0.00001*h.normal;
        end if;

      end if;

    else  -- diffuse reflection
      nextRay.direction := RandomCosineVectorOf(r.gen, h.normal);
      nextRay.origin    := hit_pos + 0.00001*h.normal;
      bxdf := h.mat.kd;
    end if;

    return (res_color + bxdf*self.PathTrace(nextRay, recursion_level-1).color, false);

  end PathTrace;




  -- path tracing with multiple importance sampling
  --
  function PathTrace(self : PathTracerMIS; r : Ray; recursion_level : Integer) return PathResult is
    res_color : float3 := (0.0, 0.0, 0.0);
    hit_pos,bxdf,refl,trans : float3;
    h : Hit;
    nextRay : Ray;
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
      lpos : float3 := LightSample(r.gen, g_light);
      r    : float  := length(hit_pos - lpos);
      sdir : float3 := normalize(lpos - hit_pos);
      cos_theta1 : float := max(dot(sdir, h.normal), 0.0);
      cos_theta2 : float := max(dot(sdir,(0.0,1.0,0.0)), 0.0);
      impP : float  := g_light.surfaceArea*cos_theta1*cos_theta2/(3.1415926535*r*r + 1.0e-5);
    begin

      if not ComputeShadow(hit_pos, lpos).in_shadow then
        explicitColor := (h.mat.kd*g_light.intensity)*impP;
      end if;

    end;


    -- pick up next ray
    --
    if length(h.mat.reflection) > 0.0 and not h.mat.fresnel then -- specular reflection
      nextRay.direction := reflect(r.direction, h.normal);
      nextRay.origin    := hit_pos + 0.00001*h.normal;
      bxdf              := h.mat.reflection;
      specularBounce    := true;
    elsif h.mat.fresnel then -- reflection or transmittion

      refl  := h.mat.reflection;
      trans := h.mat.transparency;
      ApplyFresnel(h.mat, dot(r.direction, h.normal), refl, trans);

      ksitrans := length(trans) / (length(refl) + length(trans));
      ksirefl  := length(refl) / (length(refl) + length(trans));

      ksi := rnd_uniform(r.gen, 0.0, 1.0);

      if ksi > ksitrans then
        nextRay.direction := reflect(r.direction, h.normal);
        nextRay.origin    := hit_pos + 0.00001*h.normal;
        bxdf              := refl*(1.0/ksirefl);
      else
        bxdf              := trans*(1.0/ksitrans);
        if not TotalInternalReflection(h.mat.ior, r.direction, h.normal) then
          if dot(h.normal, r.direction) > 0.0 then
            sign := -1.0;
          end if;
	  refract(h.mat.ior, r.direction, h.normal, wt => nextRay.direction);
          nextRay.origin := hit_pos - 0.00001*sign*h.normal;
        else
          nextRay.direction := reflect(r.direction, h.normal);
          nextRay.origin    := hit_pos + 0.00001*h.normal;
        end if;

      end if;

      specularBounce    := true;

    else  -- diffuse reflection
      nextRay.direction := RandomCosineVectorOf(r.gen, h.normal);
      nextRay.origin    := hit_pos + 0.00001*h.normal;
      bxdf              := h.mat.kd;
      specularBounce    := false;
    end if;

    ret := self.PathTrace(nextRay, recursion_level-1);
    implicitColor := bxdf*ret.color;

    if ret.hitLight then
      pi := 1.0/g_light.surfaceArea;
      pe := 3.1415926535/max(dot(nextRay.direction, h.normal), 1.0e-5);

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






  -- multithread stuff
  --
  task body Path_Trace_Thread is
    r : Ray;
    rayDirs : RayDirPack;
    color : float3;
    colBuff : AccumBuffRef;
  begin

    colBuff := new AccumBuff(0..width-1, 0..height-1);

    while true loop

    accept Resume;

    for y in 0 .. height - 1 loop
      for x in 0 .. width - 1 loop

	r.origin := g_cam.pos;

        if anti_aliasing_on then

	  color := background_color;
          Generate4RayDirections(x,y,rayDirs);

          for i in 0 .. 3 loop
            r.direction := normalize(g_cam.matrix*rayDirs(i));
            color := color + g_integrator.PathTrace(r,max_depth).color;
          end loop;

          colBuff(x,y) := color*0.25;

	else

	  r.direction  := EyeRayDirection(x,y);
          r.direction  := normalize(g_cam.matrix*r.direction);
	  colBuff(x,y) := g_integrator.PathTrace(r,max_depth).color;

	end if;

      end loop;
    end loop;

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
            r := r ** (1.0/2.0); -- a sort of gamma correction
            g := g ** (1.0/2.0);
            b := b ** (1.0/2.0);
            screen_buffer(x,y) := ColorToUnsigned_32(ToneMapping((r,g,b)));
          end loop;
        end loop;

        spp.all := spp.all + 1;
      end;

    end Finish;

    end loop;

    delete(colBuff);

    exception
      when The_Error : others =>
        Put_Line("Error raised in the thread:");
        Put_Line(Ada.Exceptions.Exception_Name(The_Error));
        Put_Line(Ada.Exceptions.Exception_Message(The_Error));
	Put_Line("");
        delete(colBuff);

  end Path_Trace_Thread;

  procedure MultiThreadedPathTracing is
    --threads : array(0..threads_num-1) of Path_Trace_Thread_Ptr;
  begin

    if not g_threadsCreated then
      for i in 0..threads_num-1 loop
        g_threads(i) := new Path_Trace_Thread;
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

    Ada.Numerics.Float_Random.Reset(agen);

    --for i in 0..10 loop
    --  Put("rnd_unifom(0,1) = ");
    --  Put_Line(float'image(rnd_unifom(dummyRef, 0.0, 1.0)));
    --end loop;

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

    -- select integrator
    --
    --g_integrator := new SimplePathTracer;
    --g_integrator := new PathTracerWithShadowRays;
    g_integrator := new PathTracerMIS;

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
