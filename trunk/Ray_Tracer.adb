with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Materials;
with Ada.Exceptions;
with Ray_Tracer.Intersections;
with Ray_Tracer.Integrators;

use Ada.Numerics;
use Ada.Text_IO;
use Materials;
use Ray_Tracer.Integrators;

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

    epsilon := max(abs(hit_pos.x), abs(hit_pos.y), abs(hit_pos.z))*0.000000001;
    epsilon := max(epsilon, 1.0e-30);

    shadowRay.direction := normalize(lpos-hit_pos);
    shadowRay.origin    := hit_pos + shadowRay.direction*epsilon;

    res.shadowRay := shadowRay;

    h := Intersections.FindClosestHit(shadowRay);

    maxDist  := length(hit_pos - lpos);
    epsilon2 := max(maxDist*0.000001, 1.0e-30);

    if h.is_hit and then (h.t < maxDist-epsilon2 and h.t > 10.0*epsilon) then
      res.in_shadow        := true;
      res.percentageCloser := (1.0, 1.0, 1.0);
    else
      res.in_shadow        := false;
      res.percentageCloser := (0.0, 0.0, 0.0);
    end if;

    return res;

  end ComputeShadow;






  -- multithread stuff
  --
  task body Path_Trace_Thread is
    colBuff : AccumBuffRef;
    mygen   : RandRef := new RandomGenerator;
    tracer  : Ray_Tracer.Integrators.IntegratorRef := null;
  begin

    colBuff := new AccumBuff(0..width-1, 0..height-1);
    Ada.Numerics.Float_Random.Reset(Gen => mygen.agen, Initiator => threadId*7 + threadId*threadId*13);

    -- select integrator
    --

    tracer := new SimplePathTracerLegacy;
    --tracer := new PathTracerWithShadowRays;
    --tracer := new PathTracerMIS;
    --tracer := new MLTCopyImage;
    --tracer := new MLTSimple;
    --tracer := new MLTKelmenSimple;

    tracer.gen := mygen; -- default simple generator
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
    --delete(mygen); mygen := null;

    exception

      when The_Error : others =>

      Put_Line("Error raised in the thread:");
      Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      Put_Line("");

      delete(colBuff);
      --delete(mygen);

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
    for i in 0 .. g_scn.materialsLegacy'Last loop
      if g_scn.materialsLegacy(i) = null then
        g_scn.materialsLegacy(i) := new LegacyMaterial;
      end if;
    end loop;

    -- glass material
    --
    g_scn.materialsLegacy(0).kd           := (0.0, 0.0, 0.0);
    g_scn.materialsLegacy(0).reflection   := (1.0,1.0,1.0);
    g_scn.materialsLegacy(0).roughness    := 0.25;
    g_scn.materialsLegacy(0).transparency := (1.0,1.0,1.0);
    g_scn.materialsLegacy(0).ior          := 1.75;
    g_scn.materialsLegacy(0).fresnel      := true;


    -- floor material
    --
    g_scn.materialsLegacy(1).ka := (0.0, 0.0, 0.0);
    g_scn.materialsLegacy(1).kd := (0.5, 0.5, 0.5);

    -- Left wall
    --
    g_scn.materialsLegacy(2).ka := (0.0, 0.0, 0.0);
    g_scn.materialsLegacy(2).kd := (0.25, 0.5, 0.0); --0.25 0.65 0.0

    -- Right wall
    --
    g_scn.materialsLegacy(3).ka := (0.0, 0.0, 0.0);
    g_scn.materialsLegacy(3).kd := (0.5, 0.0, 0.0);

    -- Light material
    --
    g_scn.materialsLegacy(4).kd     := (0.0, 0.0, 0.0);
    g_scn.materialsLegacy(4).ka     := (20.0, 20.0, 20.0);
    g_light.intensity         := g_scn.materialsLegacy(4).ka;
    g_light.surfaceArea       := (g_light.boxMax.x - g_light.boxMin.x)*(g_light.boxMax.z - g_light.boxMin.z);

    -- Mirror
    --
    g_scn.materialsLegacy(5).kd           := (0.0, 0.0, 0.0);
    g_scn.materialsLegacy(5).ks           := (0.5,0.5,0.5);
    g_scn.materialsLegacy(5).reflection   := (0.5,0.5,0.5);
    g_scn.materialsLegacy(5).roughness    := 0.25;
    g_scn.materialsLegacy(5).transparency := (0.0,0.0,0.0);
    g_scn.materialsLegacy(5).ior          := 1.5;

    -- Glass 2
    --
    g_scn.materialsLegacy(6).kd := (0.0, 0.0, 0.0);
    g_scn.materialsLegacy(6).ks := (0.0, 0.0, 0.0);
    g_scn.materialsLegacy(6).reflection := (0.0,0.0,0.0);
    g_scn.materialsLegacy(6).roughness  := 0.5;
    g_scn.materialsLegacy(6).transparency := (0.85,0.85,0.85);
    g_scn.materialsLegacy(6).ior := 1.75;

    -- blue
    --
    g_scn.materialsLegacy(7).ka := (0.0, 0.0, 0.0);
    g_scn.materialsLegacy(7).kd := (0.0, 0.0, 0.5);


    -- init spheres geometry
    --
    if g_scn.spheres /= null then
      delete(g_scn.spheres);
    end if;

    g_scn.spheres := new Spheres_Array(0..1);


    -- new api materials
    -- this should cause memry leak, but i'm too lazy to implement memory management for this case
    --
    declare
      lmatRef   : MaterialAreaLightRef := new MaterialAreaLight;

      dmatWhite : MaterialLambertRef   := new MaterialLambert;
      dmatRed   : MaterialLambertRef   := new MaterialLambert;
      dmatGreen : MaterialLambertRef   := new MaterialLambert;

      smatMirr  : MaterialMirrorRef    := new MaterialMirror;
      smatPhong : MaterialPhongRef     := new MaterialPhong;
      smatGlass : MaterialFresnelDielectricRef := new MaterialFresnelDielectric;

    begin

      lmatRef.emission    := (20.0, 20.0, 20.0);
      g_light.intensity   := lmatRef.emission;
      g_light.surfaceArea := (g_light.boxMax.x - g_light.boxMin.x)*(g_light.boxMax.z - g_light.boxMin.z);

      dmatWhite.kd := (0.5, 0.5, 0.5);
      dmatRed.kd   := (0.5, 0.0, 0.0);
      dmatGreen.kd := (0.25, 0.5, 0.0);

      smatMirr.reflection  := (0.75, 0.75, 0.75);
      smatPhong.reflection := (0.75, 0.75, 0.75);
      smatPhong.cosPower   := 80.0;

      smatGlass.reflection   := (0.95, 0.95, 0.95);
      smatGlass.transparency := (0.75, 0.75, 0.75);
      smatGlass.ior          := 1.75;

      g_scn.materials(0) := MaterialRef(smatGlass);
      g_scn.materials(1) := MaterialRef(dmatWhite);
      g_scn.materials(2) := MaterialRef(dmatRed);
      g_scn.materials(3) := MaterialRef(dmatGreen);
      g_scn.materials(4) := MaterialRef(lmatRef);
      g_scn.materials(5) := MaterialRef(smatMirr);
      g_scn.materials(8) := MaterialRef(smatPhong);
      g_scn.materials(9) := MaterialRef(dmatWhite);
      g_scn.materials(10):= MaterialRef(dmatWhite);

      g_scn.spheres(0).mat := MaterialRef(smatMirr);
      g_scn.spheres(1).mat := MaterialRef(smatGlass);

    end;

    --materials(0) :=

    g_scn.spheres(0).pos := (-1.5,1.0,1.5);
    g_scn.spheres(0).r   := 1.0;
    g_scn.spheres(0).matLeg := g_scn.materialsLegacy(5); -- 5, 3

    g_scn.spheres(1).pos := (1.4,1.0,3.0);
    g_scn.spheres(1).r   := 1.0;
    g_scn.spheres(1).matLeg := g_scn.materialsLegacy(0); -- 0, 1

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
