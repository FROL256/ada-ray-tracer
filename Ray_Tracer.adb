with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Materials;
with Ada.Exceptions;
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

    red   := Unsigned_32(r);
    green := Unsigned_32(g);
    blue  := Unsigned_32 (b);

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

    h := FindClosestHit(shadowRay);

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



  function FindClosestHit(r: Ray) return Hit is
    hits : array (1..4) of Hit;
    nearestHitIndex : integer range hits'First..hits'Last := 1;
    nearestHitDist  : float := infinity;
  begin

    hits(1) := IntersectAllSpheres(r, g_scn.spheres);
    hits(2) := IntersectCornellBox(r, my_cornell_box);

    if GetShapeType(g_lightRef) = Light_Shape_Rect then
      hits(3) := IntersectFlatLight(r, g_light, g_scn.materials(4));
    end if;

    hits(4) := IntersectMeshBF(r, g_scn.mymesh);

    for i in hits'First .. hits'Last loop

      if hits(i).is_hit and hits(i).t < nearestHitDist then
        nearestHitIndex := i;
        nearestHitDist  := hits(i).t;
      end if;

    end loop;

    if hits(nearestHitIndex).mat = null then
      hits(nearestHitIndex).mat := g_scn.materials(hits(nearestHitIndex).matId);
    end if;

    return hits(nearestHitIndex);

  end FindClosestHit;



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

    tracer := new SimplePathTracer;
    --tracer := new PathTracerWithShadowRays;
    --tracer := new PathTracerMIS;

    tracer.gen := mygen; -- default simple generator
    tracer.Init;         -- Ada 2005 style virtual function call

    while true loop

      accept Resume;

      tracer.DoPass(colBuff); -- Ada 2005 style virtual function call

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

    -- init spheres geometry
    --
    if g_scn.spheres /= null then
      delete(g_scn.spheres);
    end if;

    g_scn.spheres := new Spheres_Array(0..1);

    -- create lights
    --
    declare

     boxMin : float3 := (-0.75, 4.98, 1.25);
     boxMax : float3 := ( 0.75, 4.98, 3.25);
     normal : float3 := (0.0, -1.0, 0.0);
     intensity : float3 := ( 20.0, 20.0, 20.0);

     flatLight : LightRef := new AreaLight'( boxMin      => boxMin,
                                             boxMax      => boxMax,
                                             normal      => normal,
                                             intensity   => intensity,
                                             surfaceArea => (boxMax.x - boxMin.x)*(boxMax.z - boxMin.z)
                                           );

     sphPos    : float3 := (0.0, 4.5, 1.0);
     sphRadius : float  := 0.5;

     sphLight : LightRef := new SphereLight'( center      => sphPos,
                                              radius      => sphRadius,
                                              intensity   => intensity*0.5,
                                              surfaceArea => 4.0*M_PI*sphRadius*sphRadius );

     oldSpheresNum : integer := g_scn.spheres'Size;

    begin

      --g_lightRef := flatLight;
      g_lightRef := sphLight;

     if GetShapeType(g_lightRef) = Light_Shape_Rect then                        -- change with virtual function call

        g_light.boxMin := boxMin; -- for geom code, this is temporary
        g_light.boxMax := boxMax;
        g_light.normal := normal;
        g_light.intensity := intensity;

     else

       delete(g_scn.spheres);
       g_scn.spheres := new Spheres_Array(0..2);

       g_scn.spheres(2).pos := sphPos;
       g_scn.spheres(2).r   := sphRadius;
       g_scn.spheres(2).mat := new MaterialLight'(lref => g_lightRef); -- ALERT: memory leak

     end if;

    end;

    -- create materials
    -- this should cause memry leak, but i'm too lazy to implement memory management for this case (need to implement abtract delete function)
    --
    declare

      lmatRef   : MaterialRef := new MaterialLight'(lref => g_lightRef);

      dmatWhite : MaterialRef := new MaterialLambert'(kd => (0.5, 0.5, 0.5));
      dmatRed   : MaterialRef := new MaterialLambert'(kd => (0.5, 0.0, 0.0));
      dmatGreen : MaterialRef := new MaterialLambert'(kd => (0.25, 0.5, 0.0));

      smatMirr  : MaterialRef := new MaterialMirror'(reflection => (0.75, 0.75, 0.75));
      smatPhong : MaterialRef := new MaterialPhong' (reflection => (0.75, 0.75, 0.75), cosPower => 80.0);

      smatGlass : MaterialRef := new MaterialFresnelDielectric'(reflection   => (0.75, 0.75, 0.75),
                                                                transparency => (0.85, 0.85, 0.85),
                                                                ior          =>  1.75);
    begin

      g_scn.materials(0) := smatGlass;
      g_scn.materials(1) := dmatWhite;
      g_scn.materials(2) := dmatGreen;
      g_scn.materials(3) := dmatRed;
      g_scn.materials(4) := lmatRef;
      g_scn.materials(5) := smatMirr;
      g_scn.materials(8) := smatPhong;
      g_scn.materials(9) := dmatWhite;
      g_scn.materials(10):= dmatWhite;

      g_scn.spheres(0).mat := smatPhong; -- smatMirr
      g_scn.spheres(1).mat := smatGlass;

    end;


    g_scn.spheres(0).pos := (-1.5,1.0,1.5);
    g_scn.spheres(0).r   := 1.0;

    g_scn.spheres(1).pos := (1.4,1.0,3.0);
    g_scn.spheres(1).r   := 1.0;

    declare
      mrot   : float4x4 := RotationMatrix(-PI/6.0, (0.0, 1.0, 0.0));
      mscale : float4x4 := IdentityMatrix;
      mtans  : float4x4 := IdentityMatrix;
    begin

      SetCol(mtans, 3, (-0.75, 0.1, 3.1, 1.0));

      mscale(0,0) := 2.0;
      mscale(1,1) := 2.0;
      mscale(2,2) := 2.0;

      LoadMeshFromVSGF(g_scn.mymesh, mtans*mrot*mscale, "../data/pyramid.vsgf");

    end;

    -- setup camera
    --
    g_cam.pos    := (0.0, 2.55, 12.5);
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
