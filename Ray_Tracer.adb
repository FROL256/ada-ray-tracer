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
    -- max_val : float;
   begin

    --max_val := max(v.x, max(v.y, v.z));
    --if max_val > 0.0 then
    --
    --  if max_val > 1.0 then
    --    return (v.x/max_val, v.y/max_val, v.z/max_val);
    --  else
    --    return (v.x, v.y, v.z);
    --  end if;
    --
    --else
    --  return (0.0, 0.0, 0.0);
    --end if;

    return (min(v.x,1.0), min(v.y,1.0), min(v.z,1.0));

  end;


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


  procedure Generate4RayDirections (x, y : in Natural; res : out RayDirPack) is
    fov : float := Pi / (2.0);
  begin

    res(0).x := float(x) + 1.0/3.0 - (float(width) / 2.0);
    res(0).y := float(y) + 1.0/3.0 - (float(height) / 2.0);
    res(0).z := -float (width) / safe_tan (fov / 2.0);

    res(1).x := float(x) + 1.0/3.0 - (float(width) / 2.0);
    res(1).y := float(y) + 2.0/3.0 - (float(height) / 2.0);
    res(1).z := -float (width) / safe_tan (fov / 2.0);

    res(2).x := float(x) + 2.0/3.0 - (float(width) / 2.0);
    res(2).y := float(y) + 1.0/3.0 - (float(height) / 2.0);
    res(2).z := -float (width) / safe_tan (fov / 2.0);

    res(3).x := float(x) + 2.0/3.0 - (float(width) / 2.0);
    res(3).y := float(y) + 2.0/3.0 - (float(height) / 2.0);
    res(3).z := -float (width) / safe_tan (fov / 2.0);

    res(0) := normalize(res(0));
    res(1) := normalize(res(1));
    res(2) := normalize(res(2));
    res(3) := normalize(res(3));

  end;


  function Compute_Shadow(hit_pos : float3; lpos : float3) return Shadow_Hit is
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

    h := Find_Closest_Hit(shadowRay);

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

  end Compute_Shadow;

  function Find_Closest_Hit(r: Ray) return Hit is
  begin
    return Scene.Find_Closest_Hit(g_scn, r);
  end Find_Closest_Hit;


  -- multithread stuff
  --
  task body Path_Trace_Thread is
    mygen   : RandRef := new RandomGenerator;
    tracer  : Ray_Tracer.Integrators.IntegratorRef := null;
  begin

    Ada.Numerics.Float_Random.Reset(Gen => mygen.agen, Initiator => threadId*7 + threadId*threadId*13);

    -- select integrator
    --

    --tracer := new SimplePathTracer;
    --tracer := new PathTracerWithShadowRays;
    tracer := new PathTracerMIS;

    tracer.gen := mygen; -- default simple generator
    tracer.Init;         -- Ada 2005 style virtual function call

    while true loop

      accept Resume;

      tracer.DoPass(Acc_Buff); -- Ada 2005 style virtual function call

      accept Finish (spp : IntRef) do
      begin
        if Anti_Aliasing_On then
          spp.all := spp.all + 4;
        else
          spp.all := spp.all + 1;
        end if;

      end;

      end Finish;

    end loop;


    exception

      when The_Error : others =>

      Put_Line("Error raised in the thread:");
      Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      Put_Line("");

      --delete(mygen);

  end Path_Trace_Thread;

  procedure Render_Pass is
    rgb   : float3;
    normC : float;
  begin

    if not g_threadsCreated then                    -- run only once!
      for i in 0..Threads_Num-1 loop
        g_threads(i) := new Path_Trace_Thread(i+1, g_accBuff);
      end loop;
      g_threadsCreated := true;
    end if;

    for i in 0..Threads_Num-1 loop
      g_threads(i).Resume;
    end loop;

    for i in 0..Threads_Num-1 loop
      g_threads(i).Finish(g_spp);
    end loop;

    -- Get acumulated image to LDR screen
    --
    normC := 1.0/float(g_spp.all);

    for y in 0 .. height - 1 loop
      for x in 0 .. width - 1 loop
         rgb   := g_accBuff(x,y)*normC;
         rgb.x := rgb.x ** (1.0/g_gamma); -- gamma correction
         rgb.y := rgb.y ** (1.0/g_gamma);
         rgb.z := rgb.z ** (1.0/g_gamma);
         screen_buffer(x,y) := ColorToUnsigned_32(ToneMapping((rgb.x, rgb.y, rgb.z)));
      end loop;
    end loop;

  end Render_Pass;



  procedure Resize_Viewport(size_x, size_y : integer) is

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

  end Resize_Viewport;

  function GetSPP return integer is
  begin
    return g_spp.all;
  end GetSPP;


end Ray_Tracer;
