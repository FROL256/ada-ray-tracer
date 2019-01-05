with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Assertions;
with Vector_Math;
with Materials;
with Lights;
with Geometry;
with Scene;
with Ada.Unchecked_Deallocation;

use Interfaces;
use Vector_Math;
use Materials;
use Lights;
use Ada.Assertions;
use Geometry;

package Ray_Tracer is

  width  : Positive := 800;
  height : Positive := 600;

  Threads_Num      : Positive := 8;
  Anti_Aliasing_On : boolean  := true;
  Max_Trace_Depth  : Positive := 8;

  Background_Color : float3   := (0.0,0.0,0.0);

  G_Epsilon     : constant float := 1.0e-5;  -- small value for geometry offsets
  G_Epsilon_Div : constant float := 1.0e-20; -- small value for bsdf/pdf divisions

  g_gamma       : constant float := 2.0;
  g_scn         : Scene.Render_Scene;

  type ScreenBufferData    is array(integer range <>, integer range <>) of Unsigned_32;
  type ScreenBufferDataRef is access ScreenBufferData;
  screen_buffer : ScreenBufferDataRef := null;


  type Render_Type is (RT_DEBUG, RT_WHITTED, PT_STUPID, PT_SHADOW, PT_MIS); -- render type

  procedure Init_Render(a_rendType : Render_Type);

  procedure Render_Pass;
  procedure Resize_Viewport(size_x,size_y : integer);

  function GetSPP   return integer;
  function Finished return Boolean;

private

  --
  --
  type AccumBuff    is array (Integer range <>, integer range <>) of float3;
  type AccumBuffRef is access AccumBuff;

  -- pragma Atomic_Components(AccumBuff); -- atomic access to component of "AccumBuff" cannot be guaranteed

  type FloatBuff    is array (Integer range <>, integer range <>) of float;
  type FloatBuffRef is access FloatBuff;

  type Color is record
    Red   : float range 0.0..1.0;
    Green : float range 0.0..1.0;
    Blue  : float range 0.0..1.0;
  end record;


  function ColorToUnsigned_32(c: Color) return Unsigned_32;
  function ToneMapping(v : float3) return Color;

  pragma Inline (ColorToUnsigned_32);
  pragma Inline (ToneMapping);

  function EyeRayDirection (x, y : Natural) return float3;
  function Compute_Shadow(hit_pos : float3; lpos : float3) return Shadow_Hit;

  type RayDirPack is array (0 ..  3) of float3;
  procedure Generate4RayDirections (x, y : in Natural; res : out RayDirPack);

  type IntRef is access integer;

  -- multithreaded rendering stuff
  --
  task type Path_Trace_Thread(threadId : integer; Acc_Buff : AccumBuffRef) is
    entry Resume;
    entry Finish(spp : IntRef);
  end Path_Trace_Thread;

  type Path_Trace_Thread_Ptr is access Path_Trace_Thread;

  g_threads        : array(0..Threads_Num-1) of Path_Trace_Thread_Ptr;
  g_threadsCreated : boolean := false;

  ---- instantiate deallocation procedures
  --
  procedure delete is new Ada.Unchecked_Deallocation(Object => ScreenBufferData, Name => ScreenBufferDataRef);
  --procedure delete is new Ada.Unchecked_Deallocation(Object => RandomGenerator, Name => RandRef);
  procedure delete is new Ada.Unchecked_Deallocation(Object => integer, Name => IntRef);
  procedure delete is new Ada.Unchecked_Deallocation(Object => AccumBuff, Name => AccumBuffRef);
  procedure delete is new Ada.Unchecked_Deallocation(Object => Path_Trace_Thread, Name => Path_Trace_Thread_Ptr);

  function Find_Closest_Hit(r: Ray) return Hit;
  pragma Inline(Find_Closest_Hit);

  g_accBuff   : AccumBuffRef := null;
  g_spp       : IntRef       := null;
  g_rend_type : Render_Type  := PT_MIS;
  g_finish    : Boolean      := false;

end Ray_Tracer;


