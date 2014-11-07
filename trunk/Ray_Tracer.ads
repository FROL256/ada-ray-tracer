with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Assertions;
with Vector_Math;
with Materials;
with Lights;
with Ada.Unchecked_Deallocation;
limited private with Ray_Tracer.Integrators; -- just want to put Integrators to another package

use Interfaces;
use Vector_Math;
use Materials;
use Lights;
use Ada.Assertions;

package Ray_Tracer is

  width  : Positive := 800;
  height : Positive := 600;

  threads_num : Positive := 8;

  compute_shadows  : boolean  := true;
  anti_aliasing_on : boolean  := true;
  g_max_depth      : Positive := 8;

  background_color : float3   := (0.0,0.0,0.0);

  epsilon    : constant float := 1.0e-5;  -- small value for geometry offsets
  epsilonDiv : constant float := 1.0e-20; -- small value for bsdf/pdf divisions

  g_gamma    : float := 2.0;

  g_mltMutationsPerPixel : integer := 64;


  type ScreenBufferData is array(integer range <>, integer range <>) of Unsigned_32;
  type ScreenBufferDataRef is access ScreenBufferData;
  screen_buffer : ScreenBufferDataRef := null;


  procedure MultiThreadedPathTracing;

  procedure ResizeViewport(size_x,size_y : integer);
  procedure InitCornellBoxScene;

  function GetSPP return integer;


  type AccumBuff is array (Integer range <>, integer range <>) of float3;
  type AccumBuffRef is access AccumBuff;


  type FloatBuff is array (Integer range <>, integer range <>) of float;
  type FloatBuffRef is access FloatBuff;

  -- this is for test MLTCopyImage only
  --
  g_mltTestImage : AccumBuffRef := null;

private

  type Color is record
    Red   : float range 0.0..1.0;
    Green : float range 0.0..1.0;
    Blue  : float range 0.0..1.0;
  end record;

  type Hit;


  type Ray is record
    origin    : float3  := (0.0, 0.0, 0.0);
    direction : float3  := (0.0, 0.0, 1.0);
    x,y       : integer := 0;    -- needed for MLT
  end record;

  type Sphere is record
    pos : float3;
    r   : float;
    mat : MaterialRef;
  end record;

  type AABB is record
    min : float3;
    max : float3;
  end record;

  type FlatLight is record
    boxMin    : float3;
    boxMax    : float3;
    normal    : float3;
    intensity : float3;
    surfaceArea : float;
  end record;

  type Camera is record
    pos    : float3;
    lookAt : float3;
    up     : float3;
    matrix : float4x4;
  end record;

  type Cornell_Material_Indices is array (0..5) of integer;
  type Cornell_Normals is array (0..5) of float3;

  type CornellBox is record
    mat_indices : Cornell_Material_Indices;
    normals     : Cornell_Normals;
    box 	: AABB;
  end record;

  type Primitive is (Plane_TypeId, Sphere_TypeId, Triangle_TypeId, Quad_TypeId);

  type Hit(prim_type : Primitive := Plane_TypeId) is record

    is_hit : boolean := false;
    t      : float   := infinity;
    normal : float3  := (0.0, 0.0, 0.0);
    mat    : MaterialRef  := null;
    tx,ty  : float        := 0.0;
    prim_index : integer  := -1;

  end record;

  type Shadow_Hit is record
    percentageCloser : float3  := (0.0, 0.0, 0.0);
    in_shadow        : boolean := true;
    shadowRay        : Ray;
  end record;


  type Spheres_Array is array (Integer range <>) of Sphere;
  type Spheres_Array_Ptr is access Spheres_Array;

  function ColorToUnsigned_32(c: Color) return Unsigned_32;
  function ToneMapping(v : float3) return Color;
  function Luminance(c : float3) return float;

  pragma Inline (ColorToUnsigned_32);
  pragma Inline (ToneMapping);

  function EyeRayDirection (x, y : Natural) return float3;
  function ComputeShadow(hit_pos : float3; lpos : float3) return Shadow_Hit;

  type RayDirPack is array (0 ..  3) of float3;
  procedure Generate4RayDirections (x, y : in Natural; arr : out RayDirPack);

  type IntRef is access integer;



  -- multithreaded rendering stuff
  --
  task type Path_Trace_Thread(threadId : integer) is
    entry Resume;
    entry Finish(accBuff : AccumBuffRef; spp : IntRef);
  end Path_Trace_Thread;

  type Path_Trace_Thread_Ptr is access Path_Trace_Thread;

  g_threads : array(0..threads_num-1) of Path_Trace_Thread_Ptr;
  g_threadsCreated : boolean := false;

  ---- instantiate deallocation procedures
  --
  procedure delete is new Ada.Unchecked_Deallocation(Object => ScreenBufferData, Name => ScreenBufferDataRef);
  --procedure delete is new Ada.Unchecked_Deallocation(Object => RandomGenerator, Name => RandRef);
  procedure delete is new Ada.Unchecked_Deallocation(Object => integer, Name => IntRef);
  procedure delete is new Ada.Unchecked_Deallocation(Object => AccumBuff, Name => AccumBuffRef);
  procedure delete is new Ada.Unchecked_Deallocation(Object => FloatBuff, Name => FloatBuffRef);
  procedure delete is new Ada.Unchecked_Deallocation(Object => Spheres_Array, Name => Spheres_Array_Ptr);
  procedure delete is new Ada.Unchecked_Deallocation(Object => Path_Trace_Thread, Name => Path_Trace_Thread_Ptr);





  -- very lite scene description
  --

  type Materials_Array is array (0 .. 10) of MaterialRef;
  --type Lights_Array    is array (0 .. 1)  of Light;

  type Scene is record

    materials : Materials_Array;
    --lights    : Lights_Array;
    spheres   : Spheres_Array_Ptr;

  end record;

  g_scn : Scene;
  g_cam : Camera;

  g_accBuff : AccumBuffRef := null;
  g_spp     : IntRef := null;

  --random_gen : Ada.Numerics.Float_Random.Generator;

  my_cornell_box : CornellBox :=
  (
    mat_indices => (2,3,1,1,1,1),
    normals => ((1.0,0.0,0.0), (-1.0,0.0,0.0), (0.0,1.0,0.0), (0.0,-1.0,0.0), (0.0,0.0,1.0), (0.0,0.0,-1.0)),
    box => ((-2.5, 0.0, 0.0),( 2.5, 5.0, 5.0))
  );


  g_light : FlatLight;
  g_lightRef : LightRef := null;

end Ray_Tracer;

