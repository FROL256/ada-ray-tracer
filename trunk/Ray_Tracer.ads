with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Assertions;
with Vector_Math;
with Materials;
with Ada.Unchecked_Deallocation;


use Interfaces;
use Vector_Math;
use Materials;
use Ada.Assertions;

package Ray_Tracer is

  width  : Positive := 800;
  height : Positive := 600;
  threads_num : Positive := 8;

  compute_shadows  : boolean  := true;
  anti_aliasing_on : boolean  := true;
  max_depth        : Positive := 8;

  background_color : float3   := (0.0,0.0,0.0);


  type ScreenBufferData is array(integer range <>, integer range <>) of Unsigned_32;
  type ScreenBufferDataRef is access ScreenBufferData;
  screen_buffer : ScreenBufferDataRef := null;


  procedure MultiThreadedRayTracing;
  procedure MultiThreadedPathTracing;

  procedure ResizeViewport(size_x,size_y : integer);
  procedure InitCornellBoxScene;

  type UpdateScreenCallBack is access procedure(save: Boolean);

  updateScreen : UpdateScreenCallBack;

  function GetSPP return integer;

private

  procedure delete is new Ada.Unchecked_Deallocation(Object => ScreenBufferData, Name => ScreenBufferDataRef);

  type Color is record
    Red   : float range 0.0..1.0;
    Green : float range 0.0..1.0;
    Blue  : float range 0.0..1.0;
  end record;

  type Hit;

  type RandomGenerator is null record;
  type RandRef is access all RandomGenerator;

  agen : Ada.Numerics.Float_Random.Generator;
  dummyRef : RandRef;

  function rnd_uniform(gen : RandRef; l,h : float) return float;

  type Ray is record
    origin    : float3  := (0.0, 0.0, 0.0);
    direction : float3  := (0.0, 0.0, 1.0);
    gen       : RandRef := null;
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

  type Light is record
    pos   : float3;
    color : float3;
  end record;

  type FlatLight is record
    boxMin    : float3;
    boxMax    : float3;
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
    mat    : MaterialRef := null;
    x,y    : float := 0.0;
    prim_index : integer := -1;

  end record;

  type Shadow_Hit is record
    percentageCloser : float3  := (0.0, 0.0, 0.0);
    in_shadow        : boolean := true;
  end record;


  type Spheres_Array is array (Integer range <>) of Sphere;
  type Spheres_Array_Ptr is access Spheres_Array;
  procedure delete is new Ada.Unchecked_Deallocation(Object => Spheres_Array, Name => Spheres_Array_Ptr);

  function ColorToUnsigned_32(c: Color) return Unsigned_32;
  function ToneMapping(v : float3) return Color;

  pragma Inline (ColorToUnsigned_32);
  pragma Inline (ToneMapping);

  function EyeRayDirection (x, y : Natural) return float3;
  function ComputeShadow(hit_pos : float3; lpos : float3) return Shadow_Hit;
  function RayTrace (r : Ray; recursion_level : Integer) return float3;

  function Shade (in_ray : Ray; h : Hit; a_light : light) return float3;


  type RayDirPack is array (0 ..  3) of float3;

  procedure Generate4RayDirections (x, y : in Natural; arr : out RayDirPack);

  -- general rendering procedures and tasks
  --
  task type Ray_Trace_Thread(yBegin,yEnd : integer) is
    entry Finish;
  end Ray_Trace_Thread;

  type Ray_Trace_Thread_Ptr is access Ray_Trace_Thread;

  type AccumBuff is array (Integer range <>, integer range <>) of float3;

  type AccumBuffRef is access AccumBuff;
  procedure delete is new Ada.Unchecked_Deallocation(Object => AccumBuff, Name => AccumBuffRef);

  type IntRef is access integer;
  procedure delete is new Ada.Unchecked_Deallocation(Object => integer, Name => IntRef);

  task type Path_Trace_Thread is
    entry Resume;
    entry Finish(accBuff : AccumBuffRef; spp : IntRef);
  end Path_Trace_Thread;

  type Path_Trace_Thread_Ptr is access Path_Trace_Thread;

  g_threads : array(0..threads_num-1) of Path_Trace_Thread_Ptr;
  g_threadsCreated : boolean := false;


  type PathResult is record
    color    : float3  := (0.0, 0.0, 0.0);
    hitLight : boolean := false;
  end record;

  -- integrators
  --
  type Integrator is abstract tagged null record;
  type IntegratorRef is access Integrator'Class;

  function PathTrace(self : Integrator; r : Ray; recursion_level : Integer) return PathResult is abstract;


  type SimplePathTracer is new Integrator with null record;
  function PathTrace(self : SimplePathTracer; r : Ray; recursion_level : Integer) return PathResult;


  type PathTracerWithShadowRays is new Integrator with null record;
  function PathTrace(self : PathTracerWithShadowRays; r : Ray; recursion_level : Integer) return PathResult;

  type PathTracerMIS is new Integrator with null record;
  function PathTrace(self : PathTracerMIS; r : Ray; recursion_level : Integer) return PathResult;


  g_integrator : IntegratorRef := null;

  -- scene
  --

  type Materials_Array is array (0 .. 10) of MaterialRef;
  type Lights_Array is array (0 .. 1) of Light;

  type Scene is record

    materials : Materials_Array;
    lights    : Lights_Array;
    spheres   : Spheres_Array_Ptr;

  end record;

  g_scn : Scene;
  g_cam : Camera;

  g_accBuff : AccumBuffRef := null;
  g_spp     : IntRef := null;

  random_gen : Ada.Numerics.Float_Random.Generator;

  my_cornell_box : CornellBox :=
  (
    mat_indices => (2,3,1,1,1,1),
    normals => ((1.0,0.0,0.0), (-1.0,0.0,0.0), (0.0,1.0,0.0), (0.0,-1.0,0.0), (0.0,0.0,1.0), (0.0,0.0,-1.0)),
    box => ((-2.5, 0.0, 0.0),( 2.5, 5.0, 5.0))
  );


  g_light : FlatLight :=
  (
    boxMin    => (-0.75, 4.98, 1.25),
    boxMax    => ( 0.75, 4.98, 3.25),
    intensity => (10.0, 10.0, 10.0),
    surfaceArea => 1.0
  );

  null_hit : Hit := ( prim_type => Plane_TypeId,
                      prim_index => -1,
	      	      is_hit    => false,
	      	      t         => infinity,
	      	      mat       => null,
	              x         => 0.0,
                      y         => 0.0,
                      normal    => (0.0, 1.0, 0.0)
	             );

end Ray_Tracer;


