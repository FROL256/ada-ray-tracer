with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Assertions;
with Vector_Math;
with Materials;
with Geometry;
with Ada.Unchecked_Deallocation;


use Interfaces;
use Vector_Math;
use Materials;
use Geometry;
use Ada.Assertions;

package Ray_Tracer is

  width  : Positive := 800;
  height : Positive := 600;
  threads_num : Positive := 8;

  compute_shadows  : boolean  := true;
  anti_aliasing_on : boolean  := true;
  max_depth        : Positive := 10;

  background_color : float3   := (0.0,0.0,0.0);


  type ScreenBufferData is array(integer range <>, integer range <>) of Unsigned_32;
  type ScreenBufferDataRef is access ScreenBufferData;
  screen_buffer : ScreenBufferDataRef := null;


  procedure MultiThreadedRayTracing;
  procedure InitScene;
  procedure InitFractalScene;
  procedure ResizeViewport(size_x,size_y : integer);


private

  procedure delete is new Ada.Unchecked_Deallocation(Object => ScreenBufferData, Name => ScreenBufferDataRef);


  type Color is record
    Red   : float range 0.0..1.0;
    Green : float range 0.0..1.0;
    Blue  : float range 0.0..1.0;
  end record;

  type Hit;

  type Ray is record
    origin    : float3;
    direction : float3;
    --last_hit  : access constant Hit := null;
  end record;

  type Light is record
    pos   : float3;
    color : float3;
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

  type Primitive is (Plane_TypeId, Sphere_TypeId, Triangle_TypeId);

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

  procedure RaySphereIntersection(r : in ray; spos : in float3; radius : in float;
				  tmin: out float; is_hit : out boolean);

  function IntersectPlaneXZ(r: Ray) return Hit;
  function IntersectAllSpheres(r: Ray; a_spheres : Spheres_Array_Ptr) return Hit;
  function IntersectAllTriangles(r: Ray; a_triangles : Triangle_Array_Ptr; a_vert_positions : Float3_Array_Ptr) return Hit;
  function IntersectCornellBox(r: Ray; boxData : access constant CornellBox) return Hit;

  function FindClosestHit(r: Ray) return Hit;
  function ComputeShadow(hit_pos : float3; lpos : float3) return Shadow_Hit;
  function RayTrace (r : Ray; recursion_level : Integer) return float3;

  function Shade (in_ray : Ray; h : Hit; a_light : light) return float3;

  function GetCamMatrix(cam : Camera) return float4x4;

  type SpheresDirections is array(0 .. 5) of float3;

  procedure PushSphereInArrayRec(prevDirectionIndex, depth: integer; pos : float3; curr_size : float; directions : SpheresDirections);

  type RayDirPack is array (0 ..  3) of float3;

  procedure Generate4RayDirections (x, y : in Natural; arr : out RayDirPack);

  -- general rendering procedures and tasks
  --
  task type Ray_Trace_Thread(yBegin,yEnd : integer) is
    entry Finish;
  end Ray_Trace_Thread;

  type Ray_Trace_Thread_Ptr is access Ray_Trace_Thread;


  -- scene
  --

  type Materials_Array is array (0 .. 10) of MaterialRef;
  type Lights_Array is array (0 .. 1) of Light;

  type Scene is record

    materials : Materials_Array;
    lights    : Lights_Array;

    mesh1 : Mesh;
    spheres : Spheres_Array_Ptr;

    -- only for fractal generator
    --
    sph_top : integer;
    sph_top_max : integer;
    frac_sph_directions : SpheresDirections;

    floorMaterial : access MaterialWithMultiplyedTex;
    dielecric1    : access DielectricMaterial;

  end record;

  g_scn : Scene;
  g_cam : Camera;

  random_gen : Ada.Numerics.Float_Random.Generator;

  my_cornell_box : CornellBox :=
  (
    mat_indices => (0,1,2,3,4,5),
    normals => ((1.0,0.0,0.0), (-1.0,0.0,0.0), (0.0,1.0,0.0), (0.0,-1.0,0.0), (0.0,0.0,1.0), (0.0,0.0,-1.0)),
    box => ((-2.4, -1.0, 0.0),( 2.4, 3.0, 5.0))
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
