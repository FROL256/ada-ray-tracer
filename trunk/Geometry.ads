with Interfaces;
with Ada.Numerics.Float_Random;
with Vector_Math;
with Materials;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

use Interfaces;
use Vector_Math;
use Materials;
use Ada.Text_IO;

package Geometry is

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

  type Triangle is record
    A_index : integer;
    B_index : integer;
    C_index : integer;
  end record;

  type FlatLight is record
    boxMin    : float3;
    boxMax    : float3;
    normal    : float3;
    intensity : float3;
    surfaceArea : float;
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
    mat    : MaterialRef := null;   -- both mat id and mat ref are needed, but each in it's time while calc intersection
    matId  : integer := 0;          -- both mat id and mat ref are needed, but each in it's time while calc intersection
    tx,ty  : float   := 0.0;
    prim_index : integer := -1;

  end record;

  type Shadow_Hit is record
    percentageCloser : float3  := (0.0, 0.0, 0.0);
    in_shadow        : boolean := true;
    shadowRay        : Ray;
  end record;


  type Spheres_Array is array (Integer range <>) of Sphere;
  type Spheres_Array_Ptr is access Spheres_Array;

  type Float3_Array      is array(integer range <>) of float3;
  type Float2_Array      is array(integer range <>) of float2;
  type Triangle_Array    is array(integer range <>) of Triangle;
  type MaterialsId_Array is array(integer range <>) of integer;

  type Float3_Array_Ptr is access Float3_Array;
  type Float2_Array_Ptr is access Float2_Array;
  type Triangle_Array_Ptr is access Triangle_Array;
  type MaterialsId_Ptr is access MaterialsId_Array;

  procedure delete is new Ada.Unchecked_Deallocation(Object => Float3_Array, Name => Float3_Array_Ptr);
  procedure delete is new Ada.Unchecked_Deallocation(Object => Float2_Array, Name => Float2_Array_Ptr);
  procedure delete is new Ada.Unchecked_Deallocation(Object => Triangle_Array, Name => Triangle_Array_Ptr);
  procedure delete is new Ada.Unchecked_Deallocation(Object => MaterialsId_Array, Name => MaterialsId_Ptr);

  type Mesh is record
    vert_positions  : Float3_Array_Ptr   := null;
    vert_normals    : Float3_Array_Ptr   := null;
    vert_tex_coords : Float2_Array_Ptr   := null;
    triangles       : Triangle_Array_Ptr := null;
    material_ids    : MaterialsId_Ptr    := null;
  end record;

  procedure CreatePrism(self: out Mesh; mTransform : in float4x4; size,angle : in float; matId : in integer);


  function IntersectPlaneXZ   (r: Ray; planeMat : MaterialRef) return Hit;
  function IntersectAllSpheres(r: Ray; a_spheres : Spheres_Array_Ptr) return Hit;
  function IntersectFlatLight (r: Ray; lightGeom : FlatLight; lMat : MaterialRef) return Hit;
  function IntersectCornellBox(r: Ray; boxData : CornellBox) return Hit;


  private

    null_hit : Hit := ( prim_type  => Plane_TypeId,
                        prim_index => -1,
	      	        is_hit     => false,
	      	        t          => infinity,
                        mat        => null,
                        matId      => 0,
	                tx         => 0.0,
                        ty         => 0.0,
                        normal     => (0.0, 1.0, 0.0)
	             );


    procedure ComputeFlatNormals(self: in out Mesh);
    procedure AllocData(self: in out Mesh; vnum,inum: integer);
    procedure FreeData(self: in out Mesh);

end Geometry;
