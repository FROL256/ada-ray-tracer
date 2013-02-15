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

  private

    procedure ComputeFlatNormals(self: in out Mesh);
    procedure AllocData(self: in out Mesh; vnum,inum: integer);
    procedure FreeData(self: in out Mesh);

end Geometry;
