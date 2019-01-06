with Interfaces;
with Ada.Numerics.Float_Random;
with Vector_Math;
with Geometry;
with Materials;
with Lights;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

use Interfaces;
use Vector_Math;
use Ada.Text_IO;


package Scene is

  type Render_Scene is private;

  procedure Init   (a_scn : in out Render_Scene; a_path : in String);
  procedure Destroy(a_scn : in out Render_Scene);

  function  Find_Closest_Hit(a_scn : in Render_Scene; r : in Geometry.Ray) return Geometry.Hit;

  function  Material_At(a_scn : in Render_Scene; id : in Integer) return Materials.MaterialRef;
  function  Light_At   (a_scn : in Render_Scene; id : in Integer) return Lights.LightRef;

  type Camera is record
    pos    : float3;
    lookAt : float3;
    up     : float3;
    matrix : float4x4;
  end record;

  function Camera_At(a_scn : in Render_Scene; id : in Integer) return Camera;

  pragma Inline(Material_At);
  pragma Inline(Light_At);
  pragma Inline(Camera_At);

private

  type Sphere is record
    pos : float3;
    r   : float;
    mat : Materials.MaterialRef;
  end record;

  type AABB is record
    min : float3;
    max : float3;
  end record;

  type Materials_Array is array (Natural range <>) of Materials.MaterialRef;
  type Materials_Array_Ptr is access Materials_Array;

  --type Lights_Array    is array (0 .. 1)  of Lights.Light;

  type Mesh_Array is array (Natural range <>) of Geometry.Mesh;
  type Mesh_Array_Ptr is access Mesh_Array;

  type Render_Scene is record
    meshes    : Mesh_Array_Ptr      := null;
    materials : Materials_Array_Ptr := null;
    --lights    : Lights_Array;
    spheres   : Geometry.Spheres_Array_Ptr;

    mymesh     : Geometry.Mesh;

    g_light    : Geometry.FlatLight;
    g_lightRef : Lights.LightRef := null;
    g_cam      : Camera;

  end record;

  my_cornell_box : Geometry.CornellBox :=
  (
    mat_indices => (2,3,1,1,8,1),
    normals     => ((1.0,0.0,0.0),   (-1.0,0.0,0.0), (0.0,1.0,0.0), (0.0,-1.0,0.0), (0.0,0.0,1.0), (0.0,0.0,-1.0)),
    box         => ((-2.5, 0.0, 0.0),( 2.5, 5.0, 5.0))
  );

  type FloatBuff    is array (Integer range <>, integer range <>) of float;
  type FloatBuffRef is access FloatBuff;

  procedure delete is new Ada.Unchecked_Deallocation(Object => FloatBuff, Name => FloatBuffRef);
  procedure delete is new Ada.Unchecked_Deallocation(Object => Geometry.Spheres_Array, Name => Geometry.Spheres_Array_Ptr);

  procedure delete is new Ada.Unchecked_Deallocation(Object => Mesh_Array, Name => Mesh_Array_Ptr);
  procedure delete is new Ada.Unchecked_Deallocation(Object => Materials_Array, Name => Materials_Array_Ptr);

end Scene;
