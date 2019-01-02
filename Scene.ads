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

  function  Find_Closest_Hit(r : in Geometry.Ray; a_scn : in Render_Scene) return Geometry.Hit;

  function  Material_At(id : in Integer; a_scn : in Render_Scene) return Materials.MaterialRef;
  function  Light_At   (id : in Integer; a_scn : in Render_Scene) return Lights.LightRef;

  type Camera is record
    pos    : float3;
    lookAt : float3;
    up     : float3;
    matrix : float4x4;
  end record;

  function Get_Camera(a_scn : in Render_Scene) return Camera;

private

  type Materials_Array is array (0 .. 10) of Materials.MaterialRef;
  --type Lights_Array    is array (0 .. 1)  of Lights.Light;


  type Render_Scene is record
    materials : Materials_Array;
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

end Scene;
