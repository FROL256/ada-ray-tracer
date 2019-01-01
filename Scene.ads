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
--use Geometry;
--use Materials;
--use Lights;
use Ada.Text_IO;


package Scene is

  type Render_Scene is private;

  procedure Init   (a_scn : in out Render_Scene; a_path : in String);
  procedure Destroy(a_scn : in out Render_Scene);

  function  Find_Closest_Hit(r : in Geometry.Ray; a_scn : in Render_Scene) return Geometry.Hit;

  function  Material_At(id : in Integer; a_scn : in Render_Scene) return MaterialRef;
  function  Light_At   (id : in Integer; a_scn : in Render_Scene) return LightRef;

end Scene;
