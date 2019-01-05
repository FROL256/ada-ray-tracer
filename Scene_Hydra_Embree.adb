with Interfaces;
with Ada.Streams.Stream_IO;
with Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Vector_Math;
with Geometry;
with Lights;
with Materials;


use Interfaces;
use Ada.Streams.Stream_IO;
use Ada.Numerics;
use Vector_Math;
use Geometry;
use Lights;
use Materials;

package body Scene is

  procedure Init(a_scn : in out Render_Scene; a_path : in String) is
  begin
    null;
  end Init;

  procedure Destroy(a_scn : in out Render_Scene) is
  begin
    null;
  end Destroy;


  function  Material_At(a_scn : in Render_Scene; id : in Integer) return Materials.MaterialRef is
  begin
    if id < a_scn.materials'Size then
      return a_scn.materials(id);
    else
      return null;
    end if;
  end  Material_At;


  function  Light_At   (a_scn : in Render_Scene; id : in Integer) return Lights.LightRef is
  begin
    return a_scn.g_lightRef;
  end Light_At;

  function Camera_At(a_scn : in Render_Scene; id : in Integer) return Camera is
  begin
    return a_scn.g_cam;
  end Camera_At;


  function Find_Closest_Hit(a_scn : in Render_Scene; r : in Geometry.Ray) return Geometry.Hit is
    hit : Geometry.Hit;
  begin

    hit.is_hit := false;
    hit.matId  := -1;

    return hit;

  end Find_Closest_Hit;


end Scene;
