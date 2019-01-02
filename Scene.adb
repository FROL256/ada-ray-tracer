with Interfaces;
with Ada.Streams.Stream_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Vector_Math;
with Geometry;


use Interfaces;
use Ada.Streams.Stream_IO;
use Vector_Math;
use Geometry;

package body Scene is

  procedure Init   (a_scn : in out Render_Scene; a_path : in String) is
  begin
    null;
  end Init;

  procedure Destroy(a_scn : in out Render_Scene) is
  begin
    null;
  end Destroy;


  function  Material_At(id : in Integer; a_scn : in Render_Scene) return Materials.MaterialRef is
  begin
    return null;
  end  Material_At;

  function  Light_At   (id : in Integer; a_scn : in Render_Scene) return Lights.LightRef is
  begin
    return null;
  end Light_At;


  function Find_Closest_Hit(r : in Geometry.Ray; a_scn : in Render_Scene) return Geometry.Hit is
    hits            : array (1..4) of Geometry.Hit;
    nearestHitIndex : integer range hits'First..hits'Last := 1;
    nearestHitDist  : float := infinity;
  begin

    --hits(1) := IntersectAllSpheres(r, g_scn.spheres);
    --hits(2) := IntersectCornellBox(r, my_cornell_box);
    --
    --if GetShapeType(g_lightRef) = Light_Shape_Rect then
    --  hits(3) := IntersectFlatLight(r, g_light, g_scn.materials(4));
    --end if;
    --
    --hits(4) := IntersectMeshBF(r, g_scn.mymesh);
    --
    --for i in hits'First .. hits'Last loop
    --
    --  if hits(i).is_hit and hits(i).t < nearestHitDist then
    --    nearestHitIndex := i;
    --    nearestHitDist  := hits(i).t;
    --  end if;
    --
    --end loop;
    --
    --if hits(nearestHitIndex).mat = null then
    --  hits(nearestHitIndex).mat := g_scn.materials(hits(nearestHitIndex).matId);
    --end if;
    --
    --return hits(nearestHitIndex);

    return hits(1);

  end Find_Closest_Hit;


end Scene;
