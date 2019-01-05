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

  procedure Init_Cornell_Box(a_scn : in out Render_Scene);

  procedure Init(a_scn : in out Render_Scene; a_path : in String) is
  begin
    Init_Cornell_Box(a_scn);
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
    hits            : array (1..4) of Geometry.Hit;
    nearestHitIndex : integer range hits'First..hits'Last := 1;
    nearestHitDist  : float := infinity;
  begin

    hits(1) := IntersectAllSpheres(r, a_scn.spheres);
    hits(2) := IntersectCornellBox(r, my_cornell_box);

    if GetShapeType(a_scn.g_lightRef) = Light_Shape_Rect then
      hits(3) := IntersectFlatLight(r, a_scn.g_light, a_scn.materials(4));
    end if;

    hits(4) := IntersectMeshBF(r, a_scn.mymesh);

    for i in hits'First .. hits'Last loop

      if hits(i).is_hit and hits(i).t < nearestHitDist then
        nearestHitIndex := i;
        nearestHitDist  := hits(i).t;
      end if;

    end loop;

    if hits(nearestHitIndex).mat = null then
      hits(nearestHitIndex).mat := a_scn.materials(hits(nearestHitIndex).matId);
    end if;

    return hits(nearestHitIndex);

  end Find_Closest_Hit;


  procedure Init_Cornell_Box(a_scn : in out Render_Scene) is
  begin

    -- init spheres geometry
    --
    if a_scn.spheres /= null then
      delete(a_scn.spheres);
    end if;

    a_scn.spheres := new Spheres_Array(0..1);

    -- create lights
    --
    declare

     boxMin    : float3 := (-0.75, 4.98, 1.25);
     boxMax    : float3 := ( 0.75, 4.98, 3.25);
     normal    : float3 := (0.0, -1.0, 0.0);
     intensity : float3 := ( 20.0, 20.0, 20.0);

     flatLight : LightRef := new AreaLight'( boxMin      => boxMin,
                                             boxMax      => boxMax,
                                             normal      => normal,
                                             intensity   => intensity,
                                             surfaceArea => (boxMax.x - boxMin.x)*(boxMax.z - boxMin.z)
                                           );

     sphPos    : float3 := (0.0, 4.5, 1.0);
     sphRadius : float  := 0.5;

     sphLight : LightRef := new SphereLight'( center      => sphPos,
                                              radius      => sphRadius,
                                              intensity   => intensity*0.5,
                                              surfaceArea => 4.0*M_PI*sphRadius*sphRadius );

     oldSpheresNum : integer := a_scn.spheres'Size;

    begin

     a_scn.g_lightRef := sphLight;

     if GetShapeType(a_scn.g_lightRef) = Light_Shape_Rect then                   -- change with virtual function call

        a_scn.g_light.boxMin := boxMin;                                          -- for geom code, this is temporary
        a_scn.g_light.boxMax := boxMax;
        a_scn.g_light.normal := normal;
        a_scn.g_light.intensity := intensity;

     else

       delete(a_scn.spheres);
       a_scn.spheres := new Spheres_Array(0..2);

       a_scn.spheres(2).pos := sphPos;
       a_scn.spheres(2).r   := sphRadius;
       a_scn.spheres(2).mat := new MaterialLight'(lref => a_scn.g_lightRef);     -- ALERT: memory leak

     end if;

    end;

    -- create materials
    -- this should cause memry leak, but i'm too lazy to implement memory management for this case (need to implement abtract delete function)
    --
    declare

      lmatRef   : MaterialRef := new MaterialLight'(lref => a_scn.g_lightRef);

      dmatWhite : MaterialRef := new MaterialLambert'(kd => (0.5, 0.5, 0.5));
      dmatRed   : MaterialRef := new MaterialLambert'(kd => (0.5, 0.0, 0.0));
      dmatGreen : MaterialRef := new MaterialLambert'(kd => (0.25, 0.5, 0.0));

      smatMirr  : MaterialRef := new MaterialMirror'(reflection => (0.75, 0.75, 0.75));
      smatPhong : MaterialRef := new MaterialPhong' (reflection => (0.75, 0.75, 0.75), cosPower => 80.0);

      smatGlass : MaterialRef := new MaterialFresnelDielectric'(reflection   => (0.75, 0.75, 0.75),
                                                                transparency => (0.85, 0.85, 0.85),
                                                                ior          =>  1.75);
    begin

      a_scn.materials(0) := smatGlass;
      a_scn.materials(1) := dmatWhite;
      a_scn.materials(2) := dmatGreen;
      a_scn.materials(3) := dmatRed;
      a_scn.materials(4) := lmatRef;
      a_scn.materials(5) := smatMirr;
      a_scn.materials(8) := smatPhong;
      a_scn.materials(9) := dmatWhite;
      a_scn.materials(10):= dmatWhite;

      a_scn.spheres(0).mat := smatPhong; -- smatMirr
      a_scn.spheres(1).mat := smatGlass;

    end;


    a_scn.spheres(0).pos := (-1.5,1.0,1.5);
    a_scn.spheres(0).r   := 1.0;

    a_scn.spheres(1).pos := (1.4,1.0,3.0);
    a_scn.spheres(1).r   := 1.0;

    declare
      mrot   : float4x4 := RotationMatrix(-PI/6.0, (0.0, 1.0, 0.0));
      mscale : float4x4 := IdentityMatrix;
      mtans  : float4x4 := IdentityMatrix;
    begin

      SetCol(mtans, 3, (-0.75, 0.1, 3.1, 1.0));

      mscale(0,0) := 2.0;
      mscale(1,1) := 2.0;
      mscale(2,2) := 2.0;

      LoadMeshFromVSGF(a_scn.mymesh, mtans*mrot*mscale, "../data/pyramid2.vsgf");

    end;

    -- setup camera
    --
    a_scn.g_cam.pos    := (0.0, 2.55, 12.5);
    a_scn.g_cam.lookAt := (0.0, 0.0, 0.0);
    a_scn.g_cam.up     := (0.0, 1.0, 0.0);
    a_scn.g_cam.matrix := IdentityMatrix;

  end Init_Cornell_Box;


end Scene;
