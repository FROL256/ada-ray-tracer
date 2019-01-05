with Interfaces;
with Ada.Streams.Stream_IO;
with Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Vector_Math;
with Geometry;
with Lights;
with Materials;
with Pugi_Xml;


use Interfaces;
use Ada.Streams.Stream_IO;
use Ada.Numerics;
use Vector_Math;
use Geometry;
use Lights;
use Materials;
use Pugi_Xml;

package body Scene is

  function test_add(a : Integer; b : Integer) return Integer;
  pragma Import(C, test_add, "test_add");


  procedure Init(a_scn : in out Render_Scene; a_path : in String) is
    Document:   XML_Document;
    Result:     XML_Parse_Result;
    root:       XML_Node := Document.Root;
    attr:       XML_Attribute;
  begin

    Put("c/cpp: test_add(2,3) = "); Put_Line(test_add(2,3)'Image);

    Document.Load(a_path, Result => Result);

    if not Result.OK then
      Put(Result.Description);
      Put(": xml parse status ");
      Put(XML_Parse_Status'Image(Result.Status));
      Put(" at offset ");
      Put_Line(Natural'Image(Result.Offset));
    end if;
    pragma Assert(Result.OK = True);

    Put_Line("XML load success");

    root := Document.Root;

    declare
      texlib, matlib, lgtlib, geolib : XML_Node;
      camlib, setlib, scnlib, node  : XML_Node;
    begin

      texlib := root.child("textures_lib");
      matlib := root.child("materials_lib");
      lgtlib := root.child("lights_lib");
      camlib := root.child("cam_lib");
      geolib := root.child("geometry_lib");
      setlib := root.child("render_lib");
      scnlib := root.child("scenes");


      node := texlib.child("texture");
      while not node.Is_Null loop

        declare
          Name     : String  := node.Attribute("name").Value;
          Loc      : String  := node.Attribute("loc").Value;
          offset   : Natural := Natural(node.Attribute("offset").As_Uint);
          bytesize : Natural := Natural(node.Attribute("bytesize").As_Uint);
        begin
          Put("  texture with name: ");
            Put(Name);
            Put(" offset:");
            Put(Natural'Image(offset));
            Put(" bytesize:");
            Put_Line(Natural'Image(bytesize));
        end;

        node := node.Next;

      end loop;


    end;

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
