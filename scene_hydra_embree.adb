with Interfaces;
with Ada.Streams.Stream_IO;
with Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Unbounded;

with Vector_Math;
with Geometry;
with Lights;
with Materials;
with Pugi_Xml;

use Interfaces;
use Ada.Streams.Stream_IO;
use Ada.Numerics;
use Ada.Strings.Unbounded;

use Vector_Math;
use Geometry;
use Lights;
use Materials;
use Pugi_Xml;

package body Scene is

  function test_add(a : Integer; b : Integer) return Integer;
  pragma Import(C, test_add, "test_add");


  function Count_Childs(node : in XML_Node) return Integer is
    childNum : Integer := 0;
    chld     : XML_Node;
  begin

    chld := node.first_child;

    while not chld.Is_Null loop
      childNum := childNum + 1;
      chld     := chld.next;
    end loop;

    return childNum;

  end Count_Childs;

  procedure Load_Meshes(a_lib : in XML_Node; a_folder : in String; result : out Mesh_Array) is
    i    : Integer := 0;
    chld : XML_Node;
    loc  : Unbounded_String;
  begin

    chld := a_lib.first_child;

    while not chld.Is_Null loop

      loc := To_Unbounded_String( a_folder & "/" & chld.attribute("loc").value );
      Geometry.LoadMeshFromVSGF(result(i), IdentityMatrix, To_String(loc));

      i    := i + 1;
      chld := chld.next;

    end loop;

  end Load_Meshes;

  procedure Init(a_scn : in out Render_Scene; a_path : in String) is

    xmlFileName : Unbounded_String;

    Document:   XML_Document;
    Result:     XML_Parse_Result;
    root:       XML_Node := Document.Root;
    attr:       XML_Attribute;

    numMat, numLights, numMeshes : Natural;

  begin

    Put("c/cpp: test_add(2,3) = "); Put_Line(test_add(2,3)'Image);

    xmlFileName := To_Unbounded_String(a_path & "/statex_00001.xml");

    Document.Load(To_String(xmlFileName), Result => Result);

    if not Result.OK then
      Put(Result.Description);
      Put(": xml parse status ");
      Put(XML_Parse_Status'Image(Result.Status));
      Put(" at offset ");
      Put_Line(Natural'Image(Result.Offset));
    end if;
    pragma Assert(Result.OK = True);

    Put_Line("[scene]: XML load success.");

    root := Document.Root;

    declare
      texlib, matlib, lgtlib, geolib : XML_Node;
      camlib, setlib, scnlib, node   : XML_Node;
    begin

      texlib := root.child("textures_lib");
      matlib := root.child("materials_lib");
      lgtlib := root.child("lights_lib");
      camlib := root.child("cam_lib");
      geolib := root.child("geometry_lib");
      setlib := root.child("render_lib");
      scnlib := root.child("scenes");

      pragma Assert(not matlib.Is_Null);
      pragma Assert(not lgtlib.Is_Null);
      pragma Assert(not geolib.Is_Null);
      pragma Assert(not scnlib.Is_Null);

      numMat    := Count_Childs(matlib);
      numLights := Count_Childs(lgtlib);
      numMeshes := Count_Childs(geolib);

      Put("[scene]: num(meshes   ) = "); Put_Line(numMeshes'Image);
      Put("[scene]: num(lights   ) = "); Put_Line(numLights'Image);
      Put("[scene]: num(materials) = "); Put_Line(numMat'Image);

      if a_scn.meshes /= null then
        delete(a_scn.meshes);
        a_scn.meshes := null;
      end if;

      a_scn.meshes := new Mesh_Array(0 .. numMeshes-1);
      Load_Meshes(geolib, a_path, result => a_scn.meshes.all);

      node := texlib.child("texture");
      while not node.Is_Null loop

        declare
          Name     : String  := node.attribute("name").value;
          Loc      : String  := node.attribute("loc").value;
          offset   : Natural := Natural(node.attribute("offset").as_uint);
          bytesize : Natural := Natural(node.attribute("bytesize").as_uint);
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

    if a_scn.meshes /= null then
      delete(a_scn.meshes);
      a_scn.meshes := null;
    end if ;

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
