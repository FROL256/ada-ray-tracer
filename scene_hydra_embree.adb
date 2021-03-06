with Interfaces;
with Interfaces.C;
with Ada.Streams.Stream_IO;
with Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Vector_Math;
with Geometry;
with Lights;
with Materials;
with Pugi_Xml;
with System;


use Interfaces;
use Ada.Streams.Stream_IO;
use Ada.Numerics;
use Ada.Strings.Unbounded;

use Vector_Math;
use Geometry;
use Lights;
use Materials;
use Pugi_Xml;
use System;


package body Scene is

  package Float_IO is new Ada.Text_IO.Float_IO(float);

  ------------------------------------------------------------------------------ begin import extern CPP code

  procedure gcore_init_and_clear;
  pragma Import(C, gcore_init_and_clear, "gcore_init_and_clear");

  procedure gcore_destroy;
  pragma Import(C, gcore_destroy, "gcore_destroy");

  function gcore_add_mesh_3f(a_vertices4f : Address; a_vertexNum  : Integer;
                             a_indices    : Address; a_indicesNum : Integer) return Integer;
  pragma Import(C, gcore_add_mesh_3f, "gcore_add_mesh_3f");

  procedure gcore_instance_meshes(a_geomId : Integer; a_matrices : Address; a_matrixNum : Integer);
  pragma Import(C, gcore_instance_meshes, "gcore_instance_meshes");

  procedure gcore_commit_scene;
  pragma Import(C, gcore_commit_scene, "gcore_commit_scene");

  type Myfloat2 is array (0 .. 2) of float;
  type Myfloat3 is array (0 .. 3) of float;

  type HitCpp is record
    primIndex : integer := -1;
    geomIndex : integer := -1;
    instIndex : integer := -1;
    t         : float   := 0.0;
    normal    : Myfloat3;
    texCoord  : Myfloat2;
  end record;

  function gcore_closest_hit(a_rayPos : Address; a_rayDir : Address; t_near : float; t_far : float; pHit : Address) return Interfaces.C.char;
  pragma Import(C, gcore_closest_hit, "gcore_closest_hit");

  ------------------------------------------------------------------------------ end   import extern CPP code

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
      LoadMeshFromVSGF(result(i), IdentityMatrix, To_String(loc));

      i    := i + 1;
      chld := chld.next;

    end loop;

  end Load_Meshes;


  function Read_Float3_From_String(str : String) return float3 is
    arr  : array (0 .. 2) of float := (0.0, 0.0, 0.0);
    coord, i, j : Integer := str'First;
  begin

    for coord in 0 .. 2 loop

      while j <= str'Last and then str(j) /= ' ' loop -- find next space
        j := j + 1;
      end loop;

      arr(coord) := Float'Value(str(i..j-1));

      while j <= str'Last and then str(j) = ' ' loop -- now skip all spaces
        j := j + 1;
      end loop;

      i := j;

      if i = str'Last then
        exit;
      end if;

    end loop;

    return (arr(0), arr(1), arr(2));

  end Read_Float3_From_String;

  type float16 is array (0 .. 15) of float;

  function Read_Float16_From_String(str : String) return float16 is
    arr  : float16;
    coord, i, j : Integer := str'First;
  begin

    for coord in 0 .. 15 loop

      while j <= str'Last and then str(j) /= ' ' loop -- find next space
        j := j + 1;
      end loop;

      arr(coord) := Float'Value(str(i..j-1));

      while j <= str'Last and then str(j) = ' ' loop -- now skip all spaces
        j := j + 1;
      end loop;

      i := j;

      if i = str'Last then
        exit;
      end if;

    end loop;

    return arr;

  end Read_Float16_From_String;


  -- this function is a bit specific to hydra legacy format when value an be stoored in text or in attrib 'val'
  -- So it check node->attribute("val") first and if it is empty try to read float3 from node body/text
  --
  function Read_Float3_Val(a_node : in XML_Node) return float3 is
    attr_val : XML_Attribute;
  begin

    if a_node.Is_Null then
      return (0.0, 0.0, 0.0);
    else

      attr_val := a_node.attribute("val");

      if not attr_val.Is_Null then
        return Read_Float3_From_String(attr_val.value);
      else
        return Read_Float3_From_String(a_node.text);
      end if;

    end if;

  end Read_Float3_Val;


  function Create_Material_From_Node(a_mnode : in XML_Node) return MaterialRef is
    diff_color : float3;
  begin

    diff_color := Read_Float3_Val(a_mnode.child("diffuse").child("color"));

    Put("mat( "); Put(a_mnode.attribute("name").value); Put(") = ");
    Put(diff_color.x'Image); Put(" ");  Put(diff_color.y'Image); Put(" "); Put(diff_color.z'Image); Put_Line("");

    return null;
  end Create_Material_From_Node;

  procedure Load_Materials(a_lib : in XML_Node; result : out Materials_Array) is
     i    : Integer := 0;
     chld : XML_Node;
  begin

    chld := a_lib.first_child;
    while not chld.Is_Null loop
      result(i) := Create_Material_From_Node(chld);
      i         := i + 1;
      chld      := chld.next;
    end loop;

  end Load_Materials;


  procedure Load_Textures(a_node : in XML_Node; a_folder : in String) is
     i    : Integer  := 0;
     node : XML_Node := a_node.first_child;
  begin

    while not node.Is_Null loop

      declare
        Name     : String  := node.attribute("name").value;
        Loc      : String  := node.attribute("loc").value;
        offset   : Natural := Natural(node.attribute("offset").as_uint);
        bytesize : Natural := Natural(node.attribute("bytesize").as_uint);
      begin
        Put("texture with name: ");
        Put(Name);
        Put(" offset:");
        Put(Natural'Image(offset));
        Put(" bytesize:");
        Put_Line(Natural'Image(bytesize));
      end;

      node := node.Next;

    end loop;

  end Load_Textures;

  -----------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------

  procedure Add_Meshes_To_GCore(a_scn : in out Render_Scene) is
    geomId  : Integer := 0;
  begin

    -- pass all meshes to geometry core
    --
    for meshId in a_scn.meshes'First ..  a_scn.meshes'Last loop

      if a_scn.meshes(meshId).triangles'Size /= 0 then

        geomId := gcore_add_mesh_3f(a_scn.meshes(meshId).vert_positions(0)'Address, a_scn.meshes(meshId).vert_positions'Size,
                                    a_scn.meshes(meshId).triangles(0)'Address,     (a_scn.meshes(meshId).triangles'Size)*3);

        Put("geomId = "); Put_Line(geomId'Image);
      end if;

    end loop;

  end Add_Meshes_To_GCore;


  procedure Instance_All_Meshes(a_scn : in out Render_Scene; scnlib : XML_Node) is
    node : XML_Node := scnlib.child("instance");
  begin

     Put_Line("");
     while not node.Is_Null loop

       declare
         matrix : float16 := Read_Float16_From_String(node.attribute("matrix").value);
         meshId : Integer := node.attribute("mesh_id").as_int;
       begin
         --Put("matrix = "); Put_Line(node.attribute("matrix").value);
         for i in 0 .. 15 loop
           Put(matrix(i)'Image);
           Put(" ");
         end loop;
         Put_Line("");
         gcore_instance_meshes(meshId, matrix(0)'Address, 1);
       end;

       node := node.Next;

     end loop;

  end Instance_All_Meshes;

  -----------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------

  procedure Init(a_scn : in out Render_Scene; a_path : in String) is

    xmlFileName : Unbounded_String;

    Document:   XML_Document;
    Result:     XML_Parse_Result;
    root:       XML_Node := Document.Root;
    attr:       XML_Attribute;

    numMat, numLights, numMeshes : Natural;

  begin

    --Put("c/cpp: test_add(2,3) = "); Put_Line(test_add(2,3)'Image);

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

      delete(a_scn.meshes);
      delete(a_scn.materials); -- #TODO: delete each material ref ...

      a_scn.meshes    := new Mesh_Array     (0 .. numMeshes - 1);
      a_scn.materials := new Materials_Array(0 .. numMat    - 1);

                                                                     Put_Line("");
      Load_Meshes   (geolib, a_path, result => a_scn.meshes.all);    Put_Line("");
      Load_Materials(matlib,         result => a_scn.materials.all); Put_Line("");
      Load_Textures (texlib, a_path);                                Put_Line(""); -- #TODO: implement texture load

      -- now put all meshes inside embree
      --

      pragma Assert(a_scn.meshes.all'Size    > 0);
      pragma Assert(a_scn.materials.all'Size > 0);

      -- pragma Assert(not matlib.Is_Null);

      gcore_init_and_clear;

      Add_Meshes_To_GCore(a_scn);

      Instance_All_Meshes(a_scn, scnlib.child("scene"));

      gcore_commit_scene;

   end;

  end Init;

  procedure Destroy(a_scn : in out Render_Scene) is
  begin

    gcore_destroy;

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
    hit     : Geometry.Hit;
    hit_cpp : HitCpp;
    hitRes  : Interfaces.C.char;
  begin

    hitRes := gcore_closest_hit(r.origin.x'Address, r.direction.x'Address, 0.0, 100000.0,
                                hit_cpp'Address);

    hit.is_hit   := (hit_cpp.primIndex /= -1); --(hitRes /= 0);
    hit.t        := hit_cpp.t;
    hit.normal.x := hit_cpp.normal(0);
    hit.normal.y := hit_cpp.normal(1);
    hit.normal.z := hit_cpp.normal(2);
    hit.tx       := hit_cpp.texCoord(0);
    hit.ty       := hit_cpp.texCoord(1);
    hit.matId    := -1;

    return hit;

  end Find_Closest_Hit;


end Scene;
