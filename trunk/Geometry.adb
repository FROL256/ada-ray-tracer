with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Vector_Math;

use Interfaces;
use Vector_Math;


package body Geometry is


   procedure FreeData(self: in out Mesh) is
   begin

     if self.vert_positions /= null then
       delete(self.vert_positions);
       self.vert_positions := null;
     end if;

     if self.vert_normals /= null then
       delete(self.vert_normals);
       self.vert_normals := null;
     end if;

     if self.vert_tex_coords /= null then
       delete(self.vert_tex_coords);
       self.vert_tex_coords := null;
     end if;

     if self.triangles /= null then
       delete(self.triangles);
       self.triangles := null;
     end if;

     if self.material_ids /= null then
       delete(self.material_ids);
       self.material_ids := null;
     end if;

   end FreeData;

   procedure AllocData(self: in out Mesh; vnum,inum: integer) is
   begin
     self.vert_positions  := new Float3_Array(0..vnum-1);
     self.vert_normals    := new Float3_Array(0..vnum-1);
     self.vert_tex_coords := new Float2_Array(0..vnum-1);
     self.triangles       := new Triangle_Array(0..inum-1);
     self.material_ids    := new MaterialsId_Array(0..vnum-1);
   end AllocData;

   procedure ComputeFlatNormals(self: in out Mesh) is
     A,B,C,norm : float3;
   begin
     for tIndex in self.triangles'First .. self.triangles'Last loop

       A := self.vert_positions(self.triangles(tIndex).A_index);
       B := self.vert_positions(self.triangles(tIndex).B_index);
       C := self.vert_positions(self.triangles(tIndex).C_index);

       norm := normalize(cross(C-A,C-B));

       self.vert_normals(self.triangles(tIndex).A_index) := norm;
       self.vert_normals(self.triangles(tIndex).B_index) := norm;
       self.vert_normals(self.triangles(tIndex).C_index) := norm;

     end loop;
   end;


   procedure CreatePrism(self: out Mesh; mTransform : in float4x4; size,angle : in float; matId : in integer) is
     frontBound, backBound, leftBound, rightBound, h : float;
   begin

     FreeData(self);
     AllocData(self, 4*3 + 3*2, 3*2+2);

     frontBound := -size;
     backBound  :=  size;
     leftBound  := -size/2.0;
     rightBound :=  size/2.0;

     h := rightBound/safe_tan(angle/2.0);

     --Put("size  = "); Put_Line(float'Image(size));
     --Put("angle = "); Put_Line(float'Image(angle));
     --Put("h     = "); Put_Line(float'Image(h));
     --Put("lb    = "); Put_Line(float'Image(leftBound));

     --  left frace
     --
     self.vert_positions(0) := (leftBound, 0.0, frontBound);
     self.vert_positions(1) := (leftBound, 0.0, backBound);
     self.vert_positions(2) := (0.0 ,h,backBound);
     self.vert_positions(3) := (0.0 ,h,frontBound);

     self.triangles(0).A_index := 0;
     self.triangles(0).B_index := 1;
     self.triangles(0).C_index := 2;

     self.triangles(1).A_index := 0;
     self.triangles(1).B_index := 2;
     self.triangles(1).C_index := 3;

     --  right frace
     --
     self.vert_positions(4) := (rightBound,0.0,frontBound);
     self.vert_positions(5) := (rightBound,0.0,backBound);
     self.vert_positions(6) := (0.0,h,backBound);
     self.vert_positions(7) := (0.0,h,frontBound);

     self.triangles(2).A_index := 4;
     self.triangles(2).B_index := 5;
     self.triangles(2).C_index := 6;

     self.triangles(3).A_index := 4;
     self.triangles(3).B_index := 6;
     self.triangles(3).C_index := 7;

     --  down frace
     --
     self.vert_positions(8)  := (leftBound, 0.0,frontBound);
     self.vert_positions(9)  := (leftBound, 0.0,backBound);
     self.vert_positions(10) := (rightBound,0.0,backBound);
     self.vert_positions(11) := (rightBound,0.0,frontBound);

     self.triangles(4).A_index := 8;
     self.triangles(4).B_index := 9;
     self.triangles(4).C_index := 10;

     self.triangles(5).A_index := 8;
     self.triangles(5).B_index := 10;
     self.triangles(5).C_index := 11;

     --  front frace
     --
     self.vert_positions(12) := (leftBound,  0.0, frontBound);
     self.vert_positions(13) := (rightBound, 0.0, frontBound);
     self.vert_positions(14) := (0.0, h, frontBound);
     self.triangles(6).A_index := 12;
     self.triangles(6).B_index := 13;
     self.triangles(6).C_index := 14;

     --  back frace
     --
     self.vert_positions(15) := (leftBound,0.0,  backBound);
     self.vert_positions(16) := (rightBound,0.0, backBound);
     self.vert_positions(17) := (0.0, h, backBound);
     self.triangles(7).A_index := 15;
     self.triangles(7).B_index := 16;
     self.triangles(7).C_index := 17;

     for i in self.material_ids'First .. self.material_ids'Last loop
       self.material_ids(i) := matId;
     end loop;

     --ComputeFlatNormals(self);

     -- transform geometry with matrix
     --
     for i in self.vert_positions'First..self.vert_positions'Last loop
       self.vert_positions(i) := mTransform*self.vert_positions(i);
       --self.vert_normals(i)   := TransformNormal(mTransform, self.vert_normals(i));
     end loop;


   end;


end Geometry;


