with Interfaces;
with Ada.Streams.Stream_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Vector_Math;


use Interfaces;
use Ada.Streams.Stream_IO;
use Vector_Math;


package body Geometry is

  package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(float);
  use Float_Functions;


  function IntersectPlaneXZ (r : Ray; planeMat : MaterialRef) return Hit is
    t : float := infinity;
    x,y : float;
   begin

    if abs(r.direction.y) > 0.0 then

      t := - r.origin.y / r.direction.y;
      x := r.origin.x + r.direction.x * t;
      y := r.origin.z + r.direction.z * t;

      return ( prim_type  => Plane_TypeId,
               prim_index => 0,
	       is_hit     => (t > 0.0) and (abs(x) < 200.0) and (abs(y) < 200.0),
	       t          => t,
               mat        => planeMat,
               matId      => 0,  -- not used
	       tx         => x,
               ty         => y,
               normal     => (0.0, 1.0, 0.0)
	    );
    else
      return null_hit;
    end if;


  end IntersectPlaneXZ;


  function IntersectAllSpheres (r : Ray; a_spheres : Spheres_Array_Ptr) return Hit is
    min_t  : float  := infinity;
    t1, t2 : float;
    min_i  : Integer := 0;
    k      : float3;
    is_hit : boolean := false;
    b, c, d, sqrtd : float;
    finalNormal : float3 := (0.0, 1.0, 0.0);
  begin

    for i in a_spheres'First .. a_spheres'Last loop


      k := r.origin - a_spheres(i).pos;
      b := dot(k,r.direction);
      c := dot(k,k) - a_spheres(i).r*a_spheres(i).r;
      d := b * b - c;

      if d >= 0.0 then

        sqrtd := sqrt(d);
        t1 := -b - sqrtd;
        t2 := -b + sqrtd;

        if t1 > 0.0 and t1 < min_t then
          min_t := t1;
	  min_i := i;
        elsif t2 > 0.0 and t2 < min_t then
          min_t := t2;
	  min_i := i;
        end if;

      end if;

    end loop;

    is_hit := (min_t > 0.0 and min_t < infinity);
    if not is_hit then
      min_t := 1.0; -- to prevent constraint error
    else
      finalNormal := normalize((r.origin + r.direction*min_t) - a_spheres(min_i).pos);
    end if;

    return ( prim_type  => Sphere_TypeId,
             prim_index => min_i,
	     is_hit     => is_hit,
	     t          => min_t,
             mat        => a_spheres(min_i).mat,
             matId      => 0, -- not used
             normal     => finalNormal,
             tx => 0.0, ty => 0.0
            );

  exception

    when Constraint_Error =>

    return ( prim_type  => Sphere_TypeId,
             prim_index => 0,
  	     is_hit     => False,
  	     t          => 1.0,
             mat        => a_spheres(0).mat,
             matId      => 0, -- not used
             normal     => finalNormal,
             tx => 0.0, ty => 0.0
            );

  end IntersectAllSpheres;


  function IntersectFlatLight(r: Ray; lightGeom : FlatLight; lMat : MaterialRef) return Hit is
    is_hit : boolean := false;
    tmin   : float   := 1.0e38;
    inv_dir_y : float := 1.0/r.direction.y;
    hit_point : float3;
  begin

    tmin      := (lightGeom.boxMax.y - r.origin.y)*inv_dir_y;
    hit_point := r.origin + tmin*r.direction;

    is_hit    := (hit_point.x > lightGeom.boxMin.x) and (hit_point.x < lightGeom.boxMax.x) and
                 (hit_point.z > lightGeom.boxMin.z) and (hit_point.z < lightGeom.boxMax.z) and
                 (tmin >= 0.0);

    return ( prim_type  => Quad_TypeId,
             prim_index => 0,
	     is_hit     => is_hit,
	     t          => tmin,
             mat        => lMat,
             matId      => 0,              -- not used
             normal     => (0.0,-1.0,0.0),
             tx         => 0.0,
             ty         => 0.0
	   );

  end IntersectFlatLight;


  function IntersectBox(r: Ray; box : AABB) return LiteGeomHit is
    tmin,tmax : float := 0.0;
    lo,hi,lo1,hi1,lo2,hi2 : float;
    inv_dir_x,inv_dir_y,inv_dir_z : float;
    res : LiteGeomHit;
    --epsilonDiv : constant float := 1.0e-25;
  begin

    inv_dir_x := 1.0/r.direction.x;
    inv_dir_y := 1.0/r.direction.y;
    inv_dir_z := 1.0/r.direction.z;

    lo  := (box.max.x - r.origin.x)*inv_dir_x;
    hi  := (box.min.x - r.origin.x)*inv_dir_x;

    lo1 := (box.max.y - r.origin.y)*inv_dir_y;
    hi1 := (box.min.y - r.origin.y)*inv_dir_y;

    lo2 := (box.max.z - r.origin.z)*inv_dir_z;
    hi2 := (box.min.z - r.origin.z)*inv_dir_z;

    tmin := min(lo,hi);
    tmax := max(lo,hi);

    tmin := max(tmin, min(lo1,hi1));
    tmax := min(tmax, max(lo1,hi1));

    tmin := max(tmin, min(lo2,hi2));
    tmax := min(tmax, max(lo2,hi2));

    res.tmin   := tmin;
    res.tmax   := tmax;
    res.is_hit := (tmax > 0.0) and (tmin <= tmax);

    return res;

  exception

    when Constraint_Error =>
    res.tmin   := 0.0;
    res.tmax   := 0.0;
    res.is_hit := False;

    return res;

  end IntersectBox;

  function IntersectCornellBox(r: Ray; boxData : CornellBox) return Hit is
    eps     : float := 1.0e-5;
    p       : float3;
    tmpHit  : LiteGeomHit;
    planeId : integer := 0;
  begin

    tmpHit := IntersectBox(r, boxData.box);

    if tmpHit.is_hit then

      p := r.origin + tmpHit.tmax*r.direction;

      if abs(p.x - boxData.box.min.x) < eps then planeId := 0; end if;
      if abs(p.x - boxData.box.max.x) < eps then planeId := 1; end if;

      if abs(p.y - boxData.box.min.y) < eps then planeId := 2; end if;
      if abs(p.y - boxData.box.max.y) < eps then planeId := 3; end if;

      if abs(p.z - boxData.box.min.z) < eps then planeId := 4; end if;
      if abs(p.z - boxData.box.max.z) < eps then planeId := 5; end if;

      return( prim_type  => Plane_TypeId,
              prim_index => planeId,
	      is_hit     => not (planeId = 5),
	      t          => tmpHit.tmax,
              mat        => null, --g_scn.materials(boxData.mat_indices(planeId)),
              matId      => boxData.mat_indices(planeId),
              normal     => boxData.normals(planeId),
	      tx         => 0.0,
              ty         => 0.0
	    );
    else
      return null_hit;
    end if;

  end IntersectCornellBox;

  function IntersectTriangle(r: Ray; A : float3; B : float3; C : float3; t_min : float; t_max : float) return LiteGeomHit is
    res : LiteGeomHit;
    edge1, edge2, pvec, qvec, tvec : float3;
    invDet,u,v,t : float;
  begin

    edge1  := B - A;
    edge2  := C - A;
    pvec   := cross(r.direction, edge2);
    tvec   := r.origin - A;
    qvec   := cross(tvec, edge1);
    invDet := 1.0 / dot(edge1, pvec);

    v := dot(tvec, pvec)*invDet;
    u := dot(qvec, r.direction)*invDet;
    t := dot(edge2, qvec)*invDet;

    res.is_hit := false;

    if (v > 0.0 and u > 0.0 and u + v < 1.0 and t > t_min and t < t_max) then

      res.u      := u;
      res.v      := v;
      res.tmin   := t;
      res.tmax   := t + 1.0e-6;
      res.is_hit := true;

    end if;

    return res;

  end IntersectTriangle;


  function IntersectMeshBF(r: Ray; meshGeom : Mesh) return Hit is
     tmpHit, nearestHit : LiteGeomHit;
     triInd : Triangle;
     A,B,C  : float3;
     nearestTriId : integer := 0;
  begin

    tmpHit := IntersectBox(r, meshGeom.bbox);

    if tmpHit.is_hit then

      nearestHit.tmin   := tmpHit.tmin;
      nearestHit.tmax   := tmpHit.tmax;
      nearestHit.is_hit := false;

      for i in meshGeom.triangles'First .. meshGeom.triangles'Last loop

        triInd := meshGeom.triangles(i);

        A := meshGeom.vert_positions(triInd.A_index);
        B := meshGeom.vert_positions(triInd.B_index);
        C := meshGeom.vert_positions(triInd.C_index);

        tmpHit := IntersectTriangle(r,A,B,C,nearestHit.tmin,nearestHit.tmax);

        if tmpHit.is_hit then
          nearestHit   := tmpHit;
          nearestTriId := i;
        end if;

      end loop;

      triInd := meshGeom.triangles(nearestTriId);

      return( prim_type  => Triangle_TypeId,
              prim_index => nearestTriId,
	      is_hit     => nearestHit.is_hit,
	      t          => nearestHit.tmin,
              mat        => null,
              matId      => 2, --meshGeom.material_ids(nearestTriId), -- (-1.0)*normalize(cross(A-B, A-C)),
              normal     => (1.0 - nearestHit.u - nearestHit.v)*meshGeom.vert_normals(triInd.A_index) + nearestHit.v*meshGeom.vert_normals(triInd.B_index) + nearestHit.u*meshGeom.vert_normals(triInd.C_index),
	      tx         => (1.0 - nearestHit.u - nearestHit.v)*meshGeom.vert_tex_coords(triInd.A_index).x + nearestHit.v*meshGeom.vert_tex_coords(triInd.B_index).x + nearestHit.u*meshGeom.vert_tex_coords(triInd.C_index).x,
              ty         => (1.0 - nearestHit.u - nearestHit.v)*meshGeom.vert_tex_coords(triInd.A_index).y + nearestHit.v*meshGeom.vert_tex_coords(triInd.B_index).y + nearestHit.u*meshGeom.vert_tex_coords(triInd.C_index).y
	    );

    else
      return null_hit;
    end if;

  end IntersectMeshBF;



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
     self.triangles       := new Triangle_Array(0..inum/3-1);
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
     self.vert_positions(2) := (0.0,h,backBound);
     self.vert_positions(3) := (0.0,h,frontBound);

     self.triangles(0).A_index := 1;
     self.triangles(0).B_index := 0;
     self.triangles(0).C_index := 2;

     self.triangles(1).A_index := 0;
     self.triangles(1).B_index := 3;
     self.triangles(1).C_index := 2;

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


     self.bbox.min := (INFINITY, INFINITY, INFINITY);
     self.bbox.max := (-INFINITY, -INFINITY, -INFINITY);

     -- transform geometry with matrix and find bbox
     --
     for i in self.vert_positions'First..self.vert_positions'Last loop

       self.vert_positions(i) := mTransform*self.vert_positions(i);

       self.bbox.min.x := min(self.bbox.min.x, self.vert_positions(i).x);
       self.bbox.min.y := min(self.bbox.min.y, self.vert_positions(i).y);
       self.bbox.min.z := min(self.bbox.min.z, self.vert_positions(i).z);

       self.bbox.max.x := max(self.bbox.max.x, self.vert_positions(i).x);
       self.bbox.max.y := max(self.bbox.max.y, self.vert_positions(i).y);
       self.bbox.max.z := max(self.bbox.max.z, self.vert_positions(i).z);

       --self.vert_normals(i)   := TransformNormal(mTransform, self.vert_normals(i));

     end loop;

     ComputeFlatNormals(self);

   end CreatePrism;

   type VSGF_Header is record

     fileSizeInBytes : Long_Long_Integer; -- Unsigned_64
     verticesNum     : Integer;
     indicesNum      : Integer;
     materialsNum    : Integer;
     flags           : Integer;

   end record;


   procedure LoadMeshFromVSGF(self: out Mesh; mTransform : in float4x4; a_fileName : String) is

     VSGF_File : Ada.Streams.Stream_IO.File_Type;
     S         : Stream_Access;
     header    : VSGF_Header;

     temp4  : float4 := (0.0, 0.0, 0.0, 0.0);
     temp2  : float2 := (0.0, 0.0);

   begin

     Open(File => VSGF_File,
          Mode => In_File,
          Name => a_fileName,
          Form => "");

     S := Stream(VSGF_File);

     VSGF_Header'Read(S, header);

     Put("load vsgf file : ");
     Put_Line(a_fileName);
     Put("sizeInBytes    :");
     Put_Line(Long_Long_Integer'Image(header.fileSizeInBytes));
     Put("vertices       :");
     Put_Line(Integer'Image(header.verticesNum));
     Put("triangles      :");
     Put_Line(Integer'Image(header.indicesNum/3));
     Put("materials      :");
     Put_Line(Integer'Image(header.materialsNum));

     FreeData(self);
     AllocData(self, header.verticesNum, header.indicesNum);

     -- read positions
     --
     for i in 0 .. header.verticesNum - 1 loop
       float4'Read(S, temp4);
       self.vert_positions(i).x := temp4.x;
       self.vert_positions(i).y := temp4.y;
       self.vert_positions(i).z := temp4.z;
     end loop;

     -- read normals
     --
     for i in 0 .. header.verticesNum - 1 loop
       float4'Read(S, temp4);
       self.vert_normals(i).x := temp4.x;
       self.vert_normals(i).y := temp4.y;
       self.vert_normals(i).z := temp4.z;
     end loop;

     -- read texcoords
     --
     for i in 0 .. header.verticesNum - 1 loop
       float2'Read(S, temp2);
       self.vert_tex_coords(i).x := temp2.x;
       self.vert_tex_coords(i).y := temp2.y;
     end loop;

     -- read indices
     --
     for i in 0 .. (header.indicesNum/3-1) loop
       Triangle'Read(S, self.triangles(i));
     end loop;

     -- read material indices
     --
     for i in 0 .. (header.indicesNum/3-1) loop
       integer'Read(S, self.material_ids(i));
     end loop;

     Close(VSGF_File);


     for i in self.vert_positions'First..self.vert_positions'Last loop

       self.vert_positions(i) := mTransform*self.vert_positions(i);

       self.bbox.min.x := min(self.bbox.min.x, self.vert_positions(i).x);
       self.bbox.min.y := min(self.bbox.min.y, self.vert_positions(i).y);
       self.bbox.min.z := min(self.bbox.min.z, self.vert_positions(i).z);

       self.bbox.max.x := max(self.bbox.max.x, self.vert_positions(i).x);
       self.bbox.max.y := max(self.bbox.max.y, self.vert_positions(i).y);
       self.bbox.max.z := max(self.bbox.max.z, self.vert_positions(i).z);

       --self.vert_normals(i)   := TransformNormal(mTransform, self.vert_normals(i));

     end loop;

   end LoadMeshFromVSGF;

end Geometry;


