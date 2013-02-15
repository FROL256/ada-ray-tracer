with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Materials;
with Ada.Exceptions;


use Ada.Numerics;
use Ada.Text_IO;
use Materials;

package body Ray_Tracer is

  package Float_Functions is new Generic_Elementary_Functions (float);
  use Float_Functions;


  function ToneMapping(v : float3) return Color is
    max_val : float;
   begin

    max_val := max(v.x, max(v.y, v.z));
    if max_val > 0.0 then

      if max_val > 1.0 then
        return (v.x/max_val, v.y/max_val, v.z/max_val);
      else
        return (v.x, v.y, v.z);
      end if;

    else
      return (0.0, 0.0, 0.0);
    end if;

  end;


  function ColorToUnsigned_32(c : Color) return Unsigned_32 is
    res : Unsigned_32 := 0;
    r, g, b : Float;
    red, green, blue : Unsigned_32;
  begin

    r := c.red*255.0;
    g := c.green*255.0;
    b := c.blue*255.0;

    red := Unsigned_32(r);
    green := Unsigned_32(g);
    blue := Unsigned_32 (b);

    return red or Shift_Left(green, 8) or Shift_Left(blue, 16);

  end ColorToUnsigned_32;



  function EyeRayDirection (x, y : Natural) return float3 is
    res : float3;
    fov : float := Pi / (2.0);
  begin
    res.x := float(x) + 0.5 - (float(width) / 2.0);
    res.y := float(y) + 0.5 - (float(height) / 2.0);
    res.z := -float(width) / safe_tan(fov / 2.0);
    return normalize(res);
  end;


  procedure Generate4RayDirections (x, y : in Natural; arr : out RayDirPack) is
    fov : float := Pi / (2.0);
  begin

    arr(0).x := float(x) + 1.0/3.0 - (float(width) / 2.0);
    arr(0).y := float(y) + 1.0/3.0 - (float(height) / 2.0);
    arr(0).z := -float (width) / safe_tan (fov / 2.0);

    arr(1).x := float(x) + 1.0/3.0 - (float(width) / 2.0);
    arr(1).y := float(y) + 2.0/3.0 - (float(height) / 2.0);
    arr(1).z := -float (width) / safe_tan (fov / 2.0);

    arr(2).x := float(x) + 2.0/3.0 - (float(width) / 2.0);
    arr(2).y := float(y) + 1.0/3.0 - (float(height) / 2.0);
    arr(2).z := -float (width) / safe_tan (fov / 2.0);

    arr(3).x := float(x) + 2.0/3.0 - (float(width) / 2.0);
    arr(3).y := float(y) + 2.0/3.0 - (float(height) / 2.0);
    arr(3).z := -float (width) / safe_tan (fov / 2.0);

    arr(0) := normalize(arr(0));
    arr(1) := normalize(arr(1));
    arr(2) := normalize(arr(2));
    arr(3) := normalize(arr(3));

  end;


  function IntersectPlaneXZ (r : Ray) return Hit is
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
               mat        => g_scn.materials(0),
	       x          => x,
               y          => y,
               normal     => (0.0, 1.0, 0.0)
	    );
    else
      return null_hit;
    end if;


  end IntersectPlaneXZ;

  function GetCamMatrix(cam : Camera) return float4x4 is
  begin
    return LookAtMatrix(cam.pos, cam.lookAT, cam.up);
  end GetCamMatrix;

  function IntersectCornellBox(r: Ray; boxData : access constant CornellBox) return Hit is
    tmin,tmax,tminy,tmaxy,tminz,tmaxz : float := 0.0;
    t : array (0..5) of float;
    t_res : float;
    plane_index : integer := 0;
    inv_dir_x : float := 1.0/r.direction.x;
    inv_dir_y : float := 1.0/r.direction.y;
    inv_dir_z : float := 1.0/r.direction.z;
  begin


    --
    --
    if r.direction.x <= 0.0 then
      tmin := ( boxData.box.max.x - r.origin.x)*inv_dir_x;
      tmax := ( boxData.box.min.x - r.origin.x)*inv_dir_x;
      t(1) := tmin;
      t(0) := tmax;
    else
      tmin := ( boxData.box.min.x - r.origin.x)*inv_dir_x;
      tmax := ( boxData.box.max.x - r.origin.x)*inv_dir_x;
      t(0) := tmin;
      t(1) := tmax;
    end if;

    --
    --
    if r.direction.y <= 0.0 then
      tminy := ( boxData.box.max.y - r.origin.y)*inv_dir_y;
      tmaxy := ( boxData.box.min.y - r.origin.y)*inv_dir_y;
      t(3)  := tminy;
      t(2)  := tmaxy;
    else
      tminy := ( boxData.box.min.y - r.origin.y)*inv_dir_y;
      tmaxy := ( boxData.box.max.y - r.origin.y)*inv_dir_y;
      t(2)  := tminy;
      t(3)  := tmaxy;
    end if;

    if (tmin > tmaxy) or (tminy > tmax) then
      return null_hit;
    end if;

    if tminy > tmin then
      tmin := tminy;
    end if;

    if tmaxy < tmax then
      tmax := tmaxy;
    end if;


    --
    --
    if r.direction.z <= 0.0 then
      tminz := ( boxData.box.max.z - r.origin.z)*inv_dir_z;
      tmaxz := ( boxData.box.min.z - r.origin.z)*inv_dir_z;
      t(5)  := tminz;
      t(4)  := tmaxz;
    else
      tminz := ( boxData.box.min.z - r.origin.z)*inv_dir_z;
      tmaxz := ( boxData.box.max.z - r.origin.z)*inv_dir_z;
      t(4)  := tminz;
      t(5)  := tmaxz;
    end if;

    if (tmin > tmaxz) or (tminz > tmax) then
      return null_hit;
    end if;

    --if tminz > tmin then
    --  tmin := tminz;
    --end if;

    if tmaxz < tmax then
      tmax := tmaxz;
    end if;

    if (tmax > 0.0) and ( tmin <= tmax) then

      if tmin > 0.0 then
        t_res := tmin;
      else
        t_res := tmax;
      end if;

      --
      --
      for i in t'First .. t'Last loop
        if abs(t(i) - t_res) < 0.000001 then
          plane_index := i;
          exit;
        end if;
      end loop;

      return( prim_type  => Plane_TypeId,
              prim_index => plane_index,
	      is_hit     => true,
	      t          => t_res,
	      mat        => g_scn.materials(boxData.mat_indices(plane_index)),
	      x          => 0.0,
              y          => 0.0,
              normal     => boxData.normals(plane_index)
	    );
    else
      return null_hit;
    end if;

  end IntersectCornellBox;

  procedure RaySphereIntersection(r : in ray; spos : in float3; radius : in float; tmin : out float; is_hit : out boolean) is
    b,c,d,sqrtd,tmax : float;
    k : float3;
  begin

    k := r.origin - spos;
    b := dot(k,r.direction);
    c := dot(k,k) - radius*radius;
    d := b*b - c;

    is_hit := false;

    if d >= 0.0 then

      sqrtd := sqrt(d);
      tmin  := -b - sqrtd;
      tmax  := -b + sqrtd;

      if tmin > 0.0 then
        is_hit := true;
      elsif tmax > 0.0 then
        tmin := tmax;
        is_hit := true;
      end if;

    end if;

  end RaySphereIntersection;

  function IntersectAllSpheres (r : Ray; a_spheres : Spheres_Array_Ptr) return Hit is
    min_t  : float  := infinity;
    t1, t2 : float;
    min_i  : Integer := 0;
    k      : float3;
    is_hit : boolean := false;
    b, c, d, sqrtd : float;
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
    end if;

    return ( prim_type  => Sphere_TypeId,
             prim_index => min_i,
	     is_hit     => is_hit,
	     t          => min_t,
	     mat        => a_spheres(min_i).mat,
             normal     => normalize((r.origin + r.direction*min_t) - a_spheres(min_i).pos),
             x => 0.0, y => 0.0
	   );

  end IntersectAllSpheres;

  function IntersectAllTriangles(r: Ray; a_triangles : Triangle_Array_Ptr; a_vert_positions : Float3_Array_Ptr) return Hit is
    min_t    : float   := infinity;
    min_i    : Integer := -1;
    res_norm : float3:= (0.0, 1.0, 0.0);
    mat_index : Integer := 0;
  begin

     for i in a_triangles'First .. a_triangles'Last loop

       declare
         A : float3 := a_vert_positions(a_triangles(i).A_index);
         B : float3 := a_vert_positions(a_triangles(i).B_index);
         C : float3 := a_vert_positions(a_triangles(i).C_index);
         edge1,edge2,pvec,tvec,qvec : float3;
         inv_det,u,v,t : float;
       begin

         edge1 := B - A;
         edge2 := C - A;
         pvec  := cross(r.direction, edge2);
         tvec  := r.origin - A;
         qvec  := cross(tvec,edge1);

         inv_det := 1.0/dot(edge1, pvec);

         v := dot(tvec,pvec)*inv_det;
         u := dot(qvec,r.direction)*inv_det;
         t := dot(edge2,qvec)*inv_det;

         if (v > 0.0 and u > 0.0 and u + v < 1.0 and t > 0.0 and t < min_t) then
           min_t := t;
           min_i := i;
           mat_index := g_scn.mesh1.material_ids(i);
           res_norm  := normalize((-1.0)*cross(edge1,edge2));
         end if;

       end;


     end loop;

     return ( prim_type  => Triangle_TypeId,
              prim_index => min_i,
	      is_hit     => (min_i > -1),
	      t          => min_t,
	      mat        => g_scn.materials(mat_index),
              normal     => res_norm,
              x => 0.0,y => 0.0
	   );

  end IntersectAllTriangles;


  --
  --
  function FindClosestHit(r: Ray) return Hit is
    hits : array (1..3) of Hit;
    nearestHitIndex : integer range hits'First..hits'Last := 1;
    nearestHitDist : float := infinity;
  begin

    hits(1) := IntersectPlaneXZ (r);
    hits(2) := IntersectAllSpheres(r, g_scn.spheres);
    hits(3) := IntersectAllTriangles(r, g_scn.mesh1.triangles, g_scn.mesh1.vert_positions);
    --hits(4) := IntersectCornellBox(r, my_cornell_box'access);

    for i in hits'First .. hits'Last loop

      if hits(i).is_hit and hits(i).t < nearestHitDist then
        nearestHitIndex := i;
        nearestHitDist  := hits(i).t;
      end if;

    end loop;

    return hits(nearestHitIndex);

  end FindClosestHit;


  function ComputeShadow(hit_pos : float3; lpos : float3) return Shadow_Hit is
    res : Shadow_Hit;
    shadowRay : Ray;
      h : Hit;
      epsilon : float;
      epsilon2 : float;
      maxDist : float;
  begin

    epsilon := max(abs(hit_pos.x), abs(hit_pos.y), abs(hit_pos.z))*0.0001;
    epsilon := max(epsilon, 0.0001);

    shadowRay.direction := normalize(lpos-hit_pos);
    shadowRay.origin    := hit_pos + shadowRay.direction*epsilon;

    h := FindClosestHit(shadowRay);

    maxDist  := length(hit_pos - lpos);
    epsilon2 := max(maxDist*0.001, 0.0001);

    if h.is_hit and then (h.t < maxDist-epsilon2 and h.t > 10.0*epsilon) then
      res.in_shadow        := true;
      res.percentageCloser := (1.0, 1.0, 1.0);
    else
      res.in_shadow        := false;
      res.percentageCloser := (0.0, 0.0, 0.0);
    end if;

    return res;

  end ComputeShadow;


  --
  --
  function RayTrace (r : Ray; recursion_level : Integer) return float3 is
    res_color : float3 := background_color;
    h : aliased Hit;
    hit_pos : float3;
    totalInteralReflectionOccured : boolean := false;
  begin

    if recursion_level = 0 then
      return res_color;
    end if;

    h := FindClosestHit(r);

    if h.is_hit then

      hit_pos := (r.origin + r.direction*h.t);

      res_color := res_color + h.mat.ka;

      if compute_shadows then
        if not ComputeShadow(hit_pos, g_scn.lights(0).pos).in_shadow then
      	  res_color := res_color + Shade(r,h, g_scn.lights(0));
        end if;
      else
 	 res_color := res_color + Shade(r,h, g_scn.lights(0));
      end if;

      res_color := h.mat.AdjustShadedColor(res_color, h.x, h.y);

      if length(h.mat.transparency) > 0.0 then

        totalInteralReflectionOccured := TotalInternalReflection(h.mat.all, r.direction, h.normal);

        if not totalInteralReflectionOccured then

	  declare
            refractedRay : Ray;
            transmittedColor : float3;
            eps : float  := 0.001*length(hit_pos);
          begin

	    refract( mat    => h.mat.all,
	    	     rayDir => r.direction,
	    	     normal => h.normal,
                     color  => transmittedColor,
            	     wt     => refractedRay.direction);

            transmittedColor := h.mat.transparency;
            refractedRay.origin := hit_pos + r.direction*0.001;
            res_color := res_color + transmittedColor*RayTrace(refractedRay, recursion_level-1);
          end;
       end if;
      end if;


      if length(h.mat.reflection) > 0.0 then

        declare
          reflectedRay : Ray;
          eps : float  := 0.00001*length(hit_pos);
        begin

          reflectedRay.direction := reflect (r.direction, h.normal);
          reflectedRay.origin    := hit_pos + eps*h.normal;

          if totalInteralReflectionOccured then
            res_color := res_color + RayTrace(reflectedRay, recursion_level-1);
          else
            res_color := res_color + h.mat.reflection*RayTrace(reflectedRay, recursion_level-1);
          end if;

        end;
      end if;

    end if;

    return res_color;

  end RayTrace;


  function GetAttenuation(l_pos : in float3; p : in float3) return float;

  function GetAttenuation(l_pos : in float3; p : in float3) return float is
    d : float;
  begin
    d := length(l_pos-p);
    --return 1.0/(1.0 + d*0.01 + d*d*0.001);
    return 1.0;
  end GetAttenuation;

  function Shade (in_ray : Ray; h : Hit; a_light : light) return float3 is
      l,v,hit_pos,S : float3;
      NormalDotLight: float;
  begin

    hit_pos := in_ray.origin + h.t*in_ray.direction;
    l := normalize(a_light.pos - hit_pos);
    v := normalize(in_ray.origin - hit_pos);
    NormalDotLight := dot(h.normal, l);

    S := EvalCookTorranceBRDF(h.mat.all, l, v, h.normal);


    return a_light.color*(h.mat.ka + S*GetAttenuation(a_light.pos, hit_pos));

  end;


  task body Ray_Trace_Thread is
    r : Ray;
    rayDirs : RayDirPack;
    color : float3;
  begin

    for y in yBegin .. yEnd loop
      for x in 0 .. width - 1 loop

	r.origin := g_cam.pos;

        if anti_aliasing_on then

	  color := background_color;
          Generate4RayDirections(x,y,rayDirs);

          for i in 0 .. 3 loop
            r.direction := normalize(g_cam.matrix*rayDirs(i));
            color := color + RayTrace(r,max_depth);
          end loop;

          screen_buffer(x,y) := ColorToUnsigned_32(ToneMapping(color*0.25));

	else

	  r.direction := EyeRayDirection(x,y);
          r.direction := normalize(g_cam.matrix*r.direction);
	  screen_buffer(x,y) := ColorToUnsigned_32(ToneMapping(RayTrace(r,max_depth)));

	end if;

      end loop;
    end loop;

    accept Finish;

    exception
      when The_Error : others =>
        Put_Line("Error raised in the thread:");
        Put_Line(Ada.Exceptions.Exception_Name(The_Error));
        Put_Line(Ada.Exceptions.Exception_Message(The_Error));
	Put_Line("");

  end Ray_Trace_Thread;

  procedure MultiThreadedRayTracing is

    threads : array (0..threads_num) of Ray_Trace_Thread_Ptr;
    stepY : integer := height/threads_num;

  begin

    Put("ray trace threads num: ");
    Put_Line(integer'Image(threads_num));

    for i in 0..threads_num-1 loop
      threads(i) := new Ray_Trace_Thread(i*stepY, (i+1)*stepY-1);
    end loop;

    for i in 0..threads_num-1 loop
      threads(i).Finish;
    end loop;

  end MultiThreadedRayTracing;

  procedure InitScene is
    size : float := 7.0;
    tmp : float;
  begin

    -- init materials
    --
    if g_scn.floorMaterial = null then
      g_scn.floorMaterial := new MaterialWithMultiplyedTex; -- material 0
    end if;

    if  g_scn.dielecric1 = null then
       g_scn.dielecric1 := new DielectricMaterial; -- material 1
    end if;

    for i in 1 .. g_scn.materials'Last loop
      if g_scn.materials(i) = null then
        g_scn.materials(i) := new Material;
      end if;
    end loop;

    g_scn.floorMaterial.ka := (0.2, 0.2, 0.2);
    g_scn.floorMaterial.kd := (0.3, 0.3, 0.3);
    g_scn.floorMaterial.ks := (0.6, 0.6, 0.6);
    g_scn.floorMaterial.reflection := (0.65, 0.65, 0.65);
    g_scn.floorMaterial.roughness := 0.25;
    g_scn.materials(0) := MaterialRef(g_scn.floorMaterial);

    g_scn.materials(1).ka := (0.0, 0.0, 0.0);
    g_scn.materials(1).kd := (0.0, 0.0, 0.0);
    g_scn.materials(1).ks := (0.0, 0.0, 0.0);
    g_scn.materials(1).reflection := (0.45,0.45,0.45);
    g_scn.materials(1).roughness := 0.1;
    g_scn.materials(1).transparency := (0.5, 0.5, 0.5);
    g_scn.materials(1).ior := 1.5;

   --g_scn.materials(1) := MaterialRef(g_scn.dielecric1);

    g_scn.materials(2).ka := (0.0, 0.0, 0.1);
    g_scn.materials(2).kd := (0.0, 0.0, 0.2);
    g_scn.materials(2).ks := (0.0, 0.0, 0.5);
    g_scn.materials(2).reflection := (0.5,0.5,0.5);
    g_scn.materials(2).roughness := 0.25;
    g_scn.materials(2).transparency := (0.0,0.0,0.25);
    g_scn.materials(2).ior := 1.2;

    g_scn.materials(3).ka := (0.1, 0.1, 0.0);
    g_scn.materials(3).kd := (0.2, 0.2, 0.0);
    g_scn.materials(3).ks := (0.5, 0.5, 0.0);
    g_scn.materials(3).reflection := (0.5,0.5,0.5);
    g_scn.materials(3).roughness := 0.5;
    g_scn.materials(3).transparency := (0.25,0.25,0.0);
    g_scn.materials(3).ior := 1.1;

    g_scn.materials(4).ka := (0.1, 0.05, 0.0);
    g_scn.materials(4).kd := (0.2, 0.25, 0.0);
    g_scn.materials(4).ks := (0.5, 0.25, 0.0);
    g_scn.materials(4).reflection := (0.5,0.5,0.5);
    g_scn.materials(4).roughness := 0.25;

    g_scn.materials(5).ka := (0.0, 0.1, 0.0);
    g_scn.materials(5).kd := (0.0, 0.1, 0.0);
    g_scn.materials(5).ks := (0.0, 0.25, 0.0);
    g_scn.materials(5).reflection := (0.0,0.25,0.0);
    g_scn.materials(5).roughness := 0.25;
    g_scn.materials(5).transparency := (0.0,0.5,0.0);
    g_scn.materials(5).ior := 1.4;

    g_scn.materials(6).ka := (0.1, 0.0, 0.1);
    g_scn.materials(6).kd := (0.2, 0.0, 0.2);
    g_scn.materials(6).ks := (0.5, 0.0, 0.5);
    g_scn.materials(6).reflection := (0.5,0.5,0.5);
    g_scn.materials(6).roughness := 0.25;

    g_scn.materials(7).ka := (0.1, 0.0, 0.0);
    g_scn.materials(7).kd := (0.0, 0.0, 0.0);
    g_scn.materials(7).ks := (0.5, 0.5, 0.5);
    g_scn.materials(7).reflection := (0.5,0.5,0.5);
    g_scn.materials(7).roughness := 0.25;

    g_scn.materials(8).ka := (0.1, 0.0, 0.0); -- used for plane also
    g_scn.materials(8).kd := (0.2, 0.0, 0.0);
    g_scn.materials(8).ks := (0.5, 0.0, 0.0);
    g_scn.materials(8).reflection := (0.5,0.5,0.5);
    g_scn.materials(8).roughness := 0.25;

    g_scn.materials(9).ka := (0.1, 0.0, 0.1);
    g_scn.materials(9).kd := (0.2, 0.0, 0.2);
    g_scn.materials(9).ks := (0.4, 0.0, 0.4);
    g_scn.materials(9).reflection := (0.75,0.75,0.75);
    g_scn.materials(9).roughness := 0.25;

    g_scn.materials(10).ka := (0.1, 0.0, 0.0);
    g_scn.materials(10).kd := (0.5, 0.0, 0.0);
    g_scn.materials(10).ks := (0.5, 0.0, 0.0);
    g_scn.materials(10).reflection := (0.5,0.5,0.5);
    g_scn.materials(10).roughness := 0.25;

    -- init spheres geometry
    --
    if g_scn.spheres /= null then
      delete(g_scn.spheres);
    end if;

    g_scn.spheres := new Spheres_Array(0..29);

    --g_scn.spheres(0).pos := (-3.0,2.0,-1.0);
    --g_scn.spheres(0).r   := 1.5;
    --g_scn.spheres(0).mat := g_scn.materials(1);

    --g_scn.spheres(1).pos := (3.0,2.0,-1.0);
    --g_scn.spheres(1).r   := 1.5;
    --g_scn.spheres(1).mat := g_scn.materials(2);

    --g_scn.spheres(2).pos := (0.0,4.0,-2.0);
    --g_scn.spheres(2).r   := 1.5;
    --g_scn.spheres(2).mat := g_scn.materials(3);

    declare
      mTranslate : float4x4 := IdentityMatrix;
      mScale     : float4x4 := IdentityMatrix;
      mRotate    : float4x4 := IdentityMatrix;
    begin

      mTranslate(0,3) := 0.0;
      mTranslate(1,3) := 1.0;
      mTranslate(2,3) := 2.0;

      mScale(0,0) := 2.0;
      mScale(1,1) := 2.0;
      mScale(2,2) := 2.0;

      declare
        angle : float := Pi/1.5;
        sint  : float := sin(angle);
        cost  : float := cos(angle);
      begin
        mRotate(0,0) := cost; mRotate(0,2) := -sint;
        mRotate(2,0) := sint; mRotate(2,2) := cost;
      end;

      CreatePrism(self => g_scn.mesh1,
		  mTransform => mRotate, -- m1*m2 do not work right now!
		  size  =>  1.5,
		  angle =>  Pi/4.0,
		  matId =>  4);

     for i in g_scn.mesh1.vert_positions'First .. g_scn.mesh1.vert_positions'Last loop
       g_scn.mesh1.vert_positions(i) := g_scn.mesh1.vert_positions(i) + (0.0, 0.5, 0.0);
     end loop;

    end;

    for i in g_scn.spheres'First .. g_scn.spheres'Last loop

      g_scn.spheres(i).pos.x := -size + 2.0*size*Float_Random.Random(random_gen);
      g_scn.spheres(i).pos.y :=         1.0*size*Float_Random.Random(random_gen);
      g_scn.spheres(i).pos.z := -size + 2.0*size*Float_Random.Random(random_gen) - 3.0;
      g_scn.spheres(i).r := 0.5 + 0.5*Float_Random.Random(random_gen);

      tmp := 10.0*Float_Random.Random(random_gen);
      if tmp > 10.0 then
        tmp := 10.0;
      end if;

      g_scn.spheres(i).mat := g_scn.materials(Integer(tmp));

    end loop;

    g_scn.lights(0).color := (1.0, 1.0, 1.0);
    g_scn.lights(0).pos   := (10.0, 20.0, 10.0);

    g_cam.pos    := (0.0, 3.0, 12.0);
    g_cam.lookAt := (0.0, 0.0, 0.0);
    g_cam.up     := (0.0, 1.0, 0.0);
    g_cam.matrix := IdentityMatrix;

    --InitFractalScene;
    g_cam.matrix := RotationMatrix(-0.1, (1.0,0.0,0.0));

  end InitScene;


  procedure ResizeViewport(size_x, size_y : integer) is

  begin
    width  := Positive(size_x);
    height := Positive(size_y);

    delete(screen_buffer);
    screen_buffer := new ScreenBufferData(0..width-1, 0..height-1);
  end ResizeViewport;

  -- fractal spheres
  --
  procedure InitFractalScene is
    max_level : integer;
  begin

   max_level := 3;
   g_scn.sph_top_max := 1;
   for i in 1..max_level-1 loop
     g_scn.sph_top_max := g_scn.sph_top_max*5;
   end loop;
   g_scn.sph_top_max := g_scn.sph_top_max+1;

   g_scn.spheres := new Spheres_Array(0..g_scn.sph_top_max);
   g_scn.sph_top := 0;

   g_scn.frac_sph_directions(0) := (1.0,0.0,0.0);
   g_scn.frac_sph_directions(1) := (0.0,1.0,0.0);
   g_scn.frac_sph_directions(2) := (0.0,0.0,1.0);
   g_scn.frac_sph_directions(3) := (-1.0,0.0,0.0);
   g_scn.frac_sph_directions(4) := (0.0,-1.0,0.0);
   g_scn.frac_sph_directions(5) := (0.0,0.0,-1.0);

   PushSphereInArrayRec(1, max_level, (0.0,3.0,-10.0), 3.0, g_scn.frac_sph_directions);

   g_cam.pos := (10.0,8.0,10.0);

  end InitFractalScene;

  procedure PushSphereInArrayRec(prevDirectionIndex, depth: integer; pos: float3; curr_size : float; directions : SpheresDirections) is
    size, next_size, tmp : float;
    mRot : float4x4;
    dir_prev : float3 := directions(prevDirectionIndex);
    angle_rot : float;
    local_directions : SpheresDirections;
  begin

      if depth = 0 or g_scn.sph_top >= g_scn.sph_top_max then
        return;
      end if;

      size := curr_size;
      next_size := 0.5*curr_size;

      tmp := 1.0 + 9.0*Float_Random.Random(random_gen);
      if tmp > 10.0 then
        tmp := 10.0;
      end if;

      assert(g_scn.sph_top < g_scn.spheres'Last);

      g_scn.spheres(g_scn.sph_top).pos := pos;
      g_scn.spheres(g_scn.sph_top).r   := size;
      g_scn.spheres(g_scn.sph_top).mat := g_scn.materials(Integer(tmp));
      g_scn.sph_top := g_scn.sph_top+1;

      angle_rot := -1.0 + 2.0*Float_Random.Random(random_gen);
      mRot := RotationMatrix(angle_rot, dir_prev);
      for i in 0..5 loop
        local_directions(i) := normalize(mRot*directions(i));
      end loop;

      for i in 0..5 loop

        if not (i = (prevDirectionIndex+3) mod 6 ) then
          PushSphereInArrayRec(i, depth-1, pos + (size+next_size)*local_directions(i), next_size, local_directions);
        end if;
      end loop;

  end PushSphereInArrayRec;

end Ray_Tracer;
