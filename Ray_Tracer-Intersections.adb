with Ray_Tracer;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ray_Tracer;

use Ada.Numerics;
use Ray_Tracer;

package body Ray_Tracer.Intersections is

 package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(float);
 use Float_Functions;

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

   function IntersectCornellBox(r: Ray; boxData : CornellBox) return Hit is
    tmin,tmax,tminy,tmaxy,tminz,tmaxz : float := 0.0;
    imin,imax,iminy,imaxy,iminz,imaxz : integer := 0;
    lo,hi,lo1,hi1,lo2,hi2 : float;
    p : float3;
    planeId : integer := 4;
    inv_dir_x,inv_dir_y,inv_dir_z : float;
    eps : float := 1.0e-5;
  begin

    inv_dir_x := 1.0/r.direction.x;
    inv_dir_y := 1.0/r.direction.y;
    inv_dir_z := 1.0/r.direction.z;

    lo  := (boxData.box.max.x - r.origin.x)*inv_dir_x;
    hi  := (boxData.box.min.x - r.origin.x)*inv_dir_x;

    lo1 := (boxData.box.max.y - r.origin.y)*inv_dir_y;
    hi1 := (boxData.box.min.y - r.origin.y)*inv_dir_y;

    lo2 := (boxData.box.max.z - r.origin.z)*inv_dir_z;
    hi2 := (boxData.box.min.z - r.origin.z)*inv_dir_z;

    tmin := min(lo,hi);
    tmax := max(lo,hi);

    tmin := max(tmin, min(lo1,hi1));
    tmax := min(tmax, max(lo1,hi1));

    tmin := max(tmin, min(lo2,hi2));
    tmax := min(tmax, max(lo2,hi2));

    if (tmax > 0.0) and (tmin <= tmax) then

      p := r.origin + tmax*r.direction;

      if abs(p.x - boxData.box.min.x) < eps then planeId := 0; end if;
      if abs(p.x - boxData.box.max.x) < eps then planeId := 1; end if;

      if abs(p.y - boxData.box.min.y) < eps then planeId := 2; end if;
      if abs(p.y - boxData.box.max.y) < eps then planeId := 3; end if;

      if abs(p.z - boxData.box.min.z) < eps then planeId := 4; end if;
      if abs(p.z - boxData.box.max.z) < eps then planeId := 5; end if;

      return( prim_type  => Plane_TypeId,
              prim_index => planeId,
	      is_hit     => not (planeId = 5),
	      t          => tmax,
	      mat        => g_scn.materials(boxData.mat_indices(planeId)),
              normal     => boxData.normals(planeId),
	      x => 0.0, y => 0.0
	    );
    else
      return null_hit;
    end if;

  end IntersectCornellBox;


  function IntersectFlatLight(r: Ray; lightGeom : FlatLight) return Hit is
    is_hit : boolean := false;
    tmin   : float   := 1.0e38;
    inv_dir_y : float := 1.0/r.direction.y;
    hit_point : float3;
  begin

    tmin := ( lightGeom.boxMax.y - r.origin.y)*inv_dir_y;
    hit_point := r.origin + tmin*r.direction;

    is_hit    := (hit_point.x > lightGeom.boxMin.x) and (hit_point.x < lightGeom.boxMax.x) and
                 (hit_point.z > lightGeom.boxMin.z) and (hit_point.z < lightGeom.boxMax.z) and
                 (tmin >= 0.0);

    return ( prim_type  => Quad_TypeId,
             prim_index => 0,
	     is_hit     => is_hit,
	     t          => tmin,
	     mat        => g_scn.materials(4),
             normal     => (0.0,-1.0,0.0),
             x => 0.0,y => 0.0
	   );

  end IntersectFlatLight;


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


  --
  --
  function FindClosestHit(r: Ray) return Hit is
    hits : array (1..3) of Hit;
    nearestHitIndex : integer range hits'First..hits'Last := 1;
    nearestHitDist  : float := infinity;
  begin

    --hits(1) := IntersectPlaneXZ (r);
    --hits(2) := IntersectAllSpheres(r, g_scn.spheres);
    --hits(3) := IntersectAllTriangles(r, g_scn.mesh1.triangles, g_scn.mesh1.vert_positions);

    hits(1) := IntersectAllSpheres(r, g_scn.spheres);
    hits(2) := IntersectCornellBox(r, my_cornell_box);
    hits(3) := IntersectFlatLight(r, g_light);

    for i in hits'First .. hits'Last loop

      if hits(i).is_hit and hits(i).t < nearestHitDist then
        nearestHitIndex := i;
        nearestHitDist  := hits(i).t;
      end if;

    end loop;

    return hits(nearestHitIndex);

  end FindClosestHit;


end Ray_Tracer.Intersections;


