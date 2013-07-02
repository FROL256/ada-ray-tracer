with Ray_Tracer;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

use Ada.Numerics;
use Ray_Tracer;

private package Ray_Tracer.Intersections is


  function IntersectPlaneXZ(r: Ray) return Hit;
  function IntersectAllSpheres(r: Ray; a_spheres : Spheres_Array_Ptr) return Hit;
  function IntersectCornellBox(r: Ray; boxData : CornellBox) return Hit;
  function IntersectFlatLight(r: Ray; lightGeom : FlatLight) return Hit;
  function FindClosestHit(r: Ray) return Hit;

private

  null_hit : Hit := ( prim_type => Plane_TypeId,
                      prim_index => -1,
	      	      is_hit    => false,
	      	      t         => infinity,
	      	      mat       => null,
	              x         => 0.0,
                      y         => 0.0,
                      normal    => (0.0, 1.0, 0.0)
	             );


end Ray_Tracer.Intersections;

