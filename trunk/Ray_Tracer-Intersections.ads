with Ray_Tracer;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

use Ada.Numerics;
use Ray_Tracer;

private package Ray_Tracer.Intersections is

  --type Ray renames Ray_Tracer.Ray;

  function IntersectPlaneXZ(r: Ray) return Hit;
  function IntersectAllSpheres(r: Ray; a_spheres : Spheres_Array_Ptr) return Hit;
  function IntersectCornellBox(r: Ray; boxData : CornellBox) return Hit;
  function IntersectFlatLight(r: Ray; lightGeom : FlatLight) return Hit;
  function FindClosestHit(r: Ray) return Hit;

end Ray_Tracer.Intersections;

