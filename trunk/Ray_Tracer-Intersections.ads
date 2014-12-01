with Ray_Tracer;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

use Ada.Numerics;
use Ray_Tracer;

private package Ray_Tracer.Intersections is



private

  null_hit : Hit := ( prim_type  => Plane_TypeId,
                      prim_index => -1,
	      	      is_hit     => false,
	      	      t          => infinity,
                      mat        => null,
                      matId      => 0,
	              tx         => 0.0,
                      ty         => 0.0,
                      normal     => (0.0, 1.0, 0.0)
	             );


end Ray_Tracer.Intersections;

