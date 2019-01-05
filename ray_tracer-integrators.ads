with Interfaces;
with Ada.Numerics.Float_Random;
with Ada.Assertions;
with Vector_Math;
with Materials;
with Ada.Unchecked_Deallocation;
with Ray_Tracer;

use Interfaces;
use Vector_Math;
use Materials;
use Ada.Assertions;
use Ray_Tracer;

private package Ray_Tracer.Integrators is

  ---------------------------------------------------------------------------------------------------------------------------------------------
  ---------------------------------------------------------------------------------------------------------------------------------------------
  ---------------------------------------------------------------------------------------------------------------------------------------------


  -- integrators
  --
  type Integrator is abstract tagged record
    gen : RandRef := null;
  end record;

  type IntegratorRef is access Integrator'Class;

  function  PathTrace(self : Integrator; r : Ray; prevSample : MatSample; recursion_level : Integer) return float3 is abstract;
  procedure Init(self : in out Integrator) is abstract;
  procedure DoPass(self : in out Integrator; colBuff : AccumBuffRef);


  -- stupid path tracer
  --
  type SimplePathTracer is new Integrator with null record;

  procedure Init(self : in out SimplePathTracer);
  function  PathTrace(self : SimplePathTracer; r : Ray; prevSample : MatSample; recursion_level : Integer) return float3;


  -- path tracer with shadow rays
  --
  type PathTracerWithShadowRays is new Integrator with null record;

  procedure Init(self : in out PathTracerWithShadowRays);
  function  PathTrace(self : PathTracerWithShadowRays; r : Ray; prevSample : MatSample; recursion_level : Integer) return float3;


  -- path tracer with MIS
  --
  type PathTracerMIS is new Integrator with null record;

  procedure Init(self : in out PathTracerMIS);
  function  PathTrace(self : PathTracerMIS; r : Ray; prevSample : MatSample; recursion_level : Integer) return float3;


end Ray_Tracer.Integrators;


