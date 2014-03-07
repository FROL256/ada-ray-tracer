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

  type PathResult is record
    color    : float3  := (0.0, 0.0, 0.0);
    dist     : float   := 0.0;
    lightCos : float   := 0.0;
    hitLight : boolean := false;
  end record;


  -- integrators
  --
  type Integrator is abstract tagged record
    gen : RandRef := null;
  end record;

  type IntegratorRef is access Integrator'Class;

  function PathTrace(self : Integrator; r : Ray; recursion_level : Integer) return PathResult is abstract;
  procedure Init(self : in out Integrator) is abstract;
  procedure DoPass(self : in out Integrator; colBuff : AccumBuffRef);


  -- stupid path tracer
  --
  type SimplePathTracer is new Integrator with null record;

  procedure Init(self : in out SimplePathTracer);
  function PathTrace(self : SimplePathTracer; r : Ray; recursion_level : Integer) return PathResult;

  -- path tracer with shadow rays
  --
  type PathTracerWithShadowRays is new Integrator with null record;

  procedure Init(self : in out PathTracerWithShadowRays);
  function PathTrace(self : PathTracerWithShadowRays; r : Ray; recursion_level : Integer) return PathResult;


  -- path tracer with MIS
  --
  type PathTracerMIS is new Integrator with null record;

  procedure Init(self : in out PathTracerMIS);
  function PathTrace(self : PathTracerMIS; r : Ray; recursion_level : Integer) return PathResult;


  -- simple MLT implementation copying image
  --
  type MLTCopyImage is new Integrator with record
    mltHist : AccumBuffRef  := null;
    brightnessEstim : float := 0.0;
    mutationsPerPixel : integer := g_mltMutationsPerPixel;
  end record;

  procedure Init(self : in out MLTCopyImage);
  function PathTrace(self : MLTCopyImage; r : Ray; recursion_level : Integer) return PathResult;

  procedure DoPass(self : in out MLTCopyImage; colBuff : AccumBuffRef);


  -- MLT path tracing; no shadow rays, no MIS
  --
  type MLTSimple is new SimplePathTracer with record
    mltHist : AccumBuffRef  := null;
    brightnessEstim : float := 0.0;
    mutationsPerPixel : integer := g_mltMutationsPerPixel;
  end record;


  function PathTrace(self : MLTSimple; r : Ray; recursion_level : Integer) return PathResult;
  procedure Init(self : in out MLTSimple);
  procedure DoPass(self : in out MLTSimple; colBuff : AccumBuffRef);


  type KMLT_Generator is new RandomGenerator with null record;

  function rnd_uniform(gen : access KMLT_Generator; l,h : float) return float;
  procedure ResetSequenceCounter(gen : in out KMLT_Generator);
  procedure InitSequence(gen : in out KMLT_Generator);


  function  IsStackEmpty(gen : in KMLT_Generator) return boolean;
  procedure Push(gen : in out KMLT_Generator; i : in integer; val : in float);
  procedure Pop(gen : in out KMLT_Generator; i : out integer; val : out float);
  procedure ClearStack(gen : in out KMLT_Generator);
  procedure RestoreSequence(gen : in out KMLT_Generator);
  procedure ResetAllModifyCounters(gen : in out KMLT_Generator);


  function  Mutate(gen : in KMLT_Generator; a_value : float) return float;
  procedure PrimarySample(gen : in out KMLT_Generator; i : in integer; time : in integer; res : out float);

  procedure NextSample(gen           : in out KMLT_Generator;
                       I             : in float;
                       oldI          : in out float;
                       totalSamples  : in integer;
                       contrib       : in float3;
                       oldsample     : in out Sample;
                       contribsample : out Sample);



  -- simple Kelmen style MLT; no shadow rays, no MIS
  --

  type MLTKelmenSimple is new PathTracerMIS with record
    mltHist : AccumBuffRef  := null;
    brightnessEstim : float := 0.0;
    mutationsPerPixel : integer := g_mltMutationsPerPixel;
  end record;


  function PathTrace(self : MLTKelmenSimple; r : Ray; recursion_level : Integer) return PathResult;
  procedure Init(self : in out MLTKelmenSimple);
  procedure DoPass(self : in out MLTKelmenSimple; colBuff : AccumBuffRef);




end Ray_Tracer.Integrators;


