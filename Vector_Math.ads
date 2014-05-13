with Interfaces;
with Ada.Numerics.Float_Random;
with Generic_Vector_Math;

use Interfaces;

package Vector_Math is

  -- float math
  --
  package Float_Math is new Generic_Vector_Math (float);

  function safe_tan(x : float) return float;
  function sign(x : float) return float;
  function pow(Left, Right : float) return float;

  infinity : constant float := float'Last;

  M_PI   : constant float := Ada.Numerics.Pi;
  INV_PI : constant float := 1.0/Ada.Numerics.Pi;

  type float2 is new Float_Math.vector2;
  type float3 is new Float_Math.vector3;
  type float4 is new Float_Math.vector4;
  type float4x4 is new Float_Math.Matrix4;

  function sqr(x : float) return float renames Float_Math.sqr;
  function min(a, b : float) return float renames Float_Math.min;
  function max(a, b : float) return float renames Float_Math.max;
  function min(a, b, c : float) return float renames Float_Math.min;
  function max(a, b, c : float) return float renames Float_Math.max;
  function clamp(x,a,b : float) return float renames Float_Math.clamp;

  function length(a : float3) return float;
  function normalize (a : float3) return float3;
  function reflect(dir : float3; normal: float3) return float3;

  function min(a, b : Float_Math.vector3) return Float_Math.vector3 renames Float_Math.min;
  function max(a, b : Float_Math.vector3) return Float_Math.vector3 renames Float_Math.max;
  function clamp(x  : Float_Math.vector3; a,b : float) return Float_Math.vector3 renames Float_Math.clamp;
  function clamp(x,a,b : Float_Math.vector3) return Float_Math.vector3 renames Float_Math.clamp;

  function RotationMatrix(angle : float; a_v : float3) return float4x4;
  function LookAtMatrix(eye, center, up : float3) return float4x4;

  --function "*"(m1 : float4x4; m2 : float4x4) return float4x4 renames Float_Math."*";

  function "*"(m : float4x4; v : float3) return float3;
  function TransformNormal(m : float4x4; n : float3) return float3;


  IdentityMatrix : constant float4x4 := ((1.0,0.0,0.0,0.0),
                                         (0.0,1.0,0.0,0.0),
                                         (0.0,0.0,1.0,0.0),
                                         (0.0,0.0,0.0,1.0));

  -- integer math
  --
  package Integer_Math is new Generic_Vector_Math (integer);

  type int3 is new Integer_Math.vector3;
  type int4 is new Integer_Math.vector4;

  --function sqr(x : integer) return float renames Integer_Math.sqr;
  function min(a, b : integer) return integer renames Integer_Math.min;
  function max(a, b : integer) return integer renames Integer_Math.max;
  function min(a, b, c : integer) return integer renames Integer_Math.min;
  function max(a, b, c : integer) return integer renames Integer_Math.max;
  function clamp(x,a,b : integer) return integer renames Integer_Math.clamp;

  pragma Inline (normalize);
  pragma Inline (length);
  pragma Inline (reflect);
  pragma Inline (sqr);
  pragma Inline (min);
  pragma Inline (max);
  pragma Inline (clamp);





  -- rand gen
  --

  -- this random generator should replace simple random for Kelmen-style MLT
  --
  QMC_KMLT_MAXRANDS : constant := 64;
  type vector32i is array (0..QMC_KMLT_MAXRANDS-1) of integer;
  type vector32f is array (0..QMC_KMLT_MAXRANDS-1) of float;

  type RandomGenerator is tagged limited record

    agen : Ada.Numerics.Float_Random.Generator;

    -- samples array
    --
    modify  : vector32i := (others => 0); -- stores the global time when this coordinate was modified most recently
    values  : vector32f := (others => 0.0);
    u_id    : integer   := 0;

    -- samples atack
    --
    indices_stack : vector32i := (others => 0);
    values_stack  : vector32f := (others => 0.0);
    top           : integer   := 0;

    -- 'global' variables
    --
    time            : integer := 1;            -- Let us define a counter called time for the global time of the process which counts the number of accepted mutations
    large_step      : integer := 1;            -- variable large_step is 1 if a large step is made and zero otherwise
    large_step_time : integer := 1;      -- The time of the last accepted large step is stored in variable large_step_time

  end record;

  procedure ResetSequenceCounter(gen : in out RandomGenerator);
  procedure InitSequence(gen : in out RandomGenerator);
  procedure RestoreSequence(gen : in out RandomGenerator);
  procedure ClearStack(gen : in out RandomGenerator);
  procedure ResetAllModifyCounters(gen : in out RandomGenerator);

  function rnd_uniform(gen : access RandomGenerator; l,h : float) return float;
  function rnd_uniform_simple(gen : RandomGenerator; l,h : float) return float;

  function MapSampleToCosineDist(r1,r2 : float; direction, normal : float3; power : float) return float3;
  function MapSampleToCosineDistFixed(r1,r2 : float; direction, normal : float3; power : float) return float3;

  type MLTSample is record
    contrib : float3  := (0.0, 0.0, 0.0);
    w       : float   := 0.0;
    x,y     : integer := 0;
  end record;

  procedure NextSample(gen           : in out RandomGenerator;
                       I             : in float;
                       oldI          : in out float;
                       totalSamples  : in integer;
                       contrib       : in float3;
                       oldsample     : in out MLTSample;
                       contribsample : out MLTSample);

  type RandRef is access all RandomGenerator'Class;


  function RandomCosineVectorOf(gen : RandRef; norm : float3) return float3;
  function RandomCosineVectorOf(gen : RandRef; refl : float3;  norm : float3; cosPower : float) return float3;

end Vector_Math;
