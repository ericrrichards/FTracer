module Interfaces
open Tracer.Math

type Scatter = {Scattered:Ray; Attenuation:Color}

type IMaterial =
    abstract Scatter: Ray -> HitRecord -> option<Scatter>
and HitRecord = 
    { T:float; P:Vector3; Normal:Vector3; Material:IMaterial}

type IHitable =
    abstract Hit: Ray -> float -> float -> option<HitRecord>