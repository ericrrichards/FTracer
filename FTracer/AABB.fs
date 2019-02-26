module AABB
open System
open Tracer.Math

type AABB(min:Vector3, max:Vector3) = 
    member __.Min = min
    member __.Max = max

    member private a.axisHit (r:Ray) i tmin tmax= 
        let t0 = Math.Min((a.Min.[i] - r.Origin.[i]) / r.Direction.[i], (a.Max.[i] - r.Origin.[i])/r.Direction.[i])
        let t1 = Math.Max((a.Min.[i] - r.Origin.[i]) / r.Direction.[i], (a.Max.[i] - r.Origin.[i])/r.Direction.[i])
        let tmin = Math.Max(t0, tmin)
        let tmax = Math.Min(t1, tmax)
        if tmax < tmin then 
            false
        else true

    member a.Hit r tmin tmax =
        if (a.axisHit r 0 tmin tmax) = false then
            false
        elif (a.axisHit r 1 tmin tmax) = false then
            false
        elif (a.axisHit r 2 tmin tmax) = false then
            false
        else true
let surroundingBox (b0:AABB) (b1:AABB) = 
    let small = vec(Math.Min(b0.Min.X, b1.Min.X), Math.Min(b0.Min.Y, b1.Min.Y), Math.Min(b0.Min.Z, b1.Min.Z))
    let big = vec(Math.Max(b0.Max.X, b1.Max.X), Math.Max(b0.Max.Y, b1.Max.Y), Math.Max(b0.Max.Z, b1.Max.Z))
    AABB(small, big)
    
