module Camera
open Tracer.Math
open System

let rand = new Random()
let randomInUnitDisk() = 
    let mutable p = 2.0*vec(rand.NextDouble(),rand.NextDouble(),0.0) - vec(1.0,1.0,0.0)
    while (dot p p) >= 1.0 do
        p <- 2.0*vec(rand.NextDouble(),rand.NextDouble(),0.0) - vec(1.0,1.0,0.0)
    p


type Camera(lookFrom:Vector3, lookAt:Vector3, vup:Vector3, vfov:float, aspect:float, aperature:float, focusDist:float, t0:float, t1:float) =
    let theta = vfov*Math.PI/180.0
    let halfHeight = Math.Tan(theta/2.0)
    let halfWidth = aspect * halfHeight

    member __.time0 = t0
    member __.time1 = t1
    member __.Origin = lookFrom
    member __.w = (lookFrom - lookAt).Normalized
    member __.u = (vup.Cross __.w).Normalized
    member __.v= __.w.Cross __.u

    member __.LensRadius = aperature/2.0

    member __.LowerLeftCorner = __.Origin - halfWidth * focusDist * __.u - halfHeight * focusDist * __.v - focusDist * __.w
    member __.Horizontal = 2.0 * halfWidth * focusDist * __.u
    member __.Vertical = 2.0 * halfHeight * focusDist * __.v
    
    

    member c.GetRay s t = 
        let rd = c.LensRadius * randomInUnitDisk()
        let offset = c.u*rd.X + c.v*rd.Y
        let time = c.time0 + rand.NextDouble() * (c.time1-c.time0)
        Ray(c.Origin+offset, c.LowerLeftCorner + s*c.Horizontal + t*c.Vertical - c.Origin-offset, time)