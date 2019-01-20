module Camera
open Tracer.Math
open System

type Camera(lookFrom:Vector3, lookAt:Vector3, vup:Vector3, vfov:float, aspect:float) =
    let theta = vfov*Math.PI/180.0
    let halfHeight = Math.Tan(theta/2.0)
    let halfWidth = aspect * halfHeight
    
    let w = (lookFrom - lookAt).Normalized
    let u = (vup.Cross w).Normalized
    let v = w.Cross u


    member __.LowerLeftCorner = __.Origin - halfWidth*u - halfHeight*v - w
    member __.Horizontal = 2.0*halfWidth*u
    member __.Vertical = 2.0*halfHeight*v
    member __.Origin = lookFrom
    

    member c.GetRay u v = {Origin = c.Origin; Direction = c.LowerLeftCorner + u*c.Horizontal + v*c.Vertical - c.Origin}