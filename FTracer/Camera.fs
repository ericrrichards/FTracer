module Camera
open Tracer.Math

type Camera() =
    member __.LowerLeftCorner = vec(-2.0,-1.0,-1.0)
    member __.Horizontal = vec(4.0,0.0, 0.0)
    member __.Vertical = vec(0.0,2.0,0.0)
    member __.Origin = Vector3.Zero

    member c.GetRay u v = {Origin = c.Origin; Direction = c.LowerLeftCorner + u*c.Horizontal + v*c.Vertical - c.Origin}