module Camera
open Math

type Camera() =
    member __.LowerLeftCorner = {X = -2.0;Y = -1.0;Z = -1.0}
    member __.Horizontal = {X = 4.0; Y = 0.0; Z = 0.0}
    member __.Vertical = {X = 0.0; Y = 2.0; Z = 0.0}
    member __.Origin = Vector3.Zero

    member c.GetRay u v = {Origin = c.Origin; Direction = c.LowerLeftCorner + u*c.Horizontal + v*c.Vertical - c.Origin}