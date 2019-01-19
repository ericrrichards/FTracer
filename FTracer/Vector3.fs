module Tracer

type Vector3 = 
    { X : float; Y : float; Z : float}        
    static member (~-) (v) = {X = -v.X; Y = -v.Y; Z = -v.Z}
    static member (+) (v1, v2) = { X=v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z}
    static member (-) (v1, v2) = { X=v1.X - v2.X; Y = v1.Y-v2.Y; Z = v1.Z - v2.Z}
    static member (*) (v, s) = {X = v.X*s; Y = v.Y*s; Z=v.Z*s}
    static member (*) (s, v) = {X = v.X*s; Y = v.Y*s; Z=v.Z*s}
    static member (/) (v, s) = {X = v.X/s; Y = v.Y/s; Z=v.Z/s}
    member v.Length = System.Math.Sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
    member v.SquaredLength = v.X*v.X+v.Y*v.Y+v.Z*v.Z
    member v.Normalized = 
        let length = v.Length
        {X=v.X/length; Y=v.Y/length; Z = v.Z/length}
    member v1.Dot v2 = v1.X * v2.X + v1.Y*v2.Y + v1.Z + v2.Z
    member v1.Cross v2 = { 
        X = v1.Y * v2.Z - v1.Z * v2.Y
        Y = -(v1.X * v2.Z - v1.Z * v2.X)
        Z = v1.X * v2.Y - v1.Y * v2.X
    }

type Color = 
    { R:float; G:float; B:float}
    static member (~-) (c) = {R = -c.R; G = -c.G; B = -c.B}
    static member (+) (c1, c2) = { R = c1.R + c2.R; G = c1.G + c2.G; B = c1.B + c2.B}
    static member (-) (c1, c2) = { R = c1.R - c2.R; G = c1.G - c2.G; B = c1.B - c2.B}
    static member (*) (c1, c2) =  { R = c1.R * c2.R; G = c1.G * c2.G; B = c1.B * c2.B}
    static member (/) (c1, c2) =  { R = c1.R / c2.R; G = c1.G / c2.G; B = c1.B / c2.B}
    static member (*) (c, s) =  { R = c.R * s; G = c.G * s; B = c.B * s}
    static member (*) (s, c) =  { R = c.R * s; G = c.G * s; B = c.B * s}
    static member (/) (c, s) =  { R = c.R / s; G = c.G / s; B = c.B / s}