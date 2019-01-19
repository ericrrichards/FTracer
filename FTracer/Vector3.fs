﻿module Tracer

open System

type Vector3 = 
    { X : float; Y : float; Z : float}        
    static member (~-) (v) = {X = -v.X; Y = -v.Y; Z = -v.Z}
    static member (+) (v1, v2) = { X=v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z}
    static member (-) (v1, v2) = { X=v1.X - v2.X; Y = v1.Y-v2.Y; Z = v1.Z - v2.Z}
    static member (*) (v, s) = {X = v.X*s; Y = v.Y*s; Z=v.Z*s}
    static member (*) (s, v) = {X = v.X*s; Y = v.Y*s; Z=v.Z*s}
    static member (/) (v, s) = {X = v.X/s; Y = v.Y/s; Z=v.Z/s}
    member v1.Dot v2 = v1.X * v2.X + v1.Y*v2.Y + v1.Z * v2.Z
    member v1.Cross v2 = { 
        X = v1.Y * v2.Z - v1.Z * v2.Y
        Y = -(v1.X * v2.Z - v1.Z * v2.X)
        Z = v1.X * v2.Y - v1.Y * v2.X
    }
    member v.Length = System.Math.Sqrt(v.Dot(v))
    member v.SquaredLength = v.Dot(v)
    member v.Normalized = 
        let length = v.Length
        {X=v.X/length; Y=v.Y/length; Z = v.Z/length}    

    static member Zero = {X = 0.0; Y = 0.0; Z = 0.0}
    static member UnitX = {X=1.0;Y=0.0;Z=0.0}
    static member UnitY = {X=0.0;Y=1.0;Z=0.0}
    static member UnitZ = {X=0.0;Y=0.0;Z=1.0}

let dot (v1:Vector3) v2 = v1.Dot(v2)

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
    static member White = {R=1.0;G=1.0;B=1.0}
    static member Black = {R=0.0;G=0.0;B=0.0}
    static member Red = {R=1.0;G=0.0;B=0.0}

type Ray = 
    {Origin:Vector3; Direction:Vector3}
    member r.PointAt t = r.Origin + t*r.Direction

type Sphere = 
    {Center:Vector3; Radius:float}
    member c.Hit r = 
        let oc = r.Origin - c.Center
        let a = dot r.Direction r.Direction
        let b = 2.0 * dot oc r.Direction
        let c = dot oc oc - c.Radius*c.Radius
        let discriminant = b*b - 4.0*a*c;
        discriminant > 0.0