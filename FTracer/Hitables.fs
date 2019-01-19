module Hitables
open Tracer.Math
open System

type HitRecord = 
    { T:float; P:Vector3; Normal:Vector3}

type IHitable =
    abstract Hit: Ray -> float -> float -> option<HitRecord>

type HitableList(hitables:List<IHitable>) = 
    member __.List = hitables
    interface IHitable with
        member this.Hit r tmin tmax = 
            let mutable closest = tmax
            let mutable ret = None
            for obj in this.List do
                let hit = obj.Hit r tmin closest
                if hit.IsSome then
                    ret <- hit
                    closest <- hit.Value.T

            ret



type Sphere = 
    {Center:Vector3; Radius:float}
    interface IHitable with
        member s.Hit r tmin tmax = 
            let oc = r.Origin - s.Center
            let a = dot r.Direction r.Direction
            let b =  dot oc r.Direction
            let c = dot oc oc - s.Radius*s.Radius
            let discriminant = b*b - a*c;

            if discriminant > 0.0 then
                let temp = (-b - Math.Sqrt(b*b-a*c))/a
                if temp < tmax && temp > tmin then
                    let p = r.PointAt temp
                    Some({T=temp; P=p; Normal = (p-s.Center)/s.Radius})
                else 
                    let temp = (-b + Math.Sqrt(b*b-a*c))/a
                    if temp < tmax && temp > tmin then
                        let p = r.PointAt temp
                        Some({T=temp; P=p; Normal = (p-s.Center)/s.Radius})
                    else 
                        None
            else 
                None