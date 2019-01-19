module Hitables
open Math
open System

type HitRecord = 
    { T:float; P:Vector3; Normal:Vector3}

type IHitable =
    abstract Hit: Ray -> float -> float -> option<HitRecord>

type HitableList(hitables:List<IHitable>) = 
    member __.List = hitables
    interface IHitable with
        member this.Hit r tmin tmax = 
            let objs = this.List |> List.map (fun h -> h.Hit r tmin tmax )
            let hits = List.choose id objs
            if hits.IsEmpty then
                None
            else
                Some(List.minBy (fun h->h.T) hits)


type Sphere = 
    {Center:Vector3; Radius:float}
    interface IHitable with
        member this.Hit r tmin tmax = 
            let oc = r.Origin - this.Center
            let a = dot r.Direction r.Direction
            let b =  dot oc r.Direction
            let c = dot oc oc - this.Radius*this.Radius
            let discriminant = b*b - a*c;

            if discriminant > 0.0 then
                let temp = (-b - Math.Sqrt(b*b-a*c))/a
                if temp < tmax && temp > tmin then
                    let p = r.PointAt temp
                    Some({T=temp; P=p; Normal = (p-this.Center)/this.Radius})
                else 
                    let temp = (-b + Math.Sqrt(b*b-a*c))/a
                    if temp < tmax && temp > tmin then
                        let p = r.PointAt temp
                        Some({T=temp; P=p; Normal = (p-this.Center)/this.Radius})
                    else 
                        None
            else 
                None