module Hitables
open Tracer.Math
open System
open Interfaces
open AABB


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
        member this.BoundingBox t0 t1 =
            if this.List.Length < 1 then
                None
            else 
                let mutable tempBox = None
                tempBox <- this.List.Head.BoundingBox t0 t1
                if tempBox = None then
                    None
                else 
                    for i = 1 to this.List.Length do
                        tempBox <- Some(surroundingBox ((this.List.Item i).BoundingBox t0 t1).Value tempBox.Value)
                    tempBox

            




type Sphere(Center:Vector3, Radius:float, Material:IMaterial) = 
    member __.Center = Center
    member __.Radius = Radius
    member __.Material = Material
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
                    Some({T=temp; P=p; Normal = (p-s.Center)/s.Radius; Material =s.Material})
                else 
                    let temp = (-b + Math.Sqrt(b*b-a*c))/a
                    if temp < tmax && temp > tmin then
                        let p = r.PointAt temp
                        Some({T=temp; P=p; Normal = (p-s.Center)/s.Radius; Material = s.Material})
                    else 
                        None
            else 
                None
        member s.BoundingBox t0 t1 = 
            Some(AABB(s.Center - vec(s.Radius,s.Radius,s.Radius), s.Center + vec(s.Radius,s.Radius,s.Radius)))

type MovingSphere(center0:Vector3, center1:Vector3, Radius:float, Material:IMaterial, t0:float, t1:float) = 
    member __.Center0 = center0
    member __.Center1 = center1
    member __.Time0 = t0
    member __.Time1 = t1
    member s.Center t =
        s.Center0 + ((t - s.Time0)/(s.Time1-s.Time0))*(s.Center1-s.Center0)
    member __.Radius = Radius
    member __.Material = Material
    interface IHitable with
        member s.Hit r tmin tmax = 
            let oc = r.Origin - s.Center r.Time
            let a = dot r.Direction r.Direction
            let b =  dot oc r.Direction
            let c = dot oc oc - s.Radius*s.Radius
            let discriminant = b*b - a*c;

            if discriminant > 0.0 then
                let temp = (-b - Math.Sqrt(b*b-a*c))/a
                if temp < tmax && temp > tmin then
                    let p = r.PointAt temp
                    Some({T=temp; P=p; Normal = (p-s.Center r.Time)/s.Radius; Material =s.Material})
                else 
                    let temp = (-b + Math.Sqrt(b*b-a*c))/a
                    if temp < tmax && temp > tmin then
                        let p = r.PointAt temp
                        Some({T=temp; P=p; Normal = (p-s.Center r.Time)/s.Radius; Material = s.Material})
                    else 
                        None
            else 
                None
        member s.BoundingBox t0 t1 = 
            let b0 = AABB(s.Center(t0) - vec(s.Radius, s.Radius, s.Radius), s.Center(t0) + vec(s.Radius, s.Radius, s.Radius))
            let b1 = AABB(s.Center(t1) - vec(s.Radius, s.Radius, s.Radius), s.Center(t1) + vec(s.Radius, s.Radius, s.Radius))
            Some(surroundingBox b0 b1)