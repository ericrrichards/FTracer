module BVHNode

open Interfaces
open System

let boxComparerX (a:IHitable) (b:IHitable) =
    let leftBox = a.BoundingBox 0.0 0.0
    let rightBox = b.BoundingBox 0.0 0.0
    if (leftBox = None || rightBox = None) then
        failwith "no bounding box in bvh_node constructor"
    if (leftBox.Value.Min.X - rightBox.Value.Min.X < 0.0) then
        -1
    else
        1
    
let boxComparerY (a:IHitable) (b:IHitable) =
    let leftBox = a.BoundingBox 0.0 0.0
    let rightBox = b.BoundingBox 0.0 0.0
    if (leftBox = None || rightBox = None) then
        failwith "no bounding box in bvh_node constructor"
    if (leftBox.Value.Min.Y - rightBox.Value.Min.Y < 0.0) then
        -1
    else
        1
let boxComparerZ (a:IHitable) (b:IHitable) =
    let leftBox = a.BoundingBox 0.0 0.0
    let rightBox = b.BoundingBox 0.0 0.0
    if (leftBox = None || rightBox = None) then
        failwith "no bounding box in bvh_node constructor"
    if (leftBox.Value.Min.Z - rightBox.Value.Min.Z < 0.0) then
        -1
    else
        1

let rand = Random()
type BVHNode(l: List<IHitable>, t0, t1) =
    let axis = rand.Next(0,3)

    let sorted = 
        if axis = 0 then
            l|> List.sortWith boxComparerX
        elif axis = 1 then
            l|> List.sortWith boxComparerY
        else
            l|> List.sortWith boxComparerZ

    let getNodes (l:List<IHitable>) t0 t1 : (IHitable*IHitable)= 
        if l.Length = 1 then
            l.[0], l.[0]
        elif l.Length = 2 then
            l.[0], l.[1]
        else
            BVHNode( l|> List.take (l.Length/2), t0, t1):>IHitable, BVHNode(l|> List.skip (l.Length/2), t0, t1) :>IHitable 

    let (left:IHitable, right:IHitable) = getNodes sorted t0 t1
    let box = 
        if (left.BoundingBox t0 t1).IsNone || (right.BoundingBox t0 t1).IsNone then
            failwith "No bounding box for one node!"
        else
            AABB.surroundingBox (left.BoundingBox t0 t1).Value (right.BoundingBox t0 t1).Value

    member __.Left = left
    member __.Right = right

    member __.Box = box
    
    interface IHitable with
        member n.BoundingBox t0 t1 = 
            Some(n.Box)
        member n.Hit r tmin tmax =
            if (n.Box.Hit r tmin tmax) = false then
                None
            else
                let hitLeft = n.Left.Hit r tmin tmax
                let hitRight = n.Right.Hit r tmin tmax
                if hitLeft.IsSome && hitRight.IsSome then
                    if hitLeft.Value.T < hitRight.Value.T then
                        hitLeft
                    else
                        hitRight
                elif hitLeft.IsSome then
                    hitLeft
                elif hitRight.IsSome then
                    hitRight
                else 
                    None