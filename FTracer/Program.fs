// Learn more about F# at http://fsharp.org

open System
open System.Text
open System.IO
open System.Diagnostics
open Tracer.Math
open Hitables
open Camera
open Interfaces
open Materials






let rec color r (world:IHitable) (depth:int) = 
    let hit = world.Hit r 0.001 System.Double.MaxValue

    match hit with
        |Some h ->  
            if depth > 50 then 
                Color.Black
            else
                match h.Material.Scatter r h with
                    | Some s -> (s.Attenuation * (color s.Scattered world (depth+1)))
                    | None -> Color.Black
        |None ->
            let unitDirection = r.Direction.Normalized
            let t = 0.5 * (unitDirection.Y + 1.0)
            (1.0-t)*Color.White + t* rgb(0.5,0.7,1.0)              



[<EntryPoint>]
let main argv =
    let nx = 400
    let ny = 200
    let ns = 100
    let sb = StringBuilder(sprintf "P3\n%d %d\n255\n" nx ny)
    let r = Math.Cos(Math.PI/4.0)
    let world = HitableList ([
        //Sphere(vec(-r,0.0, -1.0), r, Lambertian(rgb(0.0,0.0,1.0)))
        //Sphere(vec(r,0.0,-1.0), r, Lambertian(rgb(1.0,0.0,0.0)))
        Sphere(-Vector3.UnitZ, 0.5,Lambertian(rgb(0.1,0.2,0.5))); 
        Sphere(vec(0.0,-100.5,-1.0),100.0, Lambertian(rgb(0.8,0.8,0.0)));
        Sphere(vec(1.0,0.0,-1.0), 0.5, Metal(rgb(0.8,0.6,0.2), 0.0));
        Sphere(vec(-1.0,0.0,-1.0), 0.5, Dialectric(1.5))
        Sphere(vec(-1.0,0.0,-1.0), -0.45, Dialectric(1.5))
    ])
    
    let cam = Camera(vec(-2.0, 2.0, 1.0), vec(0.0, 0.0, -1.0), Vector3.UnitY, 30.0, float(nx)/float(ny))
    let rand = Random()
    for j = ny-1 downto 0 do
        for i = 0 to nx-1 do
            let mutable col = Color.Black
            for s = 0 to ns-1 do
                let u = (float(i) + rand.NextDouble())/float(nx)
                let v = (float(j) + rand.NextDouble())/float(ny)

                let r = cam.GetRay u v

                col <- col + (color r world 0)
            col <- col/float(ns)
            let gCol = col.GammaCorrect
            let ir = int(255.99*gCol.R)
            let ig = int(255.99*gCol.G)
            let ib = int(255.99*gCol.B)
            sb.Append(sprintf "%d %d %d\n" ir ig ib) |> ignore


    File.WriteAllText("out.ppm", sb.ToString())
    Process.Start(ProcessStartInfo("out.ppm",UseShellExecute = true)) |> ignore
    0
    
