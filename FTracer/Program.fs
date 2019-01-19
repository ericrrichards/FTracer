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
            (1.0-t)*Color.White + t*{R=0.5;G=0.7;B=1.0}               



[<EntryPoint>]
let main argv =
    let nx = 400
    let ny = 200
    let ns = 50
    let sb = StringBuilder(sprintf "P3\n%d %d\n255\n" nx ny)
    let world = HitableList ([
        {Center = -Vector3.UnitZ; Radius=0.5; Material =Lambertian({R=0.8;G=0.3;B=0.3})}; 
        {Center = {X=0.0; Y = -100.5; Z = -1.0};Radius=100.0; Material=Lambertian({R=0.8;G=0.8;B=0.0})};
        {Center={X=1.0;Y=0.0;Z = -1.0}; Radius = 0.5; Material = Metal({R=0.8;G=0.6;B=0.2}, 1.0)};
        {Center={X= -1.0; Y=0.0; Z = -1.0}; Radius = 0.5; Material = Metal({R=0.8;G=0.8;B=0.8}, 0.3)}
    ])
    let cam = Camera()
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
    
