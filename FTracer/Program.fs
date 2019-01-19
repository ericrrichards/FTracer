// Learn more about F# at http://fsharp.org

open System
open System.Text
open System.IO
open System.Diagnostics
open Tracer.Math
open Hitables
open Camera


let rand1 = Random()
let randomInUnitSphere() =
    
    let mutable p = 2.0*{X = rand1.NextDouble(); Y = rand1.NextDouble(); Z = rand1.NextDouble()} - {X=1.0;Y=1.0;Z=1.0}
    while p.SquaredLength >= 1.0 do
        p <- 2.0*{X = rand1.NextDouble(); Y = rand1.NextDouble(); Z = rand1.NextDouble()} - {X=1.0;Y=1.0;Z=1.0}
    p



let rec color r (world:IHitable) = 
    let hit = world.Hit r 0.0 100000.0

    match hit with
        |Some h ->            
            let target = h.P + h.Normal + randomInUnitSphere()
            let scatter = {Origin = h.P; Direction = target - h.P}
            0.5 * (color scatter world)
            //0.5 * {R= h.Normal.X+1.0; G = h.Normal.Y+1.0; B = h.Normal.Z + 1.0}
        |None ->
            let unitDirection = r.Direction.Normalized
            let t = 0.5 * (unitDirection.Y + 1.0)
            (1.0-t)*Color.White + t*{R=0.5;G=0.7;B=1.0}               



[<EntryPoint>]
let main argv =
    let nx = 200
    let ny = 100
    let ns = 50
    let sb = StringBuilder(sprintf "P3\n%d %d\n255\n" nx ny)    
    let sphere = {Center = -Vector3.UnitZ; Radius=0.5}
    let sphere2 = {Center = {X=0.0; Y = -100.5; Z = -1.0};Radius=100.0}
    let world = HitableList ([sphere; sphere2])
    let cam = Camera()
    let rand = Random()
    for j = ny-1 downto 0 do
        for i = 0 to nx-1 do
            let mutable col = Color.Black
            for s = 0 to ns-1 do
                let u = (float(i) + rand.NextDouble())/float(nx)
                let v = (float(j) + rand.NextDouble())/float(ny)

                let r = cam.GetRay u v

                col <- col + (color r world)
            col <- col/float(ns)
            let gCol = col.GammaCorrect
            let ir = int(255.99*gCol.R)
            let ig = int(255.99*gCol.G)
            let ib = int(255.99*gCol.B)
            sb.Append(sprintf "%d %d %d\n" ir ig ib) |> ignore


    File.WriteAllText("out.ppm", sb.ToString())
    Process.Start(ProcessStartInfo("out.ppm",UseShellExecute = true)) |> ignore
    0
    
