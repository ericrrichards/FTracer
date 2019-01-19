// Learn more about F# at http://fsharp.org

open System
open System.Text
open System.IO
open System.Diagnostics
open Tracer


let color r = 
    let unitDirection = r.Direction.Normalized
    let t = 0.5 * (unitDirection.Y + 1.0)
    (1.0-t)*Color.White + t*{R=0.5;G=0.7;B=1.0}



[<EntryPoint>]
let main argv =
    let nx = 200
    let ny = 100
    let sb = StringBuilder(sprintf "P3\n%d %d\n255\n" nx ny)
    let lowerLeft = {X = -2.0; Y = -1.0; Z = -1.0}
    let horizontal = {X=4.0; Y = 0.0; Z=0.0}
    let vertical = {X = 0.0; Y = 2.0; Z = 0.0}
    let origin = Vector3.Zero
    for j = ny-1 downto 0 do
        for i = 0 to nx-1 do

            let u = float(i)/float(nx)
            let v = float(j)/float(ny)

            let r = {Origin = origin; Direction = lowerLeft + u*horizontal + v*vertical}

            let col = color r
            
            let ir = int(255.99*col.R)
            let ig = int(255.99*col.G)
            let ib = int(255.99*col.B)
            sb.Append(sprintf "%d %d %d\n" ir ig ib) |> ignore


    File.WriteAllText("out.ppm", sb.ToString())
    Process.Start(ProcessStartInfo("out.ppm",UseShellExecute = true)) |> ignore
    0
    
