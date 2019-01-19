// Learn more about F# at http://fsharp.org

open System
open System.Text
open System.IO
open System.Diagnostics
open Tracer

[<EntryPoint>]
let main argv =
    let nx = 200
    let ny = 100
    let sb = StringBuilder(sprintf "P3\n%d %d\n255\n" nx ny)
    for j = ny-1 downto 0 do
        for i = 0 to nx-1 do
            let col = {R = float(i)/float(nx); G=float(j)/float(ny); B= 0.2}
            
            let ir = int(255.99*col.R)
            let ig = int(255.99*col.G)
            let ib = int(255.99*col.B)
            sb.Append(sprintf "%d %d %d\n" ir ig ib) |> ignore


    File.WriteAllText("out.ppm", sb.ToString())
    Process.Start(ProcessStartInfo("out.ppm",UseShellExecute = true)) |> ignore
    0
    
