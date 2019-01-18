// Learn more about F# at http://fsharp.org

open System
open System.Text
open System.IO
open System.Diagnostics

[<EntryPoint>]
let main argv =
    let nx = 200
    let ny = 100
    let sb = StringBuilder(sprintf "P3\n%d %d\n255\n" nx ny)
    for j = ny-1 downto 0 do
        //printf "\n%d\n" j
        for i = 0 to nx-1 do
            //printf "%d," i
            let r = float(i)/float(nx)
            let g = float(j)/float(ny)
            let b = 0.2
            let ir = int(255.99*r)
            let ig = int(255.99*g)
            let ib = int(255.99*b)
            sb.Append(sprintf "%d %d %d\n" ir ig ib) |> ignore


    File.WriteAllText("out.ppm", sb.ToString())
    0
    
