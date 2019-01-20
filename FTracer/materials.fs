module Materials
open Tracer.Math
open Interfaces
open System


let rand1 = Random()
// NOTE: need to have the parentheses here, or else this is a value, and isn't re-evaluated each time that it is called
// which is confusing as all hell and wasted an hour or more of fiddle-frigging
let randomInUnitSphere() =
    
    let mutable p = 2.0*vec(rand1.NextDouble(),rand1.NextDouble(),rand1.NextDouble()) - vec(1.0,1.0,1.0)
    while p.SquaredLength >= 1.0 do
        p <- 2.0*vec(rand1.NextDouble(),rand1.NextDouble(),rand1.NextDouble()) - vec(1.0,1.0,1.0)
    p

let reflect v n = 
    v - 2.0 * (dot v n) * n

let refract (v:Vector3) n niOverNt = 
    let uv = v.Normalized
    let dt = dot uv n
    let discriminant = 1.0 - niOverNt*niOverNt*(1.0-dt*dt)
    if (discriminant>0.0) then
        Some(niOverNt*(uv-n*dt) - n*Math.Sqrt(discriminant))
    else
        None
let schlick cosine refIdx = 
    let r0 = (1.0-refIdx) / (1.0 + refIdx)
    let r0 = r0*r0
    r0 + (1.0-r0)*Math.Pow((1.0-cosine), 5.0)


type Lambertian(Albedo:Color)=
    member __.Albedo = Albedo
    interface IMaterial with
        member l.Scatter r_in hit = 
            let target = hit.P + hit.Normal + randomInUnitSphere()
            Some {Scattered = {Origin = hit.P; Direction = target-hit.P}; Attenuation=l.Albedo} 

type Metal(Albedo:Color, fuzz:float)=
    member __.Albedo = Albedo
    member __.Fuzz = if fuzz < 1.0 then fuzz else 1.0
        
    interface IMaterial with
        member m.Scatter r_in hit = 
            let reflected = reflect r_in.Direction.Normalized hit.Normal
            let scattered = {Origin = hit.P; Direction = reflected + m.Fuzz*randomInUnitSphere()}
            if dot scattered.Direction hit.Normal > 0.0 then
                Some {Scattered = scattered; Attenuation = m.Albedo}
            else
                None

type Dialectric(ri:float) =
    member __.RefractiveIndex = ri
    member __.Rand = Random()
    interface IMaterial with
        member d.Scatter r_in hit = 
            let reflected = reflect r_in.Direction hit.Normal
            let outwardNormal, niOverNt, cosine = 
                if (dot r_in.Direction hit.Normal) > 0.0 then
                    -hit.Normal, d.RefractiveIndex, d.RefractiveIndex * (dot r_in.Direction hit.Normal) / r_in.Direction.Length
                else 
                    hit.Normal, 1.0 / d.RefractiveIndex, -(dot r_in.Direction hit.Normal) / r_in.Direction.Length
            let reflectProb, refracted = match refract r_in.Direction outwardNormal niOverNt with
                | Some ref -> schlick cosine d.RefractiveIndex, ref
                | None -> 1.0, Vector3.Zero
            if d.Rand.NextDouble() < reflectProb then
                Some({Scattered = {Origin = hit.P; Direction=reflected}; Attenuation=Color.White})
            else 
                Some({Scattered = {Origin = hit.P; Direction=refracted}; Attenuation=Color.White})