open System
type pstate = (int*int) list

let pixOn(x:int,y:int, pixels:pstate) : pstate =
    (x,y) :: pixels

let printFrame (s:pstate) = 
    let rec exp (_s:pstate) = 
        match _s with  
        | [] -> ""
        | p :: tail -> 
        sprintf "{%d, %d}, %s" (fst p) (snd p) (exp tail)
    sprintf "{ \n{0,%d}, %s\n}" s.Length (exp s)
    

let extractState (filename:string) : pstate = 
    let Img = new Drawing.Bitmap(filename)
    let mutable pixels = []
    for i in 0..(Img.Height-1) do
        for j in 0..(Img.Width-1) do 
            if (Img.GetPixel (j,i)).R < byte(0xff) then
                pixels <- pixOn(j,i,pixels)
    pixels
let printAnimationCode (frames: string list) = 
    List.fold (
        sprintf "%s%s,\n" 
    ) "" frames

[<EntryPoint>]
let _Main(argv) =
    let frames = 3
    List.fold(fun acc i -> 
        (i
        |>sprintf "spiral/spiral%02d.png"
        |> extractState
        |> printFrame ) :: acc
    ) [] [0 .. 24]
    |> List.rev
    |> printAnimationCode
    |> printf "%s"

    0
