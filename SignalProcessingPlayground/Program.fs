open Playground
open XPlot.Plotly

[<EntryPoint>]
let main argv =
    let sr = 400.
    let n = 400

    let fb = Signal.sine 2. 1.
    let fc = Signal.cosine 50. 3.
    let s = fb * fc 
    let x = s + scale 2. Signal.whiteGaussian
    let y = x |> Filter.bandPass 43. 53.
    let result = y * Signal.cosine 50. 1. |> Filter.lowPass 3.

    sample sr n result
    |> Seq.zip (samplePoints sr n)
    |> Chart.Line
    |> Chart.Show

    sample sr n result
    |> fft
    |> Seq.zip (fftPoints sr n)
    |> Chart.Line
    |> Chart.Show

    0 // return an integer exit code