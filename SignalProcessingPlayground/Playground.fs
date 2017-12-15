module Playground

open System
open MathNet.Numerics
open MathNet.Filtering
open MathNet.Numerics.IntegralTransforms

type Signal = { Generator: float -> int -> float[]; Scalar: float; }

// sample a signal *s* under sample rate *sr* and yield *n* points
let sample sr n s =
    s.Generator sr n |> Array.map (fun x -> x * s.Scalar)

// sacle a signal *s* by a coeffcient *k*
let scale k s =
    { s with Scalar = k * s.Scalar }

// zip two signal *s1* and *s2* with an operator *op*
let zip op s1 s2 =
    let generator = fun sr n -> Array.map2 op (sample sr n s1) (sample sr n s2)
    { Generator = generator; Scalar = 1. }

type Signal with
    static member (~-) s = scale -1. s

    static member (+) (lhs, rhs) = zip (+) lhs rhs
    static member (-) (lhs, rhs) = zip (-) lhs rhs
    static member (*) (lhs, rhs) = zip (*) lhs rhs
    static member (/) (lhs, rhs) = zip (/) lhs rhs

module Signal =
    let arbitrarySignal func scalar =
        let generator = fun sr n -> Array.init n (fun i -> (float i) / sr |> func)
        { Generator = generator; Scalar = scalar; }

    let zero scalar = arbitrarySignal (fun _ -> 0.) scalar
    let step scalar = arbitrarySignal (fun _ -> 1.) scalar

    let sine freq scalar =
        let f = fun t -> sin (2.*Math.PI*freq*t)
        arbitrarySignal f scalar
    let cosine freq scalar =
        let f = fun t -> cos (2.*Math.PI*freq*t)
        arbitrarySignal f scalar

    let whiteGaussian = 
        let source = DataSources.WhiteGaussianNoiseSource()
        arbitrarySignal (fun _ -> source.ReadNextSample()) 1.

type Filter = Signal -> Signal
module Filter =
    let arbitraryFilter transform signal =
        let generator = fun sr n -> transform sr <| sample sr n signal
        { Generator = generator; Scalar = 1.; }

    let lowPass cutoff = 
        let createFilter sr = OnlineFilter.CreateLowpass(ImpulseResponse.Finite, sr, cutoff)
        arbitraryFilter <| fun sr sample -> (createFilter sr).ProcessSamples(sample)

    let highPass cutoff =
        let createFilter sr = OnlineFilter.CreateHighpass(ImpulseResponse.Finite, sr, cutoff)
        arbitraryFilter <| fun sr sample -> (createFilter sr).ProcessSamples(sample)
    
    let bandPass cutoffLow cutoffHigh =
        let createFilter sr = OnlineFilter.CreateBandpass(ImpulseResponse.Finite, sr, cutoffLow, cutoffHigh)
        arbitraryFilter <| fun sr sample -> (createFilter sr).ProcessSamples(sample)

    let bandStop cutoffLow cutoffHigh =
        let createFilter sr = OnlineFilter.CreateBandstop(ImpulseResponse.Finite, sr, cutoffLow, cutoffHigh)
        arbitraryFilter <| fun sr sample -> (createFilter sr).ProcessSamples(sample)

    let denoise =
        let filter = OnlineFilter.CreateDenoise()
        arbitraryFilter <| fun sr sample -> filter.ProcessSamples(sample)

// calculate points of sampling time
let samplePoints sr n =
    Array.init n (fun i -> (float i) / sr)

// calculate points of fft frequency
let fftPoints sr n =
    Fourier.FrequencyScale(n, sr)

let fft x =
   let x' = x |> Array.map (fun r -> complex r 0.)
   Fourier.Forward(x')

   x' |> Array.map Complex.magnitude
