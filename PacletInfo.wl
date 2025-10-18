(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "JerryI/LPM",
    "Description" -> "Local Package Manager",
    "Creator" -> "Kirill Vasin",
    "License" -> "MIT",
    "PublisherID" -> "JerryI",
    "Version" -> "0.1.8",
    "WolframVersion" -> "13+",
    "PrimaryContext" -> "JerryI`LPM`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {{"JerryI`LPM`", "LPM.wl"}}
      },
 
      {
        "Asset",
        "Assets" -> {
          {"ExamplesFolder", "./Test"}
        }
      }
    }
  |>
]
