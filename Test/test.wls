#!/usr/bin/env wolframscript

SetDirectory[DirectoryName[$InputFileName]]

Get[FileNameJoin[{ParentDirectory[Directory[]], "Kernel", "LPM.wl"}]]

PacletRepositories[{
    Github -> "https://github.com/JerryI/Internal" -> "master"
}]

<<KirillBelov`Internal`

