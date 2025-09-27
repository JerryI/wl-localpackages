#!/usr/bin/env wolframscript

SetDirectory[DirectoryName[$InputFileName]]

Get[FileNameJoin[{ParentDirectory[Directory[]], "Kernel", "LPM.wl"}]]

LPMRepositories[{
    "Github" -> "https://github.com/JerryI/Internal" -> "master"
}]

<<KirillBelov`Internal`

