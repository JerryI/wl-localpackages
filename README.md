# Wolfram Language Local Package Manager (LPM)
*A basic, stupid package manager for Wolfram Language to install paclets locally*

![logo](./logo.png)

Tired of using `PacletInstall` and having your packages stored in obscure locations? Not receiving updates? Frustrated with publishing updates on GitHub and then again on the Wolfram Repository every time? 

It sucks

Working with paclets the standard way has many issues:

- [Deployment requires](https://community.wolfram.com/groups/-/m/t/3304323) using a Mathematica notebook.
- Packages are deployed globally vs. locally (where are my packages actually located?!).
- [Version conflicts](https://community.wolfram.com/groups/-/m/t/3305665) are common.
- Complete dependency on Wolfram servers.
- It’s impossible to store dependencies individually for each project.

## Solution

### Using paclet archives
Load with a single line of code

```mathematica
LPMRepositories[{
    "https://urlToYourPaclet/CoolLibrary1.paclet",
    "https://urlToYourPaclet/CoolLibrary2.paclet"
}]

<<CoolLibrary1`
<<CoolLibrary2`
```

It overrides all installed paclets (if found) and let Wolfram find them in `wl_packages` folder in your project. You can change it by providing `"Directory"` option.

### Using Github repositories
Load a folder hosted on Github as a public repository:

```mathematica
LPMRepositories[{
    "Github" -> "https://github.com/userName/repo1" -> "master",
    "Github" -> "https://github.com/userName/repo2" -> "dev"
}]

<<CoolLibrary1`
<<CoolLibrary2`
```

✨ This approach gives you automatic updates out of the box

At any change in the provided list a paclet can installed/updated/removed.

### Options
#### `"Directory"`
By the default is `NotebookDirectory[]` or `Directory[]`

#### `"PreseveConfiguration"`
By the default is `False`. Set `True` to avoid automatic removing of packakes, which are no longer in the provided list.

## Load existing packages folder
If you want to skip all updates checks, and just load the existing configuration:

```mathematica
LPMLoad[]
```

### Options
#### `"Directory"`
By the default is `NotebookDirectory[]` or `Directory[]`


## Offline work
In the case of no internet connection possible, it will use stored configuration (aka `LPMLoad`)

## Conflict resolution
There is a known problem of WL's priorities to the package version (see [discussion](https://community.wolfram.com/groups/-/m/t/3305665))
All conflicts are resolved by an option `"ConflictResolutionFunction"`, which by the default just uninstalls a global conflicting paclet.

## Installation
LPM can be installed as a paclet (globally)

```mathematica
PacletInstall["https://raw.githubusercontent.com/JerryI/wl-localpackages/main/Build/JerryI__LPM-0.1.8.paclet"]
```

And

```
Get["JerryI`LPM`"];
```

or get/paste **this single file** to your project:

```
https://raw.githubusercontent.com/JerryI/wl-localpackages/main/Kernel/LPM.wl
```

No updates are expected or breaking changes
