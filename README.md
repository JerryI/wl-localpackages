# Wolfram Language Local Package Manager (LPM)

![logo](images-2.jpeg)


Tired of using `PacletInstall` and having your packages stored in obscure locations? Not receiving updates? Frustrated with publishing updates on GitHub and then again on the Wolfram Repository every time? 

It sucks

Working with paclets the standard way has many issues:

- [Deployment requires](https://community.wolfram.com/groups/-/m/t/3304323) using a Mathematica notebook.
- Packages are deployed globally vs. locally (where are my packages actually located?!).
- [Version conflicts](https://community.wolfram.com/groups/-/m/t/3305665) are common.
- Complete dependency on Wolfram servers.
- It’s impossible to store dependencies individually for each project.

Let me know if you’d like any further adjustments!

## Solution
Keep your modules up-to date with just github and locally to your projects just like `npm`

Just add a few lines to your project
```mathematica
PacletRepositories[{
    Github -> "https://github.com/KirillBelovTest/HTTPHandler"
}]

<<KirillBelov`HTTPHandler`
```

It overrides all installed paclets and let Mathematica find them in `wl_packages` folder in your project.
Everything is done automatically and does not mess with `Needs` you have.

## Releases, branches?
By the default it looks for the latest release and downloads it, otherwise just `master` branch will be downloaded
```mathematica
PacletRepositories[{
    Github -> "https://github.com/KirillBelovTest/HTTPHandler"
}]
```

### Branches
One can specify which branch should be downloaded

```mathematica
PacletRepositories[{
    Github -> "https://github.com/KirillBelovTest/HTTPHandler" -> "master"
}]
```

## Bypass any checks
Using a special option

```mathematica
PacletRepositories[{
    Github -> "https://github.com/KirillBelovTest/HTTPHandler" -> "master"
}, "Passive"->True]
```

will ignore any changes made to repos list and updates and directly load all local packages into WL.

## Offline work
In the case of no internet connection possible, it will use local files


## Conflict resolution
There is a known problem of WL's priorities to the package version (see [discussion](https://community.wolfram.com/groups/-/m/t/3305665))
All conflicts are resolved by an option `"ConflictResolutionFunction"`, which by the default just uninstalls a global conflicting paclet.

## Install once and be happy
Install it once and forget about `PacletInstall` command forever

```mathematica
Get["https://raw.githubusercontent.com/JerryI/wl-localpackages/main/Kernel/LPM.wl"]
```

or paste this single file to your project

No updates are expected
