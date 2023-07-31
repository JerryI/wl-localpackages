# Wolfram Language Local Packaging Manager

![logo](images-2.jpeg)

Got fed by by using `PacletInstall` that stores your packages in a middle of nowhere? Not receiving updates?
Has to publish update on github and then on Wolfram Repository everytime?
That sucks

## Solution
Keep your modules up-to date with just github and locally to your projects just like npm

Just add a few lines to your project
```mathematica
PacletRepositories[{
    Github -> "https://github.com/KirillBelovTest/HTTPHandler"
}]

<<KirillBelov`HTTPHandler`
```

It overrides all installed paclets and let Mathematica find them in `wl_packages` folder in your project.
Everything is done automatically and does not mess with `Needs` you have.

## Install once and be happy
It ships via standart wolfram paclet system. Install it once and forget about `PacletInstall` command forever
```mathematica
PacletInstall["JerryI/LPM"]
<< JerryI`LPM`
```

or directly
```mathematica
Get["https://raw.githubusercontent.com/JerryI/wl-localpackages/main/Kernel/LPM.wl"]
```

or paste this single file to your project

or from Github releases.

No updates are expected