# hyperstck

A hypermedia-driven evaluator for the [Stck](https://github.com/teodoran/stck) programming language.

## Getting started

You'll need F#. [This guide](http://fsharp.org/use/linux/) is usefull if you're using Linux, and you'll need mono as well. When F# is installed run:

    > ./build.sh

This shoud get [paket](https://fsprojects.github.io/Paket/), install dependencies and build everything to a nice executable. Then you just have to run:

    > ./build/Hyperstck.exe

And navigate to [localhost:8282](http://localhost:8282/).

**Troubleshooting**

_You're on Linux and ./build.sh failed:_

This might be caused by ToolsVersion and TargetFrameworkVersion not beeing supported by your mono installation (you should get a warning telling you what version is suported). Try to downgrade ToolsVersion and TargetFrameworkVersion in `/Hyperstck/Hyperstck.fsproj` and `/Hyperstck.Tests/Hyperstck.Tests.fsproj`.

_Permission denied when trying to run Hyperstck.exe on Linux:_

Somtimes the compiled executable won't have sufficient permissions to be executed. This can be fixed by using chmod as shown under.

    > ./build/Hyperstck.exe
    bash: ./build/Hyperstck.exe: Permission denied
    > chmod +x ./build/Hyperstck.exe