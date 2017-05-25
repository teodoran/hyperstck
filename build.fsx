// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake

// Directories
let buildDir  = "./build/"
let testDir  = "./test/"
let deployDir = "./deploy/"


// Filesets
let references  =
    !! "/**/*.fsproj" -- "/**/*.Tests.fsproj"

let testReferences  =
    !! "/**/*.Tests.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" references
    |> Log "AppBuild-Output: "
)

Target "BuildTests" (fun _ ->
    MSBuildDebug testDir "Build" testReferences
    |> Log "TestBuild-Output: "
)


Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "Hyperstck." + version + ".zip")
)

// Build order
"Clean"
  ==> "Build"
  ==> "BuildTests"
  ==> "Deploy"

// start build
RunTargetOrDefault "BuildTests"