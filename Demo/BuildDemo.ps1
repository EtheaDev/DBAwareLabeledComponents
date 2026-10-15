<#
.SYNOPSIS
    Builds the DBAwareLabeledComponents demo with the most recent Delphi
    installed on this machine.

.DESCRIPTION
    Looks in the registry (HKLM, then HKCU) for the RAD Studio installations,
    starting from Delphi 13 and going backwards (Delphi 12, 11, 10.4 ...) until
    a version with a valid RootDir and bin\rsvars.bat is found. Then configures
    the environment via rsvars.bat and invokes MSBuild on
    Demo\DBAwareLabeledComponentsDemo.dproj.
    The executable is written in Demo\Bin32 (Win32) or Demo\Bin64 (Win64),
    the DCUs in Dcu\<Platform>\<Config>.

.PARAMETER DelphiVersion
    Optional: forces a specific Delphi version (DXE3 ... D13) instead of
    searching for the most recent one.

.PARAMETER Platform
    Target platform: Win32 or Win64 (default: Win64).

.PARAMETER Config
    Build configuration: Debug or Release (default: Release).

.PARAMETER Target
    MSBuild target: Build, Clean or Rebuild (default: Build).

.EXAMPLE
    .\BuildDemo.ps1
    # Builds the demo with the most recent installed Delphi, Win64, Release

.EXAMPLE
    .\BuildDemo.ps1 -DelphiVersion D12 -Platform Win32 -Config Debug
    # Builds the demo with Delphi 12, Win32, Debug
#>

param(
    [ValidateSet("", "DXE3", "DXE4", "DXE5", "DXE6", "DXE7", "DXE8", "D10", "D10.1", "D10.2", "D10.3", "D10.4", "D11", "D12", "D13")]
    [string]$DelphiVersion = "",

    [ValidateSet("Win32", "Win64")]
    [string]$Platform = "Win64",

    [ValidateSet("Debug", "Release")]
    [string]$Config = "Release",

    [ValidateSet("Build", "Clean", "Rebuild")]
    [string]$Target = "Build"
)

$ErrorActionPreference = "Stop"

# Delphi versions, from the most recent to the oldest, with their BDS version
$delphiVersions = [ordered]@{
    "D13"   = "37.0"
    "D12"   = "23.0"
    "D11"   = "22.0"
    "D10.4" = "21.0"
    "D10.3" = "20.0"
    "D10.2" = "19.0"
    "D10.1" = "18.0"
    "D10"   = "17.0"
    "DXE8"  = "16.0"
    "DXE7"  = "15.0"
    "DXE6"  = "14.0"
    "DXE5"  = "12.0"
    "DXE4"  = "11.0"
    "DXE3"  = "10.0"
}

# Registry keys where RAD Studio registers its installations
$registryRoots = @(
    "HKLM:\SOFTWARE\WOW6432Node\Embarcadero\BDS",
    "HKLM:\SOFTWARE\Embarcadero\BDS",
    "HKCU:\Software\Embarcadero\BDS"
)

# Returns the RootDir of a BDS version, or $null if not installed
function Get-BDSRootDir([string]$BdsVersion) {
    foreach ($root in $registryRoots) {
        $key = Join-Path $root $BdsVersion
        if (Test-Path $key) {
            $rootDir = (Get-ItemProperty -Path $key -Name "RootDir" -ErrorAction SilentlyContinue).RootDir
            if ($rootDir -and (Test-Path (Join-Path $rootDir "bin\rsvars.bat"))) {
                return $rootDir
            }
        }
    }
    return $null
}

if ($DelphiVersion) {
    $candidates = @($DelphiVersion)
}
else {
    $candidates = @($delphiVersions.Keys)
}

$foundVersion = $null
$bdsRootDir = $null
foreach ($ver in $candidates) {
    $bdsRootDir = Get-BDSRootDir $delphiVersions[$ver]
    if ($bdsRootDir) {
        $foundVersion = $ver
        break
    }
}

if (-not $foundVersion) {
    if ($DelphiVersion) {
        Write-Error "Delphi $DelphiVersion (BDS $($delphiVersions[$DelphiVersion])) not found in the registry."
    }
    else {
        Write-Error "No Delphi installation found in the registry."
    }
    exit 1
}

$bdsVersion = $delphiVersions[$foundVersion]
$rsvars = Join-Path $bdsRootDir "bin\rsvars.bat"

$Project = "DBAwareLabeledComponentsDemo.dproj"
$projectPath = Join-Path $PSScriptRoot $Project
if (-not (Test-Path $projectPath)) {
    Write-Error "Project not found: $projectPath"
    exit 1
}

Write-Host "============================================" -ForegroundColor Cyan
Write-Host " DBAwareLabeledComponents Demo Build" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host " Delphi:   $foundVersion (BDS $bdsVersion)" -ForegroundColor White
Write-Host " RootDir:  $bdsRootDir" -ForegroundColor White
Write-Host " Project:  $projectPath" -ForegroundColor White
Write-Host " Platform: $Platform" -ForegroundColor White
Write-Host " Config:   $Config" -ForegroundColor White
Write-Host " Target:   $Target" -ForegroundColor White
Write-Host "============================================" -ForegroundColor Cyan

$msbuildArgs = "`"$projectPath`" /t:$Target /p:Config=$Config /p:Platform=$Platform /v:minimal /nologo"
$cmd = "call `"$rsvars`" && msbuild $msbuildArgs"

Write-Host ""
Write-Host "Executing: msbuild $Project /t:$Target /p:Config=$Config /p:Platform=$Platform" -ForegroundColor Yellow
Write-Host ""

# Run via cmd /c so rsvars.bat environment is inherited by msbuild
cmd /c $cmd

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "BUILD FAILED (exit code $LASTEXITCODE)" -ForegroundColor Red
    exit $LASTEXITCODE
}
else {
    Write-Host ""
    Write-Host "BUILD SUCCEEDED" -ForegroundColor Green
    exit 0
}
