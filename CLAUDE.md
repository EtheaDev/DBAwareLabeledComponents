# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Delphi **VCL component library** (Apache 2.0, by Ethea S.r.l.). It provides an
advanced `TLabeledDbGrid` plus a full set of standard and DB-aware VCL editors
that each carry an attached "boundary label" (like the stock `TLabeledEdit`,
but for ~30 control types and with 12 label positions). Current version: 1.4.1.

There is **no application to run and no automated test suite** — the deliverable
is the set of design-time/run-time packages that get installed into the RAD
Studio IDE. The `Demo\` folder holds a manual showcase app.

## Building

Packages are plain Delphi/MSBuild projects; there is no custom build script for
the library itself. Per Delphi version there is one folder under `Packages\`
(`DXE3`, `DXE6`, `DXE8`, `D10`, `D10_1`, `D10_2`, `D10_3`, `D10_4`, `D11`,
`D12`, `D13`). Each folder contains a `PackageGroup.groupproj` with two packages:

- `DBAwareLabeledComponents.dpk` — runtime package (`{$RUNONLY}`), contains all
  the component source units.
- `dclDBAwareLabeledComponents.dpk` — design-time package (`{$DESIGNONLY}`),
  requires `DesignIDE` + the runtime package, contains only the registry unit.

Build from the IDE (open the `.groupproj`) or with MSBuild, e.g. for Delphi 12:

```powershell
# Requires the rsvars.bat of the target RAD Studio version to be sourced first
msbuild "Packages\D12\PackageGroup.groupproj" /t:Build /p:Config=Release /p:Platform=Win32
msbuild "Packages\D12\PackageGroup.groupproj" /t:Build /p:Config=Release /p:Platform=Win64
```

DCUs are emitted under `Lib\<version>` (e.g. `Lib\Delphi12`); `Dcu\` is a build
output cache. The design package must be **installed** into the IDE to make the
components appear on the *ETHEA LabeledControls* palette (and `DbControls` for
`TDBNumberBox`).

### Installer

End users normally install via an Inno Setup installer that auto-detects
installed Delphi versions, copies sources, builds and registers the packages,
and adds the source path. Build it with `Setup\BuildSetup.bat` (calls
`iscc.exe` on `Setup\Setup.iss`, then code-signs). The `.iss` is heavily
parameterized — see its header comment for `/RADStudioVersions=` and silent /
GetIt install command lines.

## Version support via the shared include

Every source unit starts with `{$I 'DBAwareLabeledComponents.inc'}`. That single
include (`Source\DBAwareLabeledComponents.inc`) maps each compiler's `VER###`
into cumulative feature defines: `DXE3+`, `DXE6+`, … `D10_1+`, `D10_4+`, `D11+`,
`D12+`, `D13+`, plus derived flags like `GDI+`, `HiDPISupport`,
`HasStoreBitmapProperty`. **When adding support for a new Delphi release, this is
the file to extend**, and feature-gate new code with the appropriate `+` define.
Notable gates already in use:

- `D10_1+` → `TLabeledButtonEdit` / `TLabeledDBButtonEdit`
- `D10_4+` → `TDBNumberBox`, `TLabeledNumberBox`, `TLabeledDBNumberBox`

## Architecture: the BoundLabel pattern

The whole library is built around one idea, implemented in
`Source\Vcl.BoundLabel.pas`:

- `TControlBoundLabel` is a `TCustomLabel` subcomponent (`SetSubComponent(True)`,
  empty `Name`) owned by the editor it annotates.
- The positioning/painting logic lives in **free functions** in that unit —
  `RecalculateBounds`, `DrawBoundLabel`, `ChangeBoundCaption`,
  `SetParentOfLabel`, `EnterEditCtrl`, `ExitEditCtrl` — not in a common base
  class (the components must descend from many different stock VCL controls, so
  shared behavior is factored into functions instead of inheritance).

Each labeled component therefore follows an identical wiring recipe (see
`Vcl.LabeledExtCtrls.pas` for the clearest example):

1. Descend from the stock VCL control (e.g. `TLabeledRadioGroup = class(TRadioGroup)`).
2. Hold `FBoundLabel: TControlBoundLabel` + `FBoundCaption: TCaption`; publish
   `BoundLabel` (read-only) and `BoundCaption`.
3. Create `FBoundLabel` in the constructor.
4. Override and delegate to the free functions:
   - `SetBounds` → `RecalculateBounds(...)` then `inherited` then `DrawBoundLabel`
   - `SetParent` → `SetParentOfLabel`
   - `DoEnter`/`DoExit` → `EnterEditCtrl`/`ExitEditCtrl`
   - `SetBoundCaption` → `ChangeBoundCaption`
   - `Notification` → null out `FBoundLabel` on `opRemove`
   - `VisibleChanging` → mirror visibility to the label

`BoundLabel.LabelPosition` (`TControlLabelPosition`, 12 values: `lpTopLeft` …
`lpRightBottom`) and `LabelSpacing` drive placement; when the owner is
`alClient`, `RecalculateBounds` reserves client space for the label.

Two big dispatcher functions in `Vcl.BoundLabel.pas` — `GetEditControlFont` and
`SetEditControlLabelPosition` — are giant `if AEditControl is T... then` chains
over every component type. **Any new labeled component must be added to both
chains** (and font defaults flow from `GetEditControlFont`).

## Source layout (`Source\`)

Units are grouped by the VCL family they extend; all are namespaced `Vcl.*`:

- `Vcl.BoundLabel.pas` — core label class + shared positioning functions (start here).
- `Vcl.LabeledCtrls.pas` — `TLabeledEditEx`, basic edits.
- `Vcl.LabeledExtCtrls.pas` — `TLabeledRadioGroup`, `TLabeledImage`, `TLabeledColorBox`.
- `Vcl.LabeledComCtrls.pas`, `Vcl.LabeledCheckLst.pas`, `Vcl.LabeledColorGrd.pas`,
  `Vcl.LabeledMask.pas` — more standard controls.
- `Vcl.LabeledDBCtrls.pas` — the largest unit: all DB-aware editors **and**
  `TLabeledDbGrid` (the advanced grid: sort indicators, inplace checkbox,
  incremental search, alternate/custom row colors, `LinesPerRow`, `RowMargin`).
- `Vcl.LabeledDbImage.pas`, `Vcl.LabeledDBListView.pas` — DB image/list view.
- `Vcl.LabeledCurrencyEdit.pas` — `TLabeled[DB]CurrencyEdit`.
- `Vcl.LabeledButtonEdit.pas` — `TLabeled[DB]ButtonEdit` (D10.1+; date button opens `Vcl.Form.CalendarView`).
- `Vcl.DBNumberBox.pas` / `Vcl.LabeledNumberBox.pas` — NumberBox family (D10.4.2+).
- `Vcl.DbAwareLabeledUtils.pas`, `Vcl.LabeledGraphicUtils.pas`,
  `Vcl.LabeledShellUtils.pas`, `Vcl.DbAwareLabeledConsts.pas` — helpers/constants.
- `Vcl.DBAwareLabeledComponentsRegistry.pas` — `Register` (palette registration),
  IDE splash screen + About box. **Lives only in the design-time package.**

## Registering a new component

1. Implement the component in the appropriate `Vcl.Labeled*.pas` unit, following
   the BoundLabel wiring recipe above.
2. Add it to both dispatcher chains in `Vcl.BoundLabel.pas`.
3. Add a `RegisterComponents('ETHEA LabeledControls', [...])` line in
   `Vcl.DBAwareLabeledComponentsRegistry.pas` (feature-gate with the right
   `D10_x+` define if needed).
4. If the unit is new, add it to the `contains` clause of every
   `Packages\<ver>\DBAwareLabeledComponents.dpk`.
5. Bump `ComponentsVersion` in the registry unit and `LibraryVersion` in
   `Setup\Setup.iss`, and add a RELEASE NOTES entry in `README.md`.
