{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_Bindings;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_ProcessorInfo;

type
  TProcessorFeature = (pfMMX, pfEMMX, pf3DNow, pf3DNowExt, pfCMOV, pfSSE,
    pfSSE2, pfSSE3, pfSSE3x, pfSSE4A, pfSSE4B, pfSSE5, pfAVX);
  TProcessorFeatures = set of TProcessorFeature;

  PFunctionInfo = ^TFunctionInfo;

  TFunctionInfo = record
    Proc: Pointer;
    CPUFeatures: TProcessorFeatures;
    MemoryAlignment: Byte;
    // OpcodeCount : Integer;
  end;

  TFunctionBinding = class(TPersistent)
  private
    FFunctions: TList;
    FDefaultProc: Pointer;
    FPrototype: Pointer;
    function GetFunctions(Index: Integer): PFunctionInfo;
    procedure SetFunctions(Index: Integer; const Value: PFunctionInfo);
    function GetFunctionCount: Integer;
    function GetPrototype: Pointer;
  public
    constructor Create(Prototype: Pointer; DefaultProc: Pointer = nil); virtual;
    destructor Destroy; override;
    procedure Clear;

    procedure Add(Proc: Pointer; RequiredFeatures: TProcessorFeatures = [];
      MemoryAlignment: Byte = 1);
    procedure Rebind(AvailableFeatures: TProcessorFeatures = []);
    procedure RebindProcessorSpecific;

    // function FindFunction(FunctionID: Integer; PriorityCallback: TFunctionPriority = nil): Pointer;
    property Functions[Index: Integer]: PFunctionInfo read GetFunctions
      write SetFunctions;
    property FunctionCount: Integer read GetFunctionCount;
    property Prototype: Pointer read GetPrototype;
  end;

  TFunctionBindingList = class(TObject)
  private
    FList: TList;
    function GetBindingCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddBinding(Binding: TFunctionBinding);
    function HasBinding(Binding: TFunctionBinding): Boolean;
    procedure RemoveBinding(Binding: TFunctionBinding);
    function GetBindingByPointer(Binding: Pointer): TFunctionBinding;
    procedure Rebind(AvailableFeatures: TProcessorFeatures = []);
    procedure RebindProcessorSpecific;

    property BindingCount: Integer read GetBindingCount;
  end;

function GetBinding(Binding: Pointer): TFunctionBinding;

implementation

uses
  SysUtils;

var
  GBindings: TList;
  GBindingLists: TList;
  GProcessorFeatures: TProcessorFeatures;

function GetBinding(Binding: Pointer): TFunctionBinding;
var
  Index: Integer;
  FunctionIndex: Integer;
begin
  for Index := 0 to GBindings.Count - 1 do
  begin
    Result := TFunctionBinding(GBindings.Items[Index]);
    with Result do
    begin
      if Prototype = Binding then
        Exit;

      for FunctionIndex := 0 to FunctionCount - 1 do
        if PFunctionInfo(Functions[FunctionIndex]).Proc = Binding then
          Exit;
    end;
  end;
  Result := nil;
end;

{ TFunctionBinding }

constructor TFunctionBinding.Create(Prototype: Pointer;
  DefaultProc: Pointer = nil);
begin
  FPrototype := Prototype;
  FDefaultProc := DefaultProc;
  Pointer(FPrototype^) := DefaultProc;
  FFunctions := TList.Create;
  GBindings.Add(Self);
end;

destructor TFunctionBinding.Destroy;
var
  BindingIndex: Integer;
  BindingListIndex: Integer;
begin
  Clear;
  FreeAndNil(FFunctions);

  // remove current binding
  BindingIndex := GBindings.IndexOf(Self);
  if BindingIndex >= 0 then
    GBindings.Delete(BindingIndex);

  for BindingListIndex := 0 to GBindingLists.Count - 1 do
    with TFunctionBindingList(GBindingLists[BindingListIndex]) do
      RemoveBinding(Self);

  inherited;
end;

procedure TFunctionBinding.Add(Proc: Pointer;
  RequiredFeatures: TProcessorFeatures = []; MemoryAlignment: Byte = 1);
var
  Info: PFunctionInfo;
begin
  New(Info);
  Info.Proc := Proc;
  Info.CPUFeatures := RequiredFeatures;
  Info.MemoryAlignment := MemoryAlignment;

  // add function
  FFunctions.Add(Info);
end;

procedure TFunctionBinding.Clear;
var
  Index: Integer;
begin
  for Index := 0 to FFunctions.Count - 1 do
    Dispose(PFunctionInfo(FFunctions[Index]));

  // clear functions
  FFunctions.Clear;
end;

procedure TFunctionBinding.Rebind(AvailableFeatures: TProcessorFeatures = []);
var
  Index: Integer;
begin
  with FFunctions do
    for Index := 0 to Count - 1 do
      if AvailableFeatures + PFunctionInfo(Items[Index])^.CPUFeatures = AvailableFeatures
      then
        Pointer(FPrototype^) := PFunctionInfo(Items[Index])^.Proc;
end;

procedure TFunctionBinding.RebindProcessorSpecific;
begin
  Rebind(GProcessorFeatures);
end;

function TFunctionBinding.GetFunctionCount: Integer;
begin
  Result := FFunctions.Count;
end;

function TFunctionBinding.GetFunctions(Index: Integer): PFunctionInfo;
begin
  Result := FFunctions[Index];
end;

function TFunctionBinding.GetPrototype: Pointer;
begin
  Result := FPrototype;
end;

procedure TFunctionBinding.SetFunctions(Index: Integer;
  const Value: PFunctionInfo);
begin
  FFunctions[Index] := Value;
end;

{ TFunctionBindingList }

constructor TFunctionBindingList.Create;
begin
  inherited;
  FList := TList.Create;
  GBindingLists.Add(Self);
end;

destructor TFunctionBindingList.Destroy;
var
  BindingListIndex: Integer;
begin
  // remove current binding list
  BindingListIndex := GBindingLists.IndexOf(Self);
  if BindingListIndex >= 0 then
    GBindingLists.Delete(BindingListIndex);

  // free binding list
  FreeAndNil(FList);

  inherited;
end;

procedure TFunctionBindingList.AddBinding(Binding: TFunctionBinding);
begin
  FList.Add(Binding);
end;

function TFunctionBindingList.GetBindingByPointer(Binding: Pointer)
  : TFunctionBinding;
var
  Index: Integer;
  FunctionIndex: Integer;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    Result := TFunctionBinding(FList.Items[Index]);
    with Result do
    begin
      if Prototype = Binding then
        Exit;

      for FunctionIndex := 0 to FunctionCount - 1 do
        if PFunctionInfo(Functions[FunctionIndex]).Proc = Binding then
          Exit;
    end;
  end;
  Result := nil;
end;

function TFunctionBindingList.GetBindingCount: Integer;
begin
  Result := FList.Count;
end;

function TFunctionBindingList.HasBinding(Binding: TFunctionBinding): Boolean;
var
  Index: Integer;
begin
  Result := False;

  for Index := 0 to FList.Count - 1 do
    if TFunctionBinding(FList[Index]) = Binding then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TFunctionBindingList.Rebind(AvailableFeatures
  : TProcessorFeatures = []);
var
  Index: Integer;
begin
  for Index := 0 to FList.Count - 1 do
    TFunctionBinding(FList[Index]).Rebind(AvailableFeatures);
end;

procedure TFunctionBindingList.RebindProcessorSpecific;
begin
  Rebind(GProcessorFeatures);
end;

procedure TFunctionBindingList.RemoveBinding(Binding: TFunctionBinding);
var
  Index: Integer;
begin
  Index := 0;
  while Index < FList.Count do
    if Binding = TFunctionBinding(FList[Index]) then
      FList.Delete(Index)
    else
      Inc(Index);
end;

procedure SetProcessorFeatures;
begin
  GProcessorFeatures := [];
  if not Assigned(ProcessorInfo) then
    Exit;

  if ProcessorInfo.HasMMX then
    GProcessorFeatures := [pfMMX];

  if ProcessorInfo.HasExMMX then
    GProcessorFeatures := GProcessorFeatures + [pfEMMX];

  if ProcessorInfo.Has3DNow then
    GProcessorFeatures := GProcessorFeatures + [pf3DNow];

  if ProcessorInfo.HasEx3DNow then
    GProcessorFeatures := GProcessorFeatures + [pf3DNowExt];

  if ProcessorInfo.HasConditionalMove then
    GProcessorFeatures := GProcessorFeatures + [pfCMOV];

  if (ssSSE in ProcessorInfo.SupportsSSE) then
    GProcessorFeatures := GProcessorFeatures + [pfSSE];

  if (ssSSE2 in ProcessorInfo.SupportsSSE) then
    GProcessorFeatures := GProcessorFeatures + [pfSSE2];

  if (ssSSE3 in ProcessorInfo.SupportsSSE) then
    GProcessorFeatures := GProcessorFeatures + [pfSSE3];

  if (ssSSE3x in ProcessorInfo.SupportsSSE) then
    GProcessorFeatures := GProcessorFeatures + [pfSSE3x];

  if (ssSSE4A in ProcessorInfo.SupportsSSE) then
    GProcessorFeatures := GProcessorFeatures + [pfSSE4A];

  if (ssSSE4B in ProcessorInfo.SupportsSSE) then
    GProcessorFeatures := GProcessorFeatures + [pfSSE4B];

  if (ssSSE5 in ProcessorInfo.SupportsSSE) then
    GProcessorFeatures := GProcessorFeatures + [pfSSE5];

  if (ssAVX in ProcessorInfo.SupportsSSE) then
    GProcessorFeatures := GProcessorFeatures + [pfAVX];
end;

procedure CreateGlobalBindings;
begin
  GBindings := TList.Create;
  GBindingLists := TList.Create;
end;

procedure FreeGlobalBindings;
begin
  while GBindings.Count > 0 do
    TFunctionBinding(GBindings.Items[0]).Free;

  FreeAndNil(GBindingLists);
  FreeAndNil(GBindings);
end;

initialization

SetProcessorFeatures;
CreateGlobalBindings;

finalization

FreeGlobalBindings;

end.
