unit DAV_DspRelay;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 OR LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express OR implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2011-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes;

type
  TCustomDspRelay = class(TDspPersistent)
  private
    FState : Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure LowerChanged; virtual; abstract;
    procedure UpperChanged; virtual; abstract;
  public
    constructor Create; virtual;
  published
    property State: Boolean read FState;
  end;
  TCustomDspRelayClass = class of TCustomDspRelay;

  TCustomDspFloatingPointRelay = class(TCustomDspRelay)
  private
    FLower : Double;
    FUpper : Double;
    procedure SetLower(const Value: Double);
    procedure SetUpper(const Value: Double);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure LowerChanged; override;
    procedure UpperChanged; override;
  public
    constructor Create; override;
  published
    property Upper: Double read FUpper write SetUpper;
    property Lower: Double read FLower write SetLower;
  end;
  TCustomDspFloatingPointRelayClass = class of TCustomDspFloatingPointRelay;

  TDspIntegerRelay = class(TCustomDspRelay)
  private
    FLower : Integer;
    FUpper : Integer;
    procedure SetLower(const Value: Integer);
    procedure SetUpper(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure LowerChanged; override;
    procedure UpperChanged; override;
  public
    constructor Create; override;
    procedure ProcessBlock(const Data: PIntegerArray; SampleCount: Integer);
    function ProcessSample(Input: Integer): Integer;
  published
    property Upper: Integer read FUpper write SetUpper;
    property Lower: Integer read FLower write SetLower;
  end;

  TDspRelay32 = class(TCustomDspFloatingPointRelay, IDspProcessor32)
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
  published
    property Lower;
    property Upper;
  end;

  TDspRelay64 = class(TCustomDspFloatingPointRelay, IDspProcessor64)
  public
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;
  published
    property Lower;
    property Upper;
  end;

implementation

uses
  SysUtils, Math;

{ TCustomDspRelay }

constructor TCustomDspRelay.Create;
begin
 inherited;

 FState := True;
end;

procedure TCustomDspRelay.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspRelay then
  with TCustomDspRelay(Dest) do
   begin
    inherited;
    FState  := Self.State;
   end
 else inherited;
end;



{ TCustomDspFloatingPointRelay }

constructor TCustomDspFloatingPointRelay.Create;
begin
 FUpper := 1;
 FLower := -1;
end;

procedure TCustomDspFloatingPointRelay.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspFloatingPointRelay then
  with TCustomDspFloatingPointRelay(Dest) do
   begin
    inherited;
    Upper  := Self.Upper;
    Lower  := Self.Lower;
   end
 else inherited;
end;

procedure TCustomDspFloatingPointRelay.LowerChanged;
begin
 if FUpper < FLower
  then FUpper := FLower;

 Changed;
end;

procedure TCustomDspFloatingPointRelay.UpperChanged;
begin
 if FLower > FUpper
  then FLower := FUpper;

 Changed;
end;

procedure TCustomDspFloatingPointRelay.SetLower(const Value: Double);
begin
 if FLower <> Value then
  begin
   FLower := Value;
   LowerChanged;
  end;
end;

procedure TCustomDspFloatingPointRelay.SetUpper(const Value: Double);
begin
 if FUpper <> Value then
  begin
   FUpper := Value;
   UpperChanged;
  end;
end;


{ TDspIntegerRelay }

constructor TDspIntegerRelay.Create;
begin
 FUpper := 1;
 FLower := -1;
end;

procedure TDspIntegerRelay.AssignTo(Dest: TPersistent);
begin
 if Dest is TDspIntegerRelay then
  with TDspIntegerRelay(Dest) do
   begin
    inherited;
    Upper  := Self.Upper;
    Lower  := Self.Lower;
   end
 else inherited;
end;

procedure TDspIntegerRelay.LowerChanged;
begin
 if FUpper < FLower
  then FUpper := FLower;

 Changed;
end;

procedure TDspIntegerRelay.UpperChanged;
begin
 if FLower > FUpper
  then FLower := FUpper;

 Changed;
end;

procedure TDspIntegerRelay.ProcessBlock(const Data: PIntegerArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample(Data[Sample]);
end;

function TDspIntegerRelay.ProcessSample(Input: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
 inherited;

 if Input > Upper then FState := True;
 if Input < Lower then FState := False;

 if FState
  then Result := 1
  else Result := -1;
{$ELSE}
asm
    CMP     EDX, [Self + FUpper]
    JLE     @NextComparison
    MOV     Byte Ptr [Self + FState],$01
    JMP     @OutputDecision

@NextComparison:
    CMP     EDX, [Self + FLower]
    JNL     @OutputDecision
    MOV     Byte Ptr [Self + FState], $00

@OutputDecision:
    CMP     Byte Ptr [Self + FState], $00
    JZ      @Negative

    MOV     EAX, $1
    RET

@Negative:
    OR      EAX, -$1
{$ENDIF}
end;

procedure TDspIntegerRelay.SetLower(const Value: Integer);
begin
 if FLower <> Value then
  begin
   FLower := Value;
   LowerChanged;
  end;
end;

procedure TDspIntegerRelay.SetUpper(const Value: Integer);
begin
 if FUpper <> Value then
  begin
   FUpper := Value;
   UpperChanged;
  end;
end;


{ TDspRelay32 }

procedure TDspRelay32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDspRelay32.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
 inherited;

 if Input > Upper then FState := True;
 if Input < Lower then FState := False;

 if FState
  then Result := 1
  else Result := -1;
{$ELSE}
asm
    MOV     EDX, Self
    FLD     Input.Single
    FCOMP   [EDX + FUpper].Double
    FSTSW   AX
    SAHF
    JBE     @NextComparison
    MOV     Byte Ptr [EDX + FState], $01

@NextComparison:
    FLD     Input.Single
    FCOMP   [EDX + FLower].Double
    FSTSW   AX
    SAHF
    JNB     @OutputDecision
    MOV     Byte Ptr [EDX + FState],$00

@OutputDecision:
    FLD1
    CMP     Byte Ptr [EDX + FState], $00
    JZ      @Done

    FCHS

@Done:
{$ENDIF}
end;


{ TDspRelay64 }

procedure TDspRelay64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDspRelay64.ProcessSample64(Input: Double): Double;
begin
 inherited;

 if Input > Upper then FState := True;
 if Input < Lower then FState := False;

 if FState
  then Result := 1
  else Result := -1;
end;


initialization
  RegisterDspProcessors32([TDspRelay32]);
  RegisterDspProcessors64([TDspRelay64]);

end.
