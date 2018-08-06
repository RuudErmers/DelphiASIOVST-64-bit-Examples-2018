unit DAV_DspPreisach;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
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
  Classes, SysUtils, DAV_Types, DAV_Classes, DAV_DspRelay;

type
  TCustomDspPreisach = class(TDspPersistent)
  private
    FHysteronResolution : Integer;
    FHysteronScale      : Single;
    FTotalHysteronCount : Integer;
    FStates             : PByteArray;
    procedure SetHysteronResolution(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure HysteronResolutionChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property HysteronResolution: Integer read FHysteronResolution write SetHysteronResolution;
  end;

  TDspPreisach32 = class(TCustomDspPreisach, IDspProcessor32)
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
  published
    property HysteronResolution;
  end;

  TDspPreisach64 = class(TCustomDspPreisach, IDspProcessor64)
  public
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;
  published
    property HysteronResolution;
  end;

implementation

uses
  DAV_Common, DAV_MemoryUtils;

{ TCustomDspPreisach }

constructor TCustomDspPreisach.Create;
begin
 inherited;
 FStates := nil;
 HysteronResolution := 3;
end;

destructor TCustomDspPreisach.Destroy;
begin
 FreeAlignedMemory(FStates);

 inherited;
end;

procedure TCustomDspPreisach.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspPreisach then
  with TCustomDspPreisach(Dest) do
   begin
    inherited;
    HysteronResolution := Self.HysteronResolution;
   end
 else inherited;
end;

procedure TCustomDspPreisach.HysteronResolutionChanged;
begin
 FTotalHysteronCount := ((FHysteronResolution - 1) * FHysteronResolution) div 2;

 // allocate states
 ReallocateAlignedMemory(FStates, FTotalHysteronCount);

 // reset states (todo!)
 FillChar(FStates[0], FTotalHysteronCount, 1);

 FHysteronScale := 1 / FTotalHysteronCount;
end;

procedure TCustomDspPreisach.SetHysteronResolution(const Value: Integer);
begin
 if Value <= 0
  then raise Exception.Create('Value must be larger than zero!');
 
 if Value <> FHysteronResolution then
  begin
   FHysteronResolution := Value;
   HysteronResolutionChanged;
  end;
end;


{ TDspPreisach32 }

function CountBytes(Data: PByte; Count: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
 Result := 0;
 for Index := 0 to Count - 1 do
  begin
   Result := Result + Data^;
   Inc(Data);
  end;
{$ELSE}
asm
    MOV     ECX, EDX
    XOR     EDX, EDX
    LEA     EAX, EAX + ECX
    NEG     ECX
    JNL     @Done

    PUSH    EBX
@Start:
    MOVZX   EBX, [EAX + ECX]
    ADD     EDX, EBX
    ADD     ECX, 1
    JS      @Start

    POP     EBX

@Done:
    MOV     Result, EDX
{$ENDIF}
end;

procedure TDspPreisach32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDspPreisach32.ProcessSample32(Input: Single): Single;
var
  IntegerInput  : Integer;
  IntegerResult : Integer;
  HysteronIndex : Integer;
  X, Y, Offset  : Integer;
begin
 IntegerInput := Round(FHysteronResolution * Input);

 X := ((FHysteronResolution + IntegerInput) div 2);
 if X <= 0 then
  begin
   FillChar(FStates^[0], FTotalHysteronCount, 0);
   Result := -1;
   Exit;
  end
 else
  if X < FHysteronResolution - 1 then
   begin
    Offset := FHysteronResolution - 1;
    HysteronIndex := X;
    for y := 0 to FHysteronResolution - X - 2 do
     begin
      FillChar(FStates^[HysteronIndex], FHysteronResolution - 1 - X - Y, 0);
      Inc(HysteronIndex, Offset);
      Dec(Offset);
     end;
   end;

 Y := (FHysteronResolution - IntegerInput) div 2;
 if Y <= 0 then
  begin
   FillChar(FStates^[0], FTotalHysteronCount, 1);
   Result := 1;
   Exit;
  end
 else
  begin
   if Y < FHysteronResolution - 1 then
    begin
     Offset := Y * FHysteronResolution - (Y * (Y + 1)) div 2;
     FillChar(FStates^[Offset], FTotalHysteronCount - Offset, 1);
    end
   else Offset := FTotalHysteronCount;

   IntegerResult := CountBytes(@FStates[0], Offset);
   Inc(IntegerResult, FTotalHysteronCount - Offset);
  end;
 Result := 2 * FHysteronScale * IntegerResult - 1;
end;


{ TDspPreisach64 }

procedure TDspPreisach64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDspPreisach64.ProcessSample64(Input: Double): Double;
var
  IntegerInput  : Integer;
  IntegerResult : Integer;
  HysteronIndex : Integer;
  X, Y, Offset  : Integer;
begin
 IntegerInput := Round(FHysteronResolution * Input);

 X := ((FHysteronResolution + IntegerInput) div 2);
 if X <= 0 then
  begin
   FillChar(FStates^[0], FTotalHysteronCount, 0);
   Result := -1;
   Exit;
  end
 else
  if X < FHysteronResolution - 1 then
   begin
    Offset := FHysteronResolution - 1;
    HysteronIndex := X;
    for y := 0 to FHysteronResolution - X - 2 do
     begin
      FillChar(FStates^[HysteronIndex], FHysteronResolution - 1 - X - Y, 0);
      Inc(HysteronIndex, Offset);
      Dec(Offset);
     end;
   end;

 Y := (FHysteronResolution - IntegerInput) div 2;
 if Y <= 0 then
  begin
   FillChar(FStates^[0], FTotalHysteronCount, 1);
   Result := 1;
   Exit;
  end
 else
  begin
   if Y < FHysteronResolution - 1 then
    begin
     Offset := Y * FHysteronResolution - (Y * (Y + 1)) div 2;
     FillChar(FStates^[Offset], FTotalHysteronCount - Offset, 1);
    end
   else Offset := FTotalHysteronCount;

    IntegerResult := 0;
    for HysteronIndex := 0 to Offset - 1
     do Inc(IntegerResult, FStates[HysteronIndex]);
    Inc(IntegerResult, FTotalHysteronCount - Offset);
  end;
 Result := 2 * FHysteronScale * IntegerResult - 1;
end;

end.
