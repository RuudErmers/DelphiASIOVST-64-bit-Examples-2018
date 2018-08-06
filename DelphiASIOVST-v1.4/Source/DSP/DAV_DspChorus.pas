unit DAV_DspChorus;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspLFO;

type
  TCustomDspChorus = class(TDspSampleRatePersistent)
  private
    FSpeed        : Double;
    FDepth        : Double;
    FMix          : Double;
    FStages       : Byte;
    FStageMix     : Double;
    FRealBufSize  : Integer;
    FBufferSize   : Integer;
    FBufferInPos  : Integer;
    FBufferOutPos : Integer;
    FLFOs         : array of TLFOSine;
    FDrift        : Double;
    procedure SetDepth(const Value: Double);
    procedure SetMix(const Value: Double);
    procedure SetSpeed(const Value: Double);
    procedure SetStages(const Value: Byte);
    procedure SetDrift(const Value: Double);
    function GetLFO(Index: Integer): TLFOSine;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateStageMix; virtual;
    procedure DepthChanged; virtual;
    procedure MixChanged; virtual;
    procedure SampleRateChanged; override;
    procedure SpeedChanged; virtual;
    procedure StagesChanged; virtual;
    procedure UpdateBuffer; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; virtual;
    property LFO[Index: Integer]: TLFOSine read GetLFO; 
  published
    property Speed: Double read FSpeed write SetSpeed;
    property Depth: Double read FDepth write SetDepth;
    property Drift: Double read FDrift write SetDrift;
    property Stages: Byte read FStages write SetStages default 0;
    property Mix: Double read FMix write SetMix;
  end;

  TDspChorus32 = class(TCustomDspChorus, IDspProcessor32)
  private
    FBuffer32 : PDAVSingleFixedArray;
  protected
    procedure UpdateBuffer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;

    procedure Reset; override;
  published
    property Depth;
    property Drift;
    property Mix;
    property SampleRate;
    property Speed;
    property Stages;
  end;

  TDspChorus64 = class(TCustomDspChorus, IDspProcessor64)
  private
    FBuffer64 : PDAVDoubleFixedArray;
  protected
    procedure UpdateBuffer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;

    procedure Reset; override;
  published
    property Depth;
    property Drift;
    property Mix;
    property SampleRate;
    property Speed;
    property Stages;
  end;

  TDspChorus = class(TCustomDspChorus, IDspProcessor32, IDspProcessor64)
  private
    FBuffer          : Pointer;
    FBufferPrecision : TPrecision;
    procedure SetBufferPrecision(const Value: TPrecision);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferPrecisionChanged; virtual;
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
    function ProcessSample64(Input: Double): Double;

    procedure Reset; override;
  published
    property BufferPrecision: TPrecision read FBufferPrecision write SetBufferPrecision;

    property Depth;
    property Drift;
    property Mix;
    property SampleRate;
    property Speed;
    property Stages;
  end;

  TChorus = TDspChorus32;

implementation

uses
  SysUtils, Math, DAV_HalfFloat, DAV_DspInterpolation;

{ TCustomDspChorus }

constructor TCustomDspChorus.Create;
begin
 inherited;
 
 FSpeed        := 2;
 FDepth        := 0.5;
 FMix          := 0.5;
 FDrift        := 0;
 Stages        := 2;
 FBufferInPos  := 0;
 FBufferOutPos := 0;
end;

destructor TCustomDspChorus.Destroy;
var
  i : Integer;
begin
 for i := 0 to Length(FLFOs) - 1
  do FreeAndNil(FLFOs[i]);
 inherited;
end;

function TCustomDspChorus.GetLFO(Index: Integer): TLFOSine;
begin
 if (Index >= 0) and (Index < Length(FLFOs))
  then Result := FLFOs[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TCustomDspChorus.Reset;
var
  i : Integer;
begin
 for i := 0 to FStages - 1
  do FLFOs[i].Reset;
end;

procedure TCustomDspChorus.AssignTo(Dest: TPersistent);
var
  Stage : Integer;
begin
 if Dest is TCustomDspChorus then
  with TCustomDspChorus(Dest) do
   begin
    inherited;
    Speed  := Self.Speed;
    Depth  := Self.Depth;
    Mix    := Self.Mix;
    Stages := Self.Stages;
    Drift  := Self.Drift;
    for Stage := 0 to Stages - 1
     do Self.FLFOs[0].Assign(FLFOs[0]);
   end
 else inherited;
end;

procedure TCustomDspChorus.CalculateStageMix;
begin
 FStageMix := 0.5 * (FMix + Power(FMix, Stages));
end;

procedure TCustomDspChorus.StagesChanged;
var
  i, OldStages : Integer;
begin
 OldStages := Length(FLFOs);
 for i := FStages to OldStages - 1 do FreeAndNil(FLFOs[i]);
 SetLength(FLFOs, FStages);
 for i := OldStages to FStages - 1 do
  begin
   FLFOs[i]            := TLFOSine.Create;
   FLFOs[i].SampleRate := SampleRate;
  end;
 SpeedChanged;
 CalculateStageMix;
 Changed;
end;

procedure TCustomDspChorus.UpdateBuffer;
begin
 // determine buffer size
 FBufferSize  := round(sqr(Depth) * 0.25 * SampleRate); // quarter second
 FRealBufSize := FBufferSize + 8;

 // check and reset buffer position
 if FBufferInPos >= FRealBufSize
  then FBufferInPos := 4;
 FBufferOutPos := FBufferInPos + FBufferSize div 2;
 if FBufferOutPos >= FRealBufSize
  then FBufferOutPos := FBufferOutPos - FBufferSize;
end;

procedure TCustomDspChorus.SpeedChanged;
var
  LFOIndex  : Integer;
  d         : Double;
  BasePhase : Double;
begin
 // check that at least one LFO is available
 if Length(FLFOs) = 0 then Exit;

 // calculate scale factor
 d := 2 * Pi / Length(FLFOs);

 // store current base phase
 BasePhase := FLFOs[0].Phase;
 for LFOIndex := 0 to Length(FLFOs) - 1 do
  begin
   FLFOs[LFOIndex].Frequency := (1 - FDrift * Random) * Speed;
   FLFOs[LFOIndex].Phase := BasePhase + (LFOIndex + FDrift * Random) * d;
  end;
 UpdateBuffer;
 Changed;
end;

procedure TCustomDspChorus.DepthChanged;
begin
 UpdateBuffer;
 Changed;
end;

procedure TCustomDspChorus.MixChanged;
begin
 CalculateStageMix;
 Changed;
end;

procedure TCustomDspChorus.SampleRateChanged;
var
  i : Integer;
begin
 for i := 0 to Length(FLFOs) - 1
  do FLFOs[i].SampleRate := SampleRate;
 UpdateBuffer;
 inherited;
end;

procedure TCustomDspChorus.SetDepth(const Value: Double);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   DepthChanged;
  end;
end;

procedure TCustomDspChorus.SetDrift(const Value: Double);
begin
 if FDrift <> Value then
  begin
   FDrift := Value;
   SpeedChanged;
  end;
end;

procedure TCustomDspChorus.SetMix(const Value: Double);
begin
 if FMix <> Value then
  begin
   FMix := Value;
   MixChanged;
  end;
end;

procedure TCustomDspChorus.SetSpeed(const Value: Double);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   SpeedChanged;
  end;
end;

procedure TCustomDspChorus.SetStages(const Value: Byte);
begin
 if FStages <> Value then
  begin
   FStages := Value;
   StagesChanged;
  end;
end;

{ TDspChorus32 }

constructor TDspChorus32.Create;
begin
 inherited;
 FBuffer32 := nil;
 FRealBufSize := 8;
 UpdateBuffer;
end;

destructor TDspChorus32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TDspChorus32.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TDspChorus32 then
  with TDspChorus32(Dest) do
   begin
    inherited;
    assert(FRealBufSize = Self.FRealBufSize);
    Move(Self.FBuffer32^, FBuffer32^, FRealBufSize * SizeOf(Single));
   end else
 if Dest is TDspChorus64 then
  with TDspChorus64(Dest) do
   begin
    inherited;
    Assert(FRealBufSize = Self.FRealBufSize);
    for Sample := 0 to FRealBufSize - 1
     do Self.FBuffer32^[Sample] := FBuffer64^[Sample];
   end
 else inherited;
end;

procedure TDspChorus32.Reset;
begin
 FillChar(FBuffer32^[0], FRealBufSize * SizeOf(Single), 0);
end;

procedure TDspChorus32.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FRealBufSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer32, FRealBufSize * SizeOf(Single));
 if FRealBufSize > OldBufferSize
  then FillChar(FBuffer32^[OldBufferSize], (FRealBufSize - OldBufferSize) * SizeOf(Single), 0);
end;

procedure TDspChorus32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDspChorus32.ProcessSample32(Input: Single): Single;
var
  i, p : Integer;
  d, m : Double;
begin
 inherited;

 // make sure the buffer has been allocated (e.g. not nil)
 assert(FBuffer32 <> nil);

 // get delayed dry output
 Result := (1 - FMix) * FBuffer32[FBufferOutPos];

 // increase delayed output buffer position
 inc(FBufferOutPos);
 if FBufferOutPos >= FRealBufSize
  then FBufferOutPos := 4;

 m := FStageMix;
 for i := 0 to Stages - 1 do
  begin
   // calculate next LFO position
   FLFOs[i].CalculateNextSample;
   d := 4 + FBufferSize * 0.5 * (1 - FLFOs[i].Sine);

   // calculate absolute sample position
   p := round(d - 0.5);
   d := d - p;
   p := FBufferInPos + p;
   if p >= FRealBufSize
    then p := p - (FRealBufSize - 4) else
   if p < 4 then p := p + (FRealBufSize - 4);
   Result := Result + m * Hermite32_asm(d, @FBuffer32[p - 4]);
  end;

 // store new data
 FBuffer32[FBufferInPos] := Input;
 inc(FBufferInPos);
 if FBufferInPos >= FRealBufSize then
  begin
   Move(FBuffer32[FRealBufSize - 4], FBuffer32[0], 4 * SizeOf(Single));
   FBufferInPos := 4;
  end;
end;

{ TDspChorus64 }

constructor TDspChorus64.Create;
begin
 inherited;
 FBuffer64 := nil;
 FRealBufSize := 8;
 UpdateBuffer;
end;

destructor TDspChorus64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TDspChorus64.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TDspChorus32 then
  with TDspChorus32(Dest) do
   begin
    inherited;
    Assert(FRealBufSize = Self.FRealBufSize);
    for Sample := 0 to FRealBufSize - 1
     do Self.FBuffer64^[Sample] := FBuffer32^[Sample];
   end else
 if Dest is TDspChorus64 then
  with TDspChorus64(Dest) do
   begin
    inherited;
    assert(FRealBufSize = Self.FRealBufSize);
    Move(Self.FBuffer64^, FBuffer64^, FRealBufSize * SizeOf(Double));
   end
 else inherited;
end;

procedure TDspChorus64.Reset;
begin
 FillChar(FBuffer64^[0], FRealBufSize * SizeOf(Double), 0);
end;

procedure TDspChorus64.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FRealBufSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer64, FRealBufSize * SizeOf(Double));
 if FRealBufSize > OldBufferSize
  then FillChar(FBuffer64^[OldBufferSize], (FRealBufSize - OldBufferSize) * SizeOf(Double), 0);
end;

procedure TDspChorus64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDspChorus64.ProcessSample64(Input: Double): Double;
var
  i, p : Integer;
  d, m : Double;
begin
 inherited;

 // make sure the buffer has been allocated (e.g. not nil)
 assert(FBuffer64 <> nil);

 // get delayed dry output
 Result := (1 - FMix) * FBuffer64[FBufferOutPos];

 // increase delayed output buffer position
 inc(FBufferOutPos);
 if FBufferOutPos >= FRealBufSize
  then FBufferOutPos := 4;

 m := FStageMix;
 for i := 0 to Stages - 1 do
  begin
   // calculate next LFO position
   FLFOs[i].CalculateNextSample;
   d := 4 + FBufferSize * 0.5 * (1 - FLFOs[i].Sine);

   // calculate absolute sample position
   p := round(d - 0.5);
   d := d - p;
   p := FBufferInPos + p;
   if p >= FRealBufSize
    then p := p - (FRealBufSize - 4) else
   if p < 4 then p := p + (FRealBufSize - 4);
   Result := Result + m * Hermite64_asm(d, @FBuffer64[p - 4]);
  end;

 // store new data
 FBuffer64[FBufferInPos] := Input;
 inc(FBufferInPos);
 if FBufferInPos >= FRealBufSize then
  begin
   Move(FBuffer64[FRealBufSize - 4], FBuffer64[0], 4 * SizeOf(Double));
   FBufferInPos := 4;
  end;
end;


{ TDspChorus }

constructor TDspChorus.Create;
begin
 inherited;
 FBuffer := nil;
 FRealBufSize := 8;
 UpdateBuffer;
end;

destructor TDspChorus.Destroy;
begin
 FreeMem(FBuffer);
 inherited;
end;

procedure TDspChorus.AssignTo(Dest: TPersistent);
begin
(*
var
  Sample : Integer;

 if Dest is TDspChorus32 then
  with TDspChorus32(Dest) do
   begin
    inherited;
    assert(FRealBufSize = Self.FRealBufSize);
    Move(Self.FBuffer32^, FBuffer32^, FRealBufSize * SizeOf(Single));
   end else
 if Dest is TDspChorus64 then
  with TDspChorus64(Dest) do
   begin
    inherited;
    Assert(FRealBufSize = Self.FRealBufSize);
    for Sample := 0 to FRealBufSize - 1
     do Self.FBuffer32^[Sample] := FBuffer64^[Sample];
   end
 else inherited;
*)
 // yet todo!!!
end;

procedure TDspChorus.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TDspChorus.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDspChorus.ProcessSample32(Input: Single): Single;
var
  i, p : Integer;
  d, m : Double;
begin
 inherited;

 // make sure the buffer has been allocated (e.g. not nil)
 Assert(FBuffer <> nil);

 // get delayed dry output
 case FBufferPrecision of
    pcHalf : Result := (1 - FMix) * PDAVHalfFloatFixedArray(FBuffer)^[FBufferOutPos];
  pcSingle : Result := (1 - FMix) * PDAVSingleFixedArray(FBuffer)^[FBufferOutPos];
  pcDouble : Result := (1 - FMix) * PDAVDoubleFixedArray(FBuffer)^[FBufferOutPos];
  else Result := 0;
 end;

 // increase delayed output buffer position
 inc(FBufferOutPos);
 if FBufferOutPos >= FRealBufSize
  then FBufferOutPos := 4;

 m := FStageMix;
 for i := 0 to Stages - 1 do
  begin
   // calculate next LFO position
   FLFOs[i].CalculateNextSample;
   d := 4 + FBufferSize * 0.5 * (1 - FLFOs[i].Sine);

   // calculate absolute sample position
   p := round(d - 0.5);
   d := d - p;
   p := FBufferInPos + p;
   if p >= FRealBufSize
    then p := p - (FRealBufSize - 4) else
   if p < 4 then p := p + (FRealBufSize - 4);

   case FBufferPrecision of
      pcHalf : raise Exception.Create('yet to implement!');
    pcSingle : Result := Result + m * Hermite32_asm(d, @PDAVSingleFixedArray(FBuffer)^[p - 4]);
    pcDouble : Result := Result + m * Hermite64_asm(d, @PDAVDoubleFixedArray(FBuffer)^[p - 4]);
   end;

  end;

 // store new data
 case FBufferPrecision of
    pcHalf : PDAVHalfFloatFixedArray(FBuffer)^[FBufferInPos] := FastSingleToHalfFloat(Input);
  pcSingle : PDAVSingleFixedArray(FBuffer)^[FBufferInPos] := Input;
  pcDouble : PDAVDoubleFixedArray(FBuffer)^[FBufferInPos] := Input;
 end;

 inc(FBufferInPos);
 if FBufferInPos >= FRealBufSize then
  begin
   case FBufferPrecision of
      pcHalf : Move(PDAVHalfFloatFixedArray(FBuffer)[FRealBufSize - 4], FBuffer^, 4 * SizeOf(THalfFloat));
    pcSingle : Move(PDAVSingleFixedArray(FBuffer)[FRealBufSize - 4], FBuffer^, 4 * SizeOf(Single));
    pcDouble : Move(PDAVDoubleFixedArray(FBuffer)[FRealBufSize - 4], FBuffer^, 4 * SizeOf(Double));
   end;

   FBufferInPos := 4;
  end;
end;

function TDspChorus.ProcessSample64(Input: Double): Double;
var
  i, p : Integer;
  d, m : Double;
begin
 inherited;

 // make sure the buffer has been allocated (e.g. not nil)
 Assert(FBuffer <> nil);

 // get delayed dry output
 case FBufferPrecision of
    pcHalf : Result := (1 - FMix) * PDAVHalfFloatFixedArray(FBuffer)^[FBufferOutPos];
  pcSingle : Result := (1 - FMix) * PDAVSingleFixedArray(FBuffer)^[FBufferOutPos];
  pcDouble : Result := (1 - FMix) * PDAVDoubleFixedArray(FBuffer)^[FBufferOutPos];
  else Result := 0;
 end;

 // increase delayed output buffer position
 inc(FBufferOutPos);
 if FBufferOutPos >= FRealBufSize
  then FBufferOutPos := 4;

 m := FStageMix;
 for i := 0 to Stages - 1 do
  begin
   // calculate next LFO position
   FLFOs[i].CalculateNextSample;
   d := 4 + FBufferSize * 0.5 * (1 - FLFOs[i].Sine);

   // calculate absolute sample position
   p := round(d - 0.5);
   d := d - p;
   p := FBufferInPos + p;
   if p >= FRealBufSize
    then p := p - (FRealBufSize - 4) else
   if p < 4 then p := p + (FRealBufSize - 4);

   case FBufferPrecision of
      pcHalf : raise Exception.Create('yet to implement!');
    pcSingle : Result := Result + m * Hermite32_asm(d, @PDAVSingleFixedArray(FBuffer)^[p - 4]);
    pcDouble : Result := Result + m * Hermite64_asm(d, @PDAVDoubleFixedArray(FBuffer)^[p - 4]);
   end;

  end;

 // store new data
 case FBufferPrecision of
    pcHalf : PDAVHalfFloatFixedArray(FBuffer)^[FBufferInPos] := FastSingleToHalfFloat(Input);
  pcSingle : PDAVSingleFixedArray(FBuffer)^[FBufferInPos] := Input;
  pcDouble : PDAVDoubleFixedArray(FBuffer)^[FBufferInPos] := Input;
 end;

 inc(FBufferInPos);
 if FBufferInPos >= FRealBufSize then
  begin
   case FBufferPrecision of
      pcHalf : Move(PDAVHalfFloatFixedArray(FBuffer)[FRealBufSize - 4], FBuffer^, 4 * SizeOf(THalfFloat));
    pcSingle : Move(PDAVSingleFixedArray(FBuffer)[FRealBufSize - 4], FBuffer^, 4 * SizeOf(Single));
    pcDouble : Move(PDAVDoubleFixedArray(FBuffer)[FRealBufSize - 4], FBuffer^, 4 * SizeOf(Double));
   end;

   FBufferInPos := 4;
  end;
end;

procedure TDspChorus.Reset;
begin
 case FBufferPrecision of
    pcHalf : FillChar(FBuffer^, FRealBufSize * 2, 0);
  pcSingle : FillChar(FBuffer^, FRealBufSize * SizeOf(Single), 0);
  pcDouble : FillChar(FBuffer^, FRealBufSize * SizeOf(Double), 0);
 end;
end;

procedure TDspChorus.SetBufferPrecision(const Value: TPrecision);
begin
 if FBufferPrecision <> Value then
  begin
   FBufferPrecision := Value;
   BufferPrecisionChanged;
  end;
end;

procedure TDspChorus.BufferPrecisionChanged;
begin
 UpdateBuffer;
end;

procedure TDspChorus.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FRealBufSize;
 inherited;

 case FBufferPrecision of
  pcHalf :
    begin
     // allocate memory
     ReallocMem(FBuffer, FRealBufSize * 2);
     if FRealBufSize > OldBufferSize
      then FillChar(PDAVHalfFloatFixedArray(FBuffer)^[OldBufferSize],
        (FRealBufSize - OldBufferSize) * 2, 0);
    end;
  pcSingle :
    begin
     // allocate memory
     ReallocMem(FBuffer, FRealBufSize * SizeOf(Single));
     if FRealBufSize > OldBufferSize
      then FillChar(PDAVSingleFixedArray(FBuffer)^[OldBufferSize],
        (FRealBufSize - OldBufferSize) * SizeOf(Single), 0);
    end;
  pcDouble :
    begin
     // allocate memory
     ReallocMem(FBuffer, FRealBufSize * SizeOf(Double));
     if FRealBufSize > OldBufferSize
      then FillChar(PDAVDoubleFixedArray(FBuffer)^[OldBufferSize],
        (FRealBufSize - OldBufferSize) * SizeOf(Double), 0);
    end;
 end;
end;

initialization
  RegisterDspProcessors32([TDspChorus32, TDspChorus]);
  RegisterDspProcessors64([TDspChorus64, TDspChorus]);

end.
