unit DAV_DspDelayLines;

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
  Classes, DAV_Types, DAV_Classes;

// ToDo: complete assignto

type
  TCustomDelayLine = class(TDspPersistent)
  private
    procedure SetBufferSize(const Value: Integer);
  protected
    FBufferPos  : Integer;
    FBufferSize : Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; virtual; abstract;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  public
    constructor Create(const BufferSize: Integer = 0); virtual;
    procedure Reset; virtual;
    procedure ClearBuffer; virtual; abstract;
  end;

  TCustomDelayLineSamples = class(TCustomDelayLine)
  public
    property BufferSize;
  end;

  TCustomDelayLineSamples32 = class(TCustomDelayLineSamples, IDspProcessor32)
  private
    function GetSample(Index: Integer): Single;
  protected
    FBuffer : PDAVSingleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure ClearBuffer; override;
    procedure Reset; override;
    
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;

    function GetAbsMax: Single;
    function GetMaximum: Single;
    function GetMinimum: Single;

    property Sample[Index: Integer]: Single read GetSample;
  end;

  TCustomDelayLineSamples64 = class(TCustomDelayLineSamples, IDspProcessor64)
  private
    function GetSample(Index: Integer): Double;
  protected
    FBuffer : PDAVDoubleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure ClearBuffer; override;
    procedure Reset; override;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;

    function GetAbsMax: Double;
    function GetMaximum: Double;
    function GetMinimum: Double;

    property Sample[Index: Integer]: Double read GetSample;
  end;

  TBufferFloatType = (ft16, ft32, ft64);

  TDelayLineSamples = class(TCustomDelayLineSamples, IDspProcessor64)
  private
    FBufferFloatType : TBufferFloatType;
    FBytesPerSample  : Integer;
    function GetSample(Index: Integer): Double;
    procedure SetBufferFloatType(const Value: TBufferFloatType);
  protected
    FBuffer : Pointer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
    procedure BufferFloatTypeChanged; virtual;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure ClearBuffer; override;
    procedure Reset; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Double;
    function ProcessSample64(Input: Double): Double;

    function GetAbsMax: Double;
    function GetMaximum: Double;
    function GetMinimum: Double;

    property Sample[Index: Integer]: Double read GetSample;
  published
    property BufferSize;
    property BufferFloatType: TBufferFloatType read FBufferFloatType write SetBufferFloatType;
    property BytesPerSample: Integer read FBytesPerSample;
  end;

  TDelayLineSamples32 = class(TCustomDelayLineSamples32)
  published
    property BufferSize;
  end;

  TDelayLineSamples64 = class(TCustomDelayLineSamples64)
  published
    property BufferSize;
  end;

  TCustomDelayLineFractional = class(TCustomDelayLine)
  private
    procedure SetFractional(const Value: Double);
    function GetFractional: Double;
  protected
    FFractional : Double;
    procedure FractionalChanged; virtual;
  public
    constructor Create(const FractionalBufferSize: Double = 0); reintroduce; virtual;
    property FractionalBuffersize: Double read GetFractional write SetFractional;
  end;

  TDelayLineFractional32 = class(TCustomDelayLineFractional, IDspProcessor32)
  protected
    FBuffer         : PDAVSingleFixedArray;
    FRealBufferSize : Integer;
    FIntBuffer      : TDAV4SingleArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const FractionalBufferSize: Double = 0); override;
    destructor Destroy; override;

    procedure ClearBuffer; override;
    procedure Reset; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;

  published
    property FractionalBufferSize;
  end;

  TDelayLineFractional64 = class(TCustomDelayLineFractional, IDspProcessor64)
  protected
    FBuffer         : PDAVDoubleFixedArray;
    FRealBufferSize : Integer;
    FIntBuffer      : TDAV4DoubleArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const FractionalBufferSize: Double = 0); override;
    destructor Destroy; override;

    procedure ClearBuffer; override;
    procedure Reset; override;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;
  published
    property FractionalBufferSize;
  end;


  TDelayLineTime32 = class(TDspSampleRatePersistent, IDspProcessor32)
  private
    FFractionalDelay : TDelayLineFractional32;
    FTime            : Double;
    procedure SetTime(const Value: Double);
    procedure TimeChanged;
  public
    constructor Create(const BufferSize: Integer = 0); reintroduce; virtual;
    destructor Destroy; override;
    procedure Reset; virtual;
    function ProcessSample32(Input: Single): Single;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
  published
    property Samplerate;
    property Time: Double read FTime write SetTime;
  end;

  TDelayLineTime64 = class(TDspSampleRatePersistent, IDspProcessor64)
  private
    FFractionalDelay : TDelayLineFractional64;
    FTime            : Double;
    procedure SetTime(const Value: Double);
    procedure TimeChanged;
  public
    constructor Create(const BufferSize: Integer = 0); reintroduce; virtual;
    destructor Destroy; override;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;

    procedure Reset; virtual;
  published
    property Samplerate;
    property Time: Double read FTime write SetTime;
  end;


implementation

uses
  SysUtils, Math, DAV_HalfFloat, DAV_DspInterpolation;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrUnknownFloatingPointFormat = 'Floating point format unknown';

{ TCustomDelayLine }

constructor TCustomDelayLine.Create(const BufferSize: Integer = 0);
begin
 inherited Create;
 FBufferSize := BufferSize;
 if BufferSize > 0
  then BufferSizeChanged;
 FBufferPos  := 0;
end;

procedure TCustomDelayLine.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDelayLine then
  with TCustomDelayLine(Dest) do
   begin
    FBufferPos  := Self.FBufferPos;
    BufferSize := Self.BufferSize;
   end
  else inherited;
end;

procedure TCustomDelayLine.Reset;
begin
 FBufferPos := 0;
end;

procedure TCustomDelayLine.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;


{ TCustomDelayLineSamples32 }

constructor TCustomDelayLineSamples32.Create(const BufferSize: Integer = 0);
begin
 FBuffer := nil;
 inherited Create(Buffersize);
 ClearBuffer;
end;

destructor TCustomDelayLineSamples32.Destroy;
begin
 if Assigned(FBuffer)
  then FreeMem(FBuffer);
 FBuffer := nil;
 inherited;
end;

function TCustomDelayLineSamples32.GetAbsMax: Single;
var
  Pos: Integer;
begin
 Assert(FBufferSize > 0);

 Result := Abs(FBuffer^[0]);

 for Pos := 1 to FBufferSize - 1 do
  if Abs(FBuffer^[Pos]) > Result then Result := Abs(FBuffer^[Pos]);
end;

function TCustomDelayLineSamples32.GetMaximum: Single;
var
  Pos: Integer;
begin
 Assert(FBufferSize > 0);

 Result := FBuffer^[0];

 for Pos := 1 to FBufferSize - 1 do
  if FBuffer^[Pos] > Result then Result := FBuffer^[Pos];
end;

function TCustomDelayLineSamples32.GetMinimum: Single;
var
  Pos: Integer;
begin
 Assert(FBufferSize > 0);

 Result := FBuffer^[0];

 for Pos := 1 to FBufferSize - 1 do
  if FBuffer^[Pos] < Result then Result := FBuffer^[Pos];
end;

function TCustomDelayLineSamples32.GetSample(Index: Integer): Single;
var
  Pos: Integer;
begin
 if (Index < 0) or (Index >= FBufferSize)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 Pos := FBufferPos - Index;
 if Pos < 0
  then Inc(Pos, FBufferSize);
 Result := FBuffer^[Pos];
end;

procedure TCustomDelayLineSamples32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   Temp := FBuffer^[FBufferPos];
   FBuffer^[FBufferPos] := Data[Sample];
   Data[Sample] := Temp;
   Inc(FBufferPos);
   if FBufferPos >= FBufferSize
    then FBufferPos := 0;
  end;
end;

function TCustomDelayLineSamples32.ProcessSample32(Input: Single): Single;
begin
 Result := FBuffer^[FBufferPos];
 FBuffer^[FBufferPos] := Input;
 Inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TCustomDelayLineSamples32.AssignTo(Dest: TPersistent);
var
  SampleIndex : Integer;
begin
 if Dest is TCustomDelayLineSamples32 then
  with TCustomDelayLineSamples32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer^, Self.FBuffer^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomDelayLineSamples64 then
  with TCustomDelayLineSamples64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for SampleIndex := 0 to FBufferSize - 1
     do Self.FBuffer^[SampleIndex] := FBuffer^[SampleIndex];
   end
  else inherited;
end;

procedure TCustomDelayLineSamples32.BufferSizeChanged;
begin
 Assert(FBufferSize > 0);
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 if FBufferPos >= FBufferSize then
   FBufferPos := 0;
end;

procedure TCustomDelayLineSamples32.Reset;
begin
 inherited;
 ClearBuffer;
end;

procedure TCustomDelayLineSamples32.ClearBuffer;
begin
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;


{ TCustomDelayLineSamples64 }

constructor TCustomDelayLineSamples64.Create(const BufferSize: Integer = 0);
begin
 FBuffer := nil;
 inherited Create(BufferSize);
 ClearBuffer;
end;

destructor TCustomDelayLineSamples64.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

function TCustomDelayLineSamples64.GetSample(Index: Integer): Double;
var
  Pos: Integer;
begin
 if (Index < 0) or (Index >= FBufferSize)
  then raise Exception.CreateFmt('Index out of bounds(%d)', [Index]);

 Pos := FBufferPos - Index;
 if Pos < 0
  then Inc(Pos, FBufferSize);
 Result := FBuffer^[Pos];
end;

procedure TCustomDelayLineSamples64.AssignTo(Dest: TPersistent);
var
  SampleIndex : Integer;
begin
 if Dest is TCustomDelayLineSamples32 then
  with TCustomDelayLineSamples32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for SampleIndex := 0 to FBufferSize - 1
     do Self.FBuffer^[SampleIndex] := FBuffer^[SampleIndex];
   end else
 if Dest is TCustomDelayLineSamples64 then
  with TCustomDelayLineSamples64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer^, Self.FBuffer^, Self.FBufferSize * SizeOf(Double));
   end
  else inherited;
end;

procedure TCustomDelayLineSamples64.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
end;

procedure TCustomDelayLineSamples64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TCustomDelayLineSamples64.ProcessSample64(Input: Double): Double;
begin
 Result := FBuffer^[FBufferPos];
 FBuffer^[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TCustomDelayLineSamples64.Reset;
begin
 inherited;
 ClearBuffer;
end;

procedure TCustomDelayLineSamples64.ClearBuffer;
begin
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;

function TCustomDelayLineSamples64.GetAbsMax: Double;
var
  Pos: Integer;
begin
 Assert(FBufferSize > 0);

 Result := Abs(FBuffer^[0]);

 for Pos := 1 to FBufferSize - 1 do
  if Abs(FBuffer^[Pos]) > Result
   then Result := Abs(FBuffer^[Pos]);
end;

function TCustomDelayLineSamples64.GetMaximum: Double;
var
  Pos: Integer;
begin
 Assert(FBufferSize > 0);

 Result := FBuffer^[0];

 for Pos := 1 to FBufferSize - 1 do
  if FBuffer^[Pos] > Result then Result := FBuffer^[Pos];
end;

function TCustomDelayLineSamples64.GetMinimum: Double;
var
  Pos: Integer;
begin
 Assert(FBufferSize > 0);

 Result := FBuffer^[0];

 for Pos := 1 to FBufferSize - 1 do
  if FBuffer^[Pos] < Result then Result := FBuffer^[Pos];
end;


{ TDelayLineSamples }

constructor TDelayLineSamples.Create(const BufferSize: Integer = 0);
begin
 FBuffer := nil;
 inherited Create(BufferSize);
 ClearBuffer;
end;

destructor TDelayLineSamples.Destroy;
begin
 if Assigned(FBuffer)
  then FreeMem(FBuffer);
 inherited;
end;

function TDelayLineSamples.GetSample(Index: Integer): Double;
var
  Pos: Integer;
begin
 if (Index < 0) or (Index >= FBufferSize)
  then raise Exception.CreateFmt('Index out of bounds(%d)', [Index]);

 Pos := FBufferPos - Index;
 if Pos < 0
  then Inc(Pos, FBufferSize);

 case FBufferFloatType of
  ft16 : Result := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[Pos]);
  ft32 : Result := PDAVSingleFixedArray(FBuffer)^[Pos];
  ft64 : Result := PDAVDoubleFixedArray(FBuffer)^[Pos];
  else raise Exception.Create(RCStrUnknownFloatingPointFormat);
 end;
end;

procedure TDelayLineSamples.AssignTo(Dest: TPersistent);
var
  SampleIndex : Integer;
begin
 if Dest is TDelayLineSamples32 then
  with TDelayLineSamples32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    case FBufferFloatType of
     ft16 : for SampleIndex := 0 to FBufferSize - 1
             do FBuffer^[SampleIndex] := HalfFloatToSingle(PDAVHalfFloatFixedArray(Self.FBuffer)^[SampleIndex]);
     ft32 : Move(FBuffer^, Self.FBuffer^, Self.FBufferSize * FBytesPerSample);
     ft64 : for SampleIndex := 0 to FBufferSize - 1
             do FBuffer^[SampleIndex] := PDAVDoubleFixedArray(Self.FBuffer)^[SampleIndex];
    end;
   end else
 if Dest is TDelayLineSamples64 then
  with TDelayLineSamples64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    case FBufferFloatType of
     ft16 : for SampleIndex := 0 to FBufferSize - 1
             do FBuffer^[SampleIndex] := HalfFloatToSingle(PDAVHalfFloatFixedArray(Self.FBuffer)^[SampleIndex]);
     ft32 : for SampleIndex := 0 to FBufferSize - 1
             do FBuffer^[SampleIndex] := PDAVSingleFixedArray(Self.FBuffer)^[SampleIndex];
     ft64 : Move(FBuffer^, Self.FBuffer^, Self.FBufferSize * FBytesPerSample);
    end;
   end
  else inherited;
end;

procedure TDelayLineSamples.BufferSizeChanged;
begin
 case FBufferFloatType of
  ft16 : FBytesPerSample := SizeOf(THalfFloat);
  ft32 : FBytesPerSample := SizeOf(Single);
  ft64 : FBytesPerSample := SizeOf(Double);
 end;

 ReallocMem(FBuffer, FBufferSize * FBytesPerSample);
end;

procedure TDelayLineSamples.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TDelayLineSamples.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDelayLineSamples.ProcessSample32(Input: Single): Double;
begin
 case FBufferFloatType of
  ft16 : begin
          Result := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[FBufferPos]);
          PDAVHalfFloatFixedArray(FBuffer)^[FBufferPos] := SingleToHalfFloat(Input);
         end;
  ft32 : begin
          Result := PDAVSingleFixedArray(FBuffer)^[FBufferPos];
          PDAVSingleFixedArray(FBuffer)^[FBufferPos] := Input;
         end;
  ft64 : begin
          Result := PDAVDoubleFixedArray(FBuffer)^[FBufferPos];
          PDAVDoubleFixedArray(FBuffer)^[FBufferPos] := Input;
         end;
  else raise Exception.Create(RCStrUnknownFloatingPointFormat);
 end;

 Inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

function TDelayLineSamples.ProcessSample64(Input: Double): Double;
begin
 case FBufferFloatType of
  ft16 : begin
          Result := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[FBufferPos]);
          PDAVHalfFloatFixedArray(FBuffer)^[FBufferPos] := SingleToHalfFloat(Input);
         end;
  ft32 : begin
          Result := PDAVSingleFixedArray(FBuffer)^[FBufferPos];
          PDAVSingleFixedArray(FBuffer)^[FBufferPos] := Input;
         end;
  ft64 : begin
          Result := PDAVDoubleFixedArray(FBuffer)^[FBufferPos];
          PDAVDoubleFixedArray(FBuffer)^[FBufferPos] := Input;
         end;
  else raise Exception.Create(RCStrUnknownFloatingPointFormat);
 end;

 Inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TDelayLineSamples.Reset;
begin
 inherited;
 ClearBuffer;
end;

procedure TDelayLineSamples.SetBufferFloatType(const Value: TBufferFloatType);
begin
 if FBufferFloatType <> Value then
  begin
   FBufferFloatType := Value;
   BufferFloatTypeChanged;
  end;
end;

procedure TDelayLineSamples.BufferFloatTypeChanged;
begin
 case FBufferFloatType of
  ft16 : FBytesPerSample := 2;
  ft32 : FBytesPerSample := 4;
  ft64 : FBytesPerSample := 8;
 end;

 BufferSizeChanged;
 ClearBuffer;
end;

procedure TDelayLineSamples.ClearBuffer;
begin
 FillChar(FBuffer^, FBufferSize * FBytesPerSample, 0);
end;

function TDelayLineSamples.GetAbsMax: Double;
var
  Pos: Integer;
begin
 Assert(FBufferSize > 0);

 case FBufferFloatType of
  ft16 : begin
          Result := Abs(HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[0]));
          for Pos := 1 to FBufferSize - 1 do
           if Abs(HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[Pos])) > Result
            then Result := Abs(HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[Pos]));
         end;
  ft32 : begin
          Result := Abs(PDAVSingleFixedArray(FBuffer)^[0]);
          for Pos := 1 to FBufferSize - 1 do
           if Abs(PDAVSingleFixedArray(FBuffer)^[Pos]) > Result
            then Result := Abs(PDAVSingleFixedArray(FBuffer)^[Pos]);
         end;
  ft64 : begin
          Result := Abs(PDAVDoubleFixedArray(FBuffer)^[0]);
          for Pos := 1 to FBufferSize - 1 do
           if Abs(PDAVDoubleFixedArray(FBuffer)^[Pos]) > Result
            then Result := Abs(PDAVDoubleFixedArray(FBuffer)^[Pos]);
         end;
  else raise Exception.Create(RCStrUnknownFloatingPointFormat);
 end;
end;

function TDelayLineSamples.GetMaximum: Double;
var
  Pos: Integer;
begin
 Assert(FBufferSize > 0);

 case FBufferFloatType of
  ft16 : begin
          Result := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[0]);
          for Pos := 1 to FBufferSize - 1 do
           if HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[Pos]) > Result
            then Result := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[Pos]);
         end;
  ft32 : begin
          Result := PDAVSingleFixedArray(FBuffer)^[0];
          for Pos := 1 to FBufferSize - 1 do
           if PDAVSingleFixedArray(FBuffer)^[Pos] > Result
            then Result := PDAVSingleFixedArray(FBuffer)^[Pos];
         end;
  ft64 : begin
          Result := PDAVDoubleFixedArray(FBuffer)^[0];
          for Pos := 1 to FBufferSize - 1 do
           if PDAVDoubleFixedArray(FBuffer)^[Pos] > Result
            then Result := PDAVDoubleFixedArray(FBuffer)^[Pos];
         end;
  else raise Exception.Create(RCStrUnknownFloatingPointFormat);
 end;
end;

function TDelayLineSamples.GetMinimum: Double;
var
  Pos: Integer;
begin
 Assert(FBufferSize > 0);

 case FBufferFloatType of
  ft16 : begin
          Result := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[0]);
          for Pos := 1 to FBufferSize - 1 do
           if HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[Pos]) < Result
            then Result := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer)^[Pos]);
         end;
  ft32 : begin
          Result := PDAVSingleFixedArray(FBuffer)^[0];
          for Pos := 1 to FBufferSize - 1 do
           if PDAVSingleFixedArray(FBuffer)^[Pos] < Result
            then Result := PDAVSingleFixedArray(FBuffer)^[Pos];
         end;
  ft64 : begin
          Result := PDAVDoubleFixedArray(FBuffer)^[0];
          for Pos := 1 to FBufferSize - 1 do
           if PDAVDoubleFixedArray(FBuffer)^[Pos] < Result
            then Result := PDAVDoubleFixedArray(FBuffer)^[Pos];
         end;
  else raise Exception.Create(RCStrUnknownFloatingPointFormat);
 end;
end;


{ TCustomDelayLineFractional }

constructor TCustomDelayLineFractional.Create(const FractionalBufferSize: Double = 0);
begin
 Assert(FractionalBufferSize >= 0);
 inherited Create(Trunc(FractionalBufferSize));
 FFractional := FractionalBufferSize - FBufferSize;
end;

procedure TCustomDelayLineFractional.FractionalChanged;
begin
 Assert(FFractional >= 0);
end;

function TCustomDelayLineFractional.GetFractional: Double;
begin
 Result := 2 + FBufferSize + FFractional;
end;

procedure TCustomDelayLineFractional.SetFractional(const Value: Double);
begin
 if FractionalBuffersize <> Value then
  begin
(*
   if Value > 2 then
    begin
     BufferSize := Trunc(Value) - 2
    end
   else
    begin
     BufferSize := 0;
     FFractional := Value - Trunc(Value);
    end;
*)
   BufferSize := Trunc(Value);
   FFractional := Value - BufferSize;
   FractionalChanged;
  end;
end;


{ TDelayLineFractional32 }

constructor TDelayLineFractional32.Create(const FractionalBufferSize: Double = 0);
begin
 FBuffer := nil;
 inherited Create(Trunc(FractionalBufferSize));
 FIntBuffer[3] := 0;
end;

destructor TDelayLineFractional32.Destroy;
begin
// Assert(FBuffer[BufferSize - 1] = 0);
 Dispose(FBuffer);
 inherited;
end;

procedure TDelayLineFractional32.BufferSizeChanged;
var
  RealBufferPos : Integer;
begin
 // damn hack!!!
 RealBufferPos := Max(4, FBufferSize);

 if FBufferPos > RealBufferPos
  then FBufferPos := 0;
 ReallocMem(FBuffer, RealBufferPos * SizeOf(Single));
 FillChar(FBuffer^, RealBufferPos * SizeOf(Single), 0);
// ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
end;

procedure TDelayLineFractional32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDelayLineFractional32.ProcessSample32(Input: Single): Single;
begin
 FBuffer^[FBufferPos] := Input;

 Inc(FBufferPos);
 if FBufferPos >= BufferSize - 1
  then FBufferPos := 0;

 Move(FIntBuffer[0], FIntBuffer[1], 3 * SizeOf(Single));
 FIntBuffer[0] := FBuffer^[FBufferPos];
 Result := Hermite32_asm(FFractional, @FIntBuffer);
end;

procedure TDelayLineFractional32.Reset;
begin
 inherited;
 ClearBuffer;
end;

procedure TDelayLineFractional32.ClearBuffer;
begin
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;


{ TDelayLineFractional64 }

constructor TDelayLineFractional64.Create(const FractionalBufferSize: Double = 0);
begin
 FBuffer := nil;
 inherited Create(Trunc(FractionalBufferSize));
 FIntBuffer[3] := 0;
end;

destructor TDelayLineFractional64.Destroy;
begin
// Assert(FBuffer[BufferSize - 1] = 0);
 Dispose(FBuffer);
 inherited;
end;

procedure TDelayLineFractional64.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
end;

procedure TDelayLineFractional64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDelayLineFractional64.ProcessSample64(Input: Double): Double;
begin
 FBuffer^[FBufferPos] := Input;

 inc(FBufferPos);
 if FBufferPos >= BufferSize - 1
  then FBufferPos := 0;

 Move(FIntBuffer[0], FIntBuffer[1], 3 * SizeOf(Double));
 FIntBuffer[0] := FBuffer^[FBufferPos];
 Result := Hermite64_asm(FFractional, @FIntBuffer);
end;

procedure TDelayLineFractional64.Reset;
begin
 inherited;
 ClearBuffer;
end;

procedure TDelayLineFractional64.ClearBuffer;
begin
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;


{ TDelayLineTime32 }

constructor TDelayLineTime32.Create(const BufferSize: Integer);
begin
 inherited Create;
 FFractionalDelay := TDelayLineFractional32.Create(BufferSize);
 FTime := 0;
end;

destructor TDelayLineTime32.Destroy;
begin
 FreeAndNil(FFractionalDelay);
 inherited;
end;

procedure TDelayLineTime32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample32(Data[SampleIndex]);
end;

function TDelayLineTime32.ProcessSample32(Input: Single): Single;
begin
 Result := FFractionalDelay.ProcessSample32(Input);
end;

procedure TDelayLineTime32.Reset;
begin
 inherited;
 FFractionalDelay.Reset;
end;

procedure TDelayLineTime32.SetTime(const Value: Double);
begin
 if FTime <> Value then
  begin
   FTime := Value;
   TimeChanged;
  end;
end;

procedure TDelayLineTime32.TimeChanged;
begin
 FFractionalDelay.FractionalBufferSize := FTime * SampleRate;
end;


{ TDelayLineTime64 }

constructor TDelayLineTime64.Create(const BufferSize: Integer);
begin
 inherited Create;
 FFractionalDelay := TDelayLineFractional64.Create(BufferSize);
 FTime := 0;
end;

destructor TDelayLineTime64.Destroy;
begin
 FreeAndNil(FFractionalDelay);
 inherited;
end;

procedure TDelayLineTime64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDelayLineTime64.ProcessSample64(Input: Double): Double;
begin
 Result := FFractionalDelay.ProcessSample64(Input);
end;

procedure TDelayLineTime64.Reset;
begin
 inherited;
 FFractionalDelay.Reset;
end;

procedure TDelayLineTime64.SetTime(const Value: Double);
begin
 if FTime <> Value then
  begin
   FTime := Value;
   TimeChanged;
  end;
end;

procedure TDelayLineTime64.TimeChanged;
begin
 FFractionalDelay.FractionalBufferSize := FTime * SampleRate;
end;

initialization
  RegisterDspProcessors32([TDelayLineSamples32, TDelayLineFractional32]);
  RegisterDspProcessors64([TDelayLineSamples64, TDelayLineFractional64]);

end.
