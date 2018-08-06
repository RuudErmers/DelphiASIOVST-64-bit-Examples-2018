unit DAV_DspSpectralAnalysis;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_ClassesFft, DAV_Complex,
  {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF}
  DAV_DspFftReal2Complex;

type
  TCustomSpectralAnalysis = class(TDspSampleRateFftPersistent)
  protected
    FBlockPosition : Integer;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
  end;

  TCustomSpectralAnalysis32 = class(TCustomSpectralAnalysis)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    function GetFft : TFftReal2ComplexCUDA32;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
  protected
    FSignalFreq    : PDAVComplexSingleFixedArray;
    FInputBuffer   : PDAVSingleFixedArray;

    procedure AssignTo(Dest: TPersistent); override;
    procedure FFTOrderChanged; override;
    procedure PerformAnalysis(SignalIn: PDAVSingleFixedArray); overload; virtual;
    procedure PerformAnalysis(Spectum: PDAVComplexSingleFixedArray); overload; virtual; abstract;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat32 read GetFft;
    {$ELSE} {$IFDEF Use_CUDA}
    property Fft : TFftReal2ComplexCUDA32 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat32  read GetFft;
    {$ENDIF}{$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock(const Input: PDAVSingleFixedArray; const SampleFrames: Integer); virtual;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    function ProcessSample32(Input: Single): Single; virtual;
  end;

  TCustomSpectralAnalysis64 = class(TCustomSpectralAnalysis)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat64;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat64;
    {$ENDIF}
  protected
    FSignalFreq    : PDAVComplexDoubleFixedArray;
    FInputBuffer   : PDAVDoubleFixedArray;

    procedure AssignTo(Dest: TPersistent); override;
    procedure FFTOrderChanged; override;
    procedure PerformAnalysis(SignalIn: PDAVDoubleFixedArray); overload; virtual;
    procedure PerformAnalysis(Spectum: PDAVComplexDoubleFixedArray); overload; virtual; abstract;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat64 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat64 read GetFft;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input: PDAVDoubleFixedArray; const SampleFrames: Integer); virtual;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    function ProcessSample64(Input: Double): Double; virtual;
  end;

  TALCCISChannel = class(TCustomSpectralAnalysis32)

  end;

implementation

uses
  Math, SysUtils;

resourcestring
  RCStrIRBlockOrderError = 'Maximum IR block order must be larger or equal ' +
    'the minimum IR block order!';

{ TCustomSpectralAnalysis }

constructor TCustomSpectralAnalysis.Create;
begin
 inherited;
 FBlockPosition := 0;
end;

procedure TCustomSpectralAnalysis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSpectralAnalysis then
  with TCustomSpectralAnalysis(Dest) do
   begin
    inherited;
    FBlockPosition := Self.FBlockPosition;
   end
  else inherited;
end;


{ TCustomSpectralAnalysis32 }

constructor TCustomSpectralAnalysis32.Create;
begin
 inherited;

 FSignalFreq := nil;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(6);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}
 FFTOrderChanged;
end;

destructor TCustomSpectralAnalysis32.Destroy;
begin
 Dispose(FSignalFreq);
 Dispose(FInputBuffer);
 FreeAndNil(FFft);
 inherited;
end;

procedure TCustomSpectralAnalysis32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSpectralAnalysis32 then
  with TCustomSpectralAnalysis32(Dest) do
   begin
    inherited;
    FBlockPosition := Self.FBlockPosition;
   end
 else inherited;
end;

procedure TCustomSpectralAnalysis32.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FInputBuffer, FFFTSize * SizeOf(Single));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));

 FillChar(FInputBuffer^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
end;

{$IFDEF Use_IPPS}
function TCustomSpectralAnalysis32.GetFft : TFftReal2ComplexIPPSFloat32;
begin
 Result := TFftReal2ComplexIPPSFloat32(FFft);
end;

{$ELSE} {$IFDEF Use_CUDA}

function TCustomSpectralAnalysis32.GetFft : TFftReal2ComplexCUDA32;
begin
 Result := TFftReal2ComplexCUDA32(FFft);
end;

{$ELSE}

function TCustomSpectralAnalysis32.GetFft : TFftReal2ComplexNativeFloat32;
begin
 Result := TFftReal2ComplexNativeFloat32(FFft);
end;
{$ENDIF}{$ENDIF}

procedure TCustomSpectralAnalysis32.PerformAnalysis(SignalIn: PDAVSingleFixedArray);
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 PerformAnalysis(FSignalFreq);
end;

procedure TCustomSpectralAnalysis32.ProcessBlock(
  const Input: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FFFTSizeHalf then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));

    // increase block position and Break
    Inc(FBlockPosition, SampleFrames - CurrentPosition);
    Exit;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Single));

    PerformAnalysis(FInputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FFFTSizeHalf - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TCustomSpectralAnalysis32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleFrames: Integer);
begin
 ProcessBlock(Data, SampleFrames);
end;

function TCustomSpectralAnalysis32.ProcessSample32(Input: Single): Single;
begin
 // copy to ring buffer only
 FInputBuffer^[FFFTSizeHalf + FBlockPosition] := Input;

 // increase block position and Break
 Inc(FBlockPosition, 1);
 if FBlockPosition >= FFFTSizeHalf then
  begin
   PerformAnalysis(FInputBuffer);

   // discard already used input buffer part to make space for new data
   Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Single));

   // increase current position and reset block position
   FBlockPosition := 0;
  end;
end;


{ TCustomSpectralAnalysis64 }

constructor TCustomSpectralAnalysis64.Create;
begin
 inherited;

 FSignalFreq := nil;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat64.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat64.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}
 FFTOrderChanged;
end;

destructor TCustomSpectralAnalysis64.Destroy;
begin
 Dispose(FSignalFreq);
 Dispose(FInputBuffer);
 FreeAndNil(FFft);
 inherited;
end;

procedure TCustomSpectralAnalysis64.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSpectralAnalysis64 then
  with TCustomSpectralAnalysis64(Dest) do
   begin
    inherited;
    FBlockPosition := Self.FBlockPosition;
   end
 else inherited;
end;

procedure TCustomSpectralAnalysis64.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FInputBuffer, FFFTSize * SizeOf(Double));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64));

 FillChar(FInputBuffer^[0], FFFTSize * SizeOf(Double), 0);
 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64), 0);
end;

{$IFDEF Use_IPPS}
function TCustomSpectralAnalysis64.GetFft : TFftReal2ComplexIPPSFloat64;
begin
 Result := TFftReal2ComplexIPPSFloat64(FFft);
end;

{$ELSE}

function TCustomSpectralAnalysis64.GetFft : TFftReal2ComplexNativeFloat64;
begin
 Result := TFftReal2ComplexNativeFloat64(FFft);
end;
{$ENDIF}

procedure TCustomSpectralAnalysis64.PerformAnalysis(SignalIn: PDAVDoubleFixedArray);
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 PerformAnalysis(FSignalFreq);
end;

procedure TCustomSpectralAnalysis64.ProcessBlock(
  const Input: PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FFFTSizeHalf then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));

    // increase block position and Break
    Inc(FBlockPosition, SampleFrames - CurrentPosition);
    Break;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Double));

    PerformAnalysis(FInputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Double));

    // increase current position and reset block position
    Inc(CurrentPosition, (FFFTSizeHalf - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TCustomSpectralAnalysis64.ProcessBlock64(
  const Data: PDAVDoubleFixedArray; SampleFrames: Integer);
begin
 ProcessBlock(Data, SampleFrames);
end;

function TCustomSpectralAnalysis64.ProcessSample64(Input: Double): Double;
begin
 // copy to ring buffer only
 FInputBuffer^[FFFTSizeHalf + FBlockPosition] := Input;

 // increase block position and Break
 Inc(FBlockPosition, 1);
 if FBlockPosition >= FFFTSizeHalf then
  begin
   PerformAnalysis(FInputBuffer);

   // discard already used input buffer part to make space for new data
   Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Double));

   // increase current position and reset block position
   FBlockPosition := 0;
  end;
end;

end.

