unit DAV_DspSpectralEffects;

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

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_ClassesFft, DAV_Complex,
  {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF}
  DAV_DspFftReal2Complex;

// TODO: check and implement all assignto functions!!!

type
  TCustomSpectralEffect = class(TDspSampleRateFftPersistent)
  protected
    FBlockPosition : Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ClearIOBuffers; virtual; abstract;
  public
    constructor Create; override;
    procedure Clear; virtual;
  end;

  TCustomSpectralEffect32 = class(TCustomSpectralEffect)
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
    FOutputBuffer  : PDAVSingleFixedArray;

    procedure AssignTo(Dest: TPersistent); override;
    procedure ClearIOBuffers; override;
    procedure FFTOrderChanged; override;
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVSingleFixedArray); overload; virtual;
    procedure PerformSpectralEffect(Spectum: PDAVComplexSingleFixedArray); overload; virtual; abstract;

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

    procedure ProcessBlock(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer); virtual;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    function ProcessSample32(Input: Single): Single; virtual;
  end;

  TCustomSpectralEffect64 = class(TCustomSpectralEffect)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat64;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat64;
    {$ENDIF}
  protected
    FSignalFreq    : PDAVComplexDoubleFixedArray;
    FInputBuffer   : PDAVDoubleFixedArray;
    FOutputBuffer  : PDAVDoubleFixedArray;

    procedure AssignTo(Dest: TPersistent); override;
    procedure FFTOrderChanged; override;
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVDoubleFixedArray); overload; virtual;
    procedure PerformSpectralEffect(Spectum: PDAVComplexDoubleFixedArray); overload; virtual; abstract;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat64 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat64 read GetFft;
    {$ENDIF}
    procedure ClearIOBuffers; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock(const Input, Output: PDAVDoubleFixedArray; const SampleFrames: Integer); virtual;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    function ProcessSample64(Input: Double): Double; virtual;
  end;

implementation

uses
  Math, SysUtils;

resourcestring
  RCStrIRBlockOrderError = 'Maximum IR block order must be larger or equal ' +
    'the minimum IR block order!';

{ TCustomSpectralEffect }

constructor TCustomSpectralEffect.Create;
begin
 inherited;
 FBlockPosition := 0;
end;

procedure TCustomSpectralEffect.Clear;
begin
  ClearIOBuffers;
end;

procedure TCustomSpectralEffect.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSpectralEffect then
  with TCustomSpectralEffect(Dest) do
   begin
    inherited;
    FBlockPosition := Self.FBlockPosition;
   end
  else inherited;
end;


{ TCustomSpectralEffect32 }

constructor TCustomSpectralEffect32.Create;
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

destructor TCustomSpectralEffect32.Destroy;
begin
 Dispose(FSignalFreq);
 Dispose(FInputBuffer);
 Dispose(FOutputBuffer);
 FreeAndNil(FFft);
 inherited;
end;

procedure TCustomSpectralEffect32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSpectralEffect32 then
  with TCustomSpectralEffect32(Dest) do
   begin
    inherited;
    FBlockPosition := Self.FBlockPosition;
   end
 else inherited;
end;

procedure TCustomSpectralEffect32.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FInputBuffer, FFFTSize * SizeOf(Single));
 ReallocMem(FOutputBuffer, FFFTSize * SizeOf(Single));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
 ClearIOBuffers;
end;

procedure TCustomSpectralEffect32.ClearIOBuffers;
begin
 FillChar(FInputBuffer^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FOutputBuffer^[0], FFFTSize * SizeOf(Single), 0);
end;

{$IFDEF Use_IPPS}
function TCustomSpectralEffect32.GetFft : TFftReal2ComplexIPPSFloat32;
begin
 Result := TFftReal2ComplexIPPSFloat32(FFft);
end;

{$ELSE} {$IFDEF Use_CUDA}

function TCustomSpectralEffect32.GetFft : TFftReal2ComplexCUDA32;
begin
 Result := TFftReal2ComplexCUDA32(FFft);
end;

{$ELSE}

function TCustomSpectralEffect32.GetFft : TFftReal2ComplexNativeFloat32;
begin
 Result := TFftReal2ComplexNativeFloat32(FFft);
end;
{$ENDIF}{$ENDIF}

procedure TCustomSpectralEffect32.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVSingleFixedArray);
begin
 Fft.PerformFFT(FSignalFreq, SignalIn);
 PerformSpectralEffect(FSignalFreq);
 Fft.PerformIFFT(FSignalFreq, SignalOut);
end;

procedure TCustomSpectralEffect32.ProcessBlock(const Input,
  Output: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FFFTSizeHalf then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));

    // increase block position and Break
    Inc(FBlockPosition, SampleFrames - CurrentPosition);
    Exit;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Single));

    PerformSpectralEffect(FInputBuffer, FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FFFTSizeHalf - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TCustomSpectralEffect32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleFrames: Integer);
begin
 ProcessBlock(Data, Data, SampleFrames);
end;

function TCustomSpectralEffect32.ProcessSample32(Input: Single): Single;
begin
 // copy to ring buffer only
 FInputBuffer^[FFFTSizeHalf + FBlockPosition] := Input;
 Result := FOutputBuffer^[FBlockPosition];

 // increase block position and Break
 Inc(FBlockPosition, 1);
 if FBlockPosition >= FFFTSizeHalf then
  begin
   PerformSpectralEffect(FInputBuffer, FOutputBuffer);

   // discard already used input buffer part to make space for new data
   Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Single));

   // increase current position and reset block position
   FBlockPosition := 0;
  end;
end;


{ TCustomSpectralEffect64 }

constructor TCustomSpectralEffect64.Create;
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

destructor TCustomSpectralEffect64.Destroy;
begin
 Dispose(FSignalFreq);
 Dispose(FInputBuffer);
 Dispose(FOutputBuffer);
 FreeAndNil(FFft);
 inherited;
end;

procedure TCustomSpectralEffect64.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSpectralEffect64 then
  with TCustomSpectralEffect64(Dest) do
   begin
    inherited;
    FBlockPosition := Self.FBlockPosition;
   end
 else inherited;
end;

procedure TCustomSpectralEffect64.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FInputBuffer, FFFTSize * SizeOf(Double));
 ReallocMem(FOutputBuffer, FFFTSize * SizeOf(Double));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64), 0);
 ClearIOBuffers;
end;

procedure TCustomSpectralEffect64.ClearIOBuffers;
begin
 FillChar(FInputBuffer^[0], FFFTSize * SizeOf(Double), 0);
 FillChar(FOutputBuffer^[0], FFFTSize * SizeOf(Double), 0);
end;

{$IFDEF Use_IPPS}
function TCustomSpectralEffect64.GetFft : TFftReal2ComplexIPPSFloat64;
begin
 Result := TFftReal2ComplexIPPSFloat64(FFft);
end;

{$ELSE}

function TCustomSpectralEffect64.GetFft : TFftReal2ComplexNativeFloat64;
begin
 Result := TFftReal2ComplexNativeFloat64(FFft);
end;
{$ENDIF}

procedure TCustomSpectralEffect64.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVDoubleFixedArray);
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 PerformSpectralEffect(FSignalFreq);
 FFft.PerformIFFT(FSignalFreq, SignalOut);
end;

procedure TCustomSpectralEffect64.ProcessBlock(const Input,
  Output: PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FFFTSizeHalf then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));

    // increase block position and Break
    Inc(FBlockPosition, SampleFrames - CurrentPosition);
    Break;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Double));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Double));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FFFTSizeHalf], FOutputBuffer^[0], FFFTSizeHalf * SizeOf(Double));
    FillChar(FOutputBuffer^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Double), 0);

    PerformSpectralEffect(FInputBuffer, FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Double));

    // increase current position and reset block position
    Inc(CurrentPosition, (FFFTSizeHalf - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TCustomSpectralEffect64.ProcessBlock64(
  const Data: PDAVDoubleFixedArray; SampleFrames: Integer);
begin
 ProcessBlock(Data, Data, SampleFrames);
end;

function TCustomSpectralEffect64.ProcessSample64(Input: Double): Double;
begin
 // copy to ring buffer only
 FInputBuffer^[FFFTSizeHalf + FBlockPosition] := Input;
 Result := FOutputBuffer^[FBlockPosition];

 // increase block position and Break
 Inc(FBlockPosition, 1);
 if FBlockPosition >= FFFTSizeHalf then
  begin
   PerformSpectralEffect(FInputBuffer, FOutputBuffer);

   // discard already used input buffer part to make space for new data
   Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Double));

   // increase current position and reset block position
   FBlockPosition := 0;
  end;
end;

end.
