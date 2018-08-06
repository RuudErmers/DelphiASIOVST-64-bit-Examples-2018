unit DAV_DspCorrelation;

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
  Classes, DAV_Types, DAV_Complex, DAV_Classes, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS{$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

type
  TCustomCorrelation = class(TDspPersistent)
  private
    function GetFftOrder: Byte;
    procedure SetFftOrder(const Value: Byte);
  protected
    FFFT         : TFftReal2Complex;
    FFFTSize     : Integer;
    FFFTSizeHalf : Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ImpulseResponseChanged; virtual; abstract;
    procedure FFTOrderChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property FFTOrder: Byte read GetFftOrder write SetFftOrder;
    property FFTSize: Integer read FFFTSize;
  end;

  TCorrelation32 = class(TCustomCorrelation)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    function GetFft : TFftReal2ComplexCUDA32;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
  protected
    FSignalFreq      : PDAVComplexSingleFixedArray;
    FCorrelationFreq : PDAVComplexSingleFixedArray;

    procedure AssignTo(Dest: TPersistent); override;
    procedure FFTOrderChanged; override;

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

    procedure CrossCorrelation(const SignalA, SignalB, Correlation: PDAVSingleFixedArray); overload; virtual;
    procedure CrossCorrelation(const Signal, SignalCorrelation: PDAVSingleFixedArray); overload; virtual;
    procedure AutoCorrelation(const Signal, Correlation: PDAVSingleFixedArray); overload; virtual;
    procedure AutoCorrelation(const SignalCorrelation: PDAVSingleFixedArray); overload; virtual;
  published
    property FFTOrder;
    property FFTSize;
  end;

  TCorrelation64 = class(TCustomCorrelation)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat64;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat64;
    {$ENDIF}
  protected
    FSignalFreq         : PDAVComplexDoubleFixedArray;
    FCorrelationFreq    : PDAVComplexDoubleFixedArray;

    procedure FFTOrderChanged; override;
    procedure AssignTo(Dest: TPersistent); override;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat64 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat64 read GetFft;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CrossCorrelation(const SignalA, SignalB, Correlation: PDAVDoubleFixedArray); overload; virtual;
    procedure CrossCorrelation(const Signal, SignalCorrelation: PDAVDoubleFixedArray); overload; virtual;
    procedure AutoCorrelation(const Signal, Correlation: PDAVDoubleFixedArray); overload; virtual;
    procedure AutoCorrelation(const SignalCorrelation: PDAVDoubleFixedArray); overload; virtual;
  published
    property FFTOrder;
  end;

implementation

uses
  Math, SysUtils, DAV_BlockProcessing;

resourcestring
  RCStrIRBlockOrderError = 'Maximum IR block order must be larger or equal ' +
    'the minimum IR block order!';

{ TCustomCorrelation }

procedure TCustomCorrelation.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomCorrelation then
  with TCustomCorrelation(Dest) do
   begin
    inherited;
    FFFT.Assign(Self.FFFT);
    FFFTSize     := Self.FFFTSize;
    FFFTSizeHalf := Self.FFFTSizeHalf;
   end
 else inherited;
end;

constructor TCustomCorrelation.Create;
begin
 inherited;
end;

destructor TCustomCorrelation.Destroy;
begin
 FreeAndNil(FFft);
 inherited;
end;

function TCustomCorrelation.GetFftOrder: Byte;
begin
 Result := FFft.Order;
end;

procedure TCustomCorrelation.FFTOrderChanged;
begin
 FFFTSize     := FFft.FFTSize;
 FFFTSizeHalf := FFFTSize shr 1;
 FFft.AutoScaleType := astDivideInvByN;
end;

procedure TCustomCorrelation.SetFftOrder(const Value: Byte);
begin
 if FFft.Order <> Value then
  begin
   FFft.Order := Value;
   FFTOrderChanged;
  end;
end;

{ TCorrelation32 }

constructor TCorrelation32.Create;
begin
 inherited;

 FSignalFreq      := nil;
 FCorrelationFreq := nil;

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

destructor TCorrelation32.Destroy;
begin
 Dispose(FSignalFreq);
 Dispose(FCorrelationFreq);
 FreeAndNil(FFft);
 inherited;
end;

procedure TCorrelation32.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
 ReallocMem(FCorrelationFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
 FillChar(FCorrelationFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
end;

{$IFDEF Use_IPPS}
function TCorrelation32.GetFft : TFftReal2ComplexIPPSFloat32;
begin
 Result := TFftReal2ComplexIPPSFloat32(FFft);
end;

{$ELSE} {$IFDEF Use_CUDA}

function TCorrelation32.GetFft : TFftReal2ComplexCUDA32;
begin
 Result := TFftReal2ComplexCUDA32(FFft);
end;

{$ELSE}

function TCorrelation32.GetFft : TFftReal2ComplexNativeFloat32;
begin
 Result := TFftReal2ComplexNativeFloat32(FFft);
end;
{$ENDIF}{$ENDIF}

procedure TCorrelation32.AssignTo(Dest: TPersistent);
var
  Bin : Integer;
begin
 inherited;
 if Dest is TCorrelation32 then
  with TCorrelation32(Dest) do 
   begin
    Move(Self.FSignalFreq^, FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
    Move(Self.FCorrelationFreq^, FCorrelationFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
   end else
 if Dest is TCorrelation64 then
  with TCorrelation64(Dest) do
   begin
    for Bin := 0 to FFFTSizeHalf do
     begin
      FSignalFreq^[Bin].Re := Self.FSignalFreq^[Bin].Re;
      FSignalFreq^[Bin].Im := Self.FSignalFreq^[Bin].Im;
      FCorrelationFreq^[Bin].Re := Self.FCorrelationFreq^[Bin].Re;
      FCorrelationFreq^[Bin].Im := Self.FCorrelationFreq^[Bin].Im;
     end;
   end;
end;

procedure TCorrelation32.AutoCorrelation(
  const SignalCorrelation: PDAVSingleFixedArray);
begin
 AutoCorrelation(SignalCorrelation, SignalCorrelation);
end;

procedure TCorrelation32.AutoCorrelation(const Signal,
  Correlation: PDAVSingleFixedArray);
begin
 FFft.PerformFFT(FCorrelationFreq, Signal);
 ComplexMultiplyConjugated32(FCorrelationFreq, FCorrelationFreq, FFFTSizeHalf);
 FFft.PerformIFFT(FCorrelationFreq, Correlation);
end;

procedure TCorrelation32.CrossCorrelation(const Signal,
  SignalCorrelation: PDAVSingleFixedArray);
begin
 CrossCorrelation(Signal, Signal, SignalCorrelation);
end;

procedure TCorrelation32.CrossCorrelation(const SignalA, SignalB,
  Correlation: PDAVSingleFixedArray);
begin
 FFft.PerformFFT(FSignalFreq, SignalA);
 FFft.PerformFFT(FCorrelationFreq, SignalB);

 ComplexMultiplyConjugated32(FCorrelationFreq, FSignalFreq, FFFTSizeHalf);

 FFft.PerformIFFT(FCorrelationFreq, Correlation);
end;


{ TCorrelation64 }

constructor TCorrelation64.Create;
begin
 inherited;

 FSignalFreq      := nil;
 FCorrelationFreq := nil;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat64.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat64.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}
 FFTOrderChanged;
end;

destructor TCorrelation64.Destroy;
begin
 Dispose(FSignalFreq);
 Dispose(FCorrelationFreq);

 FreeAndNil(FFft);
 inherited;
end;

procedure TCorrelation64.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64));
 ReallocMem(FCorrelationFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64), 0);
 FillChar(FCorrelationFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64), 0);
end;

{$IFDEF Use_IPPS}
function TCorrelation64.GetFft : TFftReal2ComplexIPPSFloat64;
begin
 Result := TFftReal2ComplexIPPSFloat64(FFft);
end;

{$ELSE}

function TCorrelation64.GetFft : TFftReal2ComplexNativeFloat64;
begin
 Result := TFftReal2ComplexNativeFloat64(FFft);
end;
{$ENDIF}

procedure TCorrelation64.AssignTo(Dest: TPersistent);
var
  Bin : Integer;
begin
 inherited;
 if Dest is TCorrelation32 then
  with TCorrelation32(Dest) do
   begin
    for Bin := 0 to FFFTSizeHalf do
     begin
      FSignalFreq^[Bin].Re := Self.FSignalFreq^[Bin].Re;
      FSignalFreq^[Bin].Im := Self.FSignalFreq^[Bin].Im;
      FCorrelationFreq^[Bin].Re := Self.FCorrelationFreq^[Bin].Re;
      FCorrelationFreq^[Bin].Im := Self.FCorrelationFreq^[Bin].Im;
     end;
   end else
 if Dest is TCorrelation64 then
  with TCorrelation64(Dest) do
   begin
    Move(Self.FSignalFreq^, FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64));
    Move(Self.FCorrelationFreq^, FCorrelationFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64));
   end;
end;

procedure TCorrelation64.AutoCorrelation(
  const SignalCorrelation: PDAVDoubleFixedArray);
begin
 AutoCorrelation(SignalCorrelation, SignalCorrelation);
end;

procedure TCorrelation64.AutoCorrelation(const Signal,
  Correlation: PDAVDoubleFixedArray);
begin
 FFft.PerformFFT(FCorrelationFreq, Signal);
 ComplexMultiplyConjugated64(FCorrelationFreq, FCorrelationFreq, FFFTSizeHalf);
 FFft.PerformIFFT(FCorrelationFreq, Correlation);
end;

procedure TCorrelation64.CrossCorrelation(const Signal,
  SignalCorrelation: PDAVDoubleFixedArray);
begin
 CrossCorrelation(Signal, Signal, SignalCorrelation);
end;

procedure TCorrelation64.CrossCorrelation(const SignalA, SignalB,
  Correlation: PDAVDoubleFixedArray);
begin
 FFft.PerformFFT(FSignalFreq, SignalA);
 FFft.PerformFFT(FCorrelationFreq, SignalB);

 ComplexMultiplyConjugated64(FCorrelationFreq, FSignalFreq, FFFTSizeHalf);

 FFft.PerformIFFT(FCorrelationFreq, Correlation);
end;

end.
