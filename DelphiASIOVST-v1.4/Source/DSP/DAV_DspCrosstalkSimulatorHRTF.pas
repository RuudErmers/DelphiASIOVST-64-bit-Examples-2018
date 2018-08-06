unit DAV_DspCrosstalkSimulatorHRTF;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspCrosstalkSimulator,
  DAV_DspConvolution, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  DAV_DspFftReal2Complex, {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF}
  DAV_DspHrtf;

type
  TCustomHrtfCrosstalkSimulator = class(TCustomCrosstalkSimulator)
  private
    FHrtf : THrtfs;
  protected  
    procedure AssignTo(Dest: TPersistent); override;
    procedure HrtfChanged(Sender: TObject); virtual; abstract;
    procedure ReloadImpulseResponses; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Hrtf: THrtfs read FHrtf;
  end;

  TCustomSimpleHrtfCrosstalkSimulator = class(TCustomHrtfCrosstalkSimulator)
  private
    FConvolution : array [0..1] of TConvolution32;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure HrtfChanged(Sender: TObject); override;
    procedure ReloadImpulseResponses; override;
    procedure SampleRateChanged; override;
  public
    procedure ProcessSample(var Left, Right: Single); overload; override;
    procedure ProcessSample(var Left, Right: Double); overload; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

  TCustomCompleteHrtfCrosstalkSimulator = class(TCustomHrtfCrosstalkSimulator)
  private
    FConvolution : array [0..1, 0..1] of TConvolution32;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure HrtfChanged(Sender: TObject); override;
    procedure SampleRateChanged; override;
    procedure ReloadImpulseResponses; override;
  public
    procedure ProcessSample(var Left, Right: Single); overload; override;
    procedure ProcessSample(var Left, Right: Double); overload; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, DAV_Math, DAV_Complex;

{ TCustomHrtfCrosstalkSimulator }

constructor TCustomHrtfCrosstalkSimulator.Create;
begin
 inherited;
 FHrtf := THrtfs.Create;
 FHrtf.OnHrtfChanged := HrtfChanged;
end;

destructor TCustomHrtfCrosstalkSimulator.Destroy;
begin
 FreeAndNil(FHrtf);
 inherited;
end;

procedure TCustomHrtfCrosstalkSimulator.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomHrtfCrosstalkSimulator then
  with TCustomHrtfCrosstalkSimulator(Dest) do
   begin
    inherited;
    FHrtf.Assign(Self.FHrtf);
   end
 else inherited;
end;

procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
asm
    // DC
    FLD     [EAX].Single
    FMUL    [EDX].Single
    FSTP    [EAX].Single
    ADD     EAX, 4
    ADD     EDX, 4

    // Nyquist
    FLD     [EAX].Single
    FMUL    [EDX].Single
    FSTP    [EAX].Single
    ADD     EAX, 4
    ADD     EDX, 4

    DEC     ECX
@Start:
    FLD     [EAX    ].Single  // A.Re
    FLD     [EAX + 4].Single  // A.Im, A.Re
    FLD     [EDX    ].Single  // B.Re, A.Im, A.Re
    FLD     [EDX + 4].Single  // B.Im, B.Re, A.Im, A.Re
    FLD     ST(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
    FMUL    ST(0), ST(2)      // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
    FLD     ST(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
    FMUL    ST(0), ST(2)      // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
    FSUBP                     // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
    FSTP    [EAX    ].Single  // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
    FXCH    ST(2)             // A.Im, B.Re, B.Im, A.Re
    FMULP                     // A.Im * B.Re, B.Im, A.Re
    FXCH    ST(2)             // B.Im, A.Re, A.Im * B.Re
    FMULP                     // B.Im * A.Re, A.Im * B.Re
    FADDP                     // A.Im * B.Re + A.Re * B.Im
    FSTP    [EAX + 4].Single  // A.Im := A.Im * B.Re + A.Re * B.Im
    ADD     EAX, 8
    ADD     EDX, 8
    LOOP    @Start

    // Nyquist
    FLD   [EAX].Single
    FMUL  [EDX].Single
    FSTP  [EAX].Single
end;

{ TCustomSimpleHrtfCrosstalkSimulator }

constructor TCustomSimpleHrtfCrosstalkSimulator.Create;
begin
 inherited;
 FConvolution[0] := TConvolution32.Create;
 FConvolution[1] := TConvolution32.Create;
end;

destructor TCustomSimpleHrtfCrosstalkSimulator.Destroy;
begin
 FreeAndNil(FConvolution[0]);
 FreeAndNil(FConvolution[1]);
 inherited;
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSimpleHrtfCrosstalkSimulator then
  with TCustomSimpleHrtfCrosstalkSimulator(Dest) do
   begin
    inherited;
    FConvolution[0].Assign(Self.FConvolution[0]);
    FConvolution[1].Assign(Self.FConvolution[1]);
   end
 else inherited;
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.HrtfChanged(Sender: TObject);
begin
 ReloadImpulseResponses;
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.SamplerateChanged;
begin
 ReloadImpulseResponses;
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.ReloadImpulseResponses;
var
  IR  : array [0..1] of PDAVSingleFixedArray;
  Frq : array [0..1] of PDAVComplexSingleFixedArray;
  Sz  : Integer;
  Ord : Integer;
  {$IFDEF Use_IPPS}
  Fft : TFftReal2ComplexIPPSFloat32;
  {$ELSE} {$IFDEF Use_CUDA}
  Fft : TFftReal2ComplexCUDA32;
  {$ELSE}
  Fft : TFftReal2ComplexNativeFloat32;
  {$ENDIF}{$ENDIF}
begin
 Sz := FHrtf.MaximumHrirSize;
 GetMem(IR[0], Sz * SizeOf(Single));
 GetMem(IR[1], Sz * SizeOf(Single));
 try
  // one ear only
  FHrtf.InterpolateHrir(-60, 90, Sz, IR[0], IR[1]);

  // update FFT order
  Ord := CeilLog2(Sz);
  FConvolution[0].FFTOrder := Ord;
  FConvolution[1].FFTOrder := Ord;

  {$IFDEF Use_IPPS}
  Fft := TFftReal2ComplexIPPSFloat32.Create(Ord + 1);
  {$ELSE} {$IFDEF Use_CUDA}
  Fft := TFftReal2ComplexCUDA32.Create(Ord + 1);
  {$ELSE}
  Fft := TFftReal2ComplexNativeFloat32.Create(Ord + 1);
  Fft.DataOrder := doPackedComplex;
  {$ENDIF}{$ENDIF}

  // deconvolution
  GetMem(Frq[0], ((Sz div 2) + 1) * SizeOf(Single));
  GetMem(Frq[1], ((Sz div 2) + 1) * SizeOf(Single));
  try
   Fft.PerformFFT(Frq[0], IR[0]);
   Fft.PerformFFT(Frq[1], IR[1]);

   // perform deconvolution

  finally
   Dispose(Frq[0]);
   Dispose(Frq[1]);
  end;

  // load impulse responses
  FConvolution[0].LoadImpulseResponse(IR[0], sz);
  FConvolution[1].LoadImpulseResponse(IR[0], sz);
 finally
  Dispose(IR[0]);
  Dispose(IR[1]);
 end;
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.ProcessSample(var Left, Right: Single);
var
  Data : Array [0..3] of Single;
begin
 Data[0] := Left;
 Data[1] := Right;

 // convolve crosstalk
 FConvolution[0].ProcessBlock(@Data[1], @Data[2], 1);
 FConvolution[1].ProcessBlock(@Data[0], @Data[3], 1);

 // mix data
 Left  := Data[0] + Data[2];
 Right := Data[1] + Data[3];
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.ProcessSample(var Left, Right: Double);
var
  Data : Array [0..3] of Single;
begin
 Data[0] := Left;
 Data[1] := Right;

 // convolve crosstalk
 FConvolution[0].ProcessBlock(@Data[1], @Data[2], 1);
 FConvolution[1].ProcessBlock(@Data[0], @Data[3], 1);

 // mix data
 Left  := Data[0] + Data[2];
 Right := Data[1] + Data[3];
end;

{ TCustomCompleteHrtfCrosstalkSimulator }

constructor TCustomCompleteHrtfCrosstalkSimulator.Create;
begin
 inherited;
 FConvolution[0, 0] := TConvolution32.Create;
 FConvolution[1, 0] := TConvolution32.Create;
 FConvolution[0, 0] := TConvolution32.Create;
 FConvolution[1, 1] := TConvolution32.Create;
end;

destructor TCustomCompleteHrtfCrosstalkSimulator.Destroy;
begin
 FreeAndNil(FConvolution[0, 0]);
 FreeAndNil(FConvolution[1, 0]);
 FreeAndNil(FConvolution[0, 0]);
 FreeAndNil(FConvolution[1, 1]);
 inherited;
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomCompleteHrtfCrosstalkSimulator then
  with TCustomCompleteHrtfCrosstalkSimulator(Dest) do
   begin
    inherited;
    FConvolution[0, 0].Assign(Self.FConvolution[0, 0]);
    FConvolution[0, 1].Assign(Self.FConvolution[0, 1]);
    FConvolution[1, 0].Assign(Self.FConvolution[1, 0]);
    FConvolution[1, 1].Assign(Self.FConvolution[1, 1]);
   end
 else inherited;
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.HrtfChanged(Sender: TObject);
begin
 ReloadImpulseResponses;
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.SamplerateChanged;
begin
 ReloadImpulseResponses;
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.ReloadImpulseResponses;
var
  IR : array [0..1, 0..1] of PDAVSingleFixedArray;
  Sz : Integer;
begin
 Sz := FHrtf.MaximumHrirSize;
 GetMem(IR[0, 0], Sz * SizeOf(Single));
 GetMem(IR[0, 1], Sz * SizeOf(Single));
 GetMem(IR[1, 0], Sz * SizeOf(Single));
 GetMem(IR[1, 1], Sz * SizeOf(Single));
 try
  // update FFT order
  FConvolution[0, 0].FFTOrder := CeilLog2(Sz);
  FConvolution[0, 1].FFTOrder := CeilLog2(Sz);
  FConvolution[1, 0].FFTOrder := CeilLog2(Sz);
  FConvolution[1, 1].FFTOrder := CeilLog2(Sz);

  // left ear
  FHrtf.InterpolateHrir(-60, 90, Sz, IR[0, 0], IR[0, 1]);
  FConvolution[0, 0].LoadImpulseResponse(IR[0, 0], sz);
  FConvolution[0, 1].LoadImpulseResponse(IR[0, 1], sz);

  // right ear
  FHrtf.InterpolateHrir(+60, 90, Sz, IR[1, 0], IR[1, 1]);
  FConvolution[1, 0].LoadImpulseResponse(IR[1, 0], sz);
  FConvolution[1, 1].LoadImpulseResponse(IR[1, 1], sz);
 finally
  Dispose(IR[0, 0]);
  Dispose(IR[0, 1]);
  Dispose(IR[1, 0]);
  Dispose(IR[1, 1]);
 end;
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.ProcessSample(var Left,
  Right: Single);
var
  Data : Array [0..3] of Single;
begin
 Data[0] := Left;
 Data[1] := Right;

 // convolve crosstalk
 FConvolution[0, 1].ProcessBlock(@Data[1], @Data[2], 1);
 FConvolution[1, 1].ProcessBlock(@Data[0], @Data[3], 1);

 // convolve direct
 FConvolution[0, 0].ProcessBlock(@Data[0], @Data[0], 1);
 FConvolution[1, 0].ProcessBlock(@Data[1], @Data[1], 1);

 // mix data
 Left  := Data[0] + Data[2];
 Right := Data[1] + Data[3];
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.ProcessSample(var Left,
  Right: Double);
var
  Data : Array [0..3] of Single;
begin
 Data[0] := Left;
 Data[1] := Right;

 // convolve crosstalk
 FConvolution[0, 1].ProcessBlock(@Data[1], @Data[2], 1);
 FConvolution[1, 1].ProcessBlock(@Data[0], @Data[3], 1);

 // convolve direct
 FConvolution[0, 0].ProcessBlock(@Data[0], @Data[0], 1);
 FConvolution[1, 0].ProcessBlock(@Data[1], @Data[1], 1);

 // mix data
 Left  := Data[0] + Data[2];
 Right := Data[1] + Data[3];
end;

end.
