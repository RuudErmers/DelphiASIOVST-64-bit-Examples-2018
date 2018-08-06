unit DAV_DspConvolution;

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
{-$DEFINE CheckDataIntegrety}

uses
  Classes, DAV_Types, DAV_Complex, DAV_Classes, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS{$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

// TODO: check and implement all assignto functions!!!

type
  TCustomConvolution = class(TDspPersistent)
  private
    function GetFftOrder: Byte;
    procedure SetFftOrder(const Value: Byte);
  protected
    FFFT         : TFftReal2Complex;
    FFFTSize     : Integer;
    FFFTSizeHalf : Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateFftSizeVariables; virtual;
    procedure ImpulseResponseChanged; virtual; abstract;
    procedure FFTOrderChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property FFTOrder: Byte read GetFftOrder write SetFftOrder;
    property FFTSize: Integer read FFFTSize;
  end;

  TConvolution32 = class(TCustomConvolution, IDspProcessor32)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    function GetFft : TFftReal2ComplexCUDA32;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    procedure SetIRSizePadded(const Value: Integer);
    procedure SetIRSize(const Value: Integer);
  protected
    FImpulseResponse    : PDAVSingleFixedArray;
    FFilterFreqs        : array of PDAVComplexSingleFixedArray;
    FSignalFreq         : PDAVComplexSingleFixedArray;
    FConvolved          : PDAVComplexSingleFixedArray;
    FConvolvedTime      : PDAVSingleFixedArray;
    FInputBuffer        : PDAVSingleFixedArray;
    FOutputBuffer       : PDAVSingleFixedArray;

    FIRSize             : Integer;
    FBlockPosition      : Integer;

    FFreqRespBlockCount : Integer;
    FIRSizePadded       : Integer;

    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateFftSizeVariables; override;
    procedure ImpulseResponseChanged; override;
    procedure IRSizePaddedChanged; virtual;
    procedure FFTOrderChanged; override;
    procedure PerformConvolution(SignalIn, SignalOut: PDAVSingleFixedArray); virtual;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat32 read GetFft;
    {$ELSE} {$IFDEF Use_CUDA}
    property Fft : TFftReal2ComplexCUDA32 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat32  read GetFft;
    {$ENDIF}{$ENDIF}
    property IRSizePadded: Integer read FIRSizePadded write SetIRSizePadded;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output : PDAVSingleFixedArray; const SampleFrames: Integer); virtual;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; virtual;
    procedure LoadImpulseResponse(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVSingleDynArray); overload; virtual;
  published
    property FFTOrder;
    property FFTSize;
    property IRSize: Integer read FIRSize write SetIRSize;
  end;

  TConvolution64 = class(TCustomConvolution, IDspProcessor64)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat64;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat64;
    {$ENDIF}
    procedure SetIRSizePadded(const Value: Integer);
  protected
    FImpulseResponse    : PDAVDoubleFixedArray;
    FFilterFreqs        : array of PDAVComplexDoubleFixedArray;
    FSignalFreq         : PDAVComplexDoubleFixedArray;
    FConvolved          : PDAVComplexDoubleFixedArray;
    FConvolvedTime      : PDAVDoubleFixedArray;
    FInputBuffer        : PDAVDoubleFixedArray;
    FOutputBuffer       : PDAVDoubleFixedArray;

    FIRSize             : Integer;
    FBlockPosition      : Integer;

    FFreqRespBlockCount : Integer;
    FIRSizePadded       : Integer;

    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateFftSizeVariables; override;
    procedure ImpulseResponseChanged; override;
    procedure IRSizePaddedChanged; virtual;
    procedure FFTOrderChanged; override;
    procedure PerformConvolution(SignalIn, SignalOut: PDAVDoubleFixedArray); virtual;

    property IRSize: Integer read FIRSize;
    property IRSizePadded: Integer read FIRSizePadded write SetIRSizePadded;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat64 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat64 read GetFft;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output : PDAVDoubleFixedArray; const SampleFrames: Integer); virtual;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double; virtual;
    procedure LoadImpulseResponse(const Data: PDAVDoubleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVDoubleDynArray); overload; virtual;
  published
    property FFTOrder;
    property FFTSize;
  end;

  TCustomLowLatencyConvolution = class(TDspPersistent)
  end;

  TCustomLowLatencyConvolutionStage32 = class(TPersistent)
  private
    function GetCount: Integer;
  protected
    FFFTSize       : Integer;
    FOutputPos     : Integer;
    FLatency       : Integer;
    FMod, FModAnd  : Integer;

    FIRSpectrums   : array of PDAVComplexSingleFixedArray;
    FSignalFreq    : PDAVComplexSingleFixedArray;
    FConvolved     : PDAVComplexSingleFixedArray;
    FConvolvedTime : PDAVSingleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const IROrder: Byte; const StartPos, Latency, Count: Integer); virtual;
    destructor Destroy; override;
    procedure PerformConvolution(const SignalIn, SignalOut: PDAVSingleFixedArray); virtual; abstract;
  published
    property Count: Integer read GetCount;
    property Latency: Integer read FLatency;
  end;

  TLowLatencyConvolutionStage32 = class(TCustomLowLatencyConvolutionStage32)
  protected
    {$IFDEF Use_IPPS}
    FFft         : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft         : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft         : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    FFFTSize     : Integer;
    FFFTSizeHalf : Integer;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const IROrder: Byte; const StartPos, Latency, Count: Integer); override;
    destructor Destroy; override;

    procedure FFTOrderChanged; virtual;
    procedure PerformConvolution(const SignalIn, SignalOut: PDAVSingleFixedArray); override;
    procedure CalculateIRSpectrums(const IR: PDAVSingleFixedArray);
  published
    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat32 read FFft;
    {$ELSE} {$IFDEF Use_CUDA}
    property Fft : TFftReal2ComplexCUDA32 read FFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat32 read FFft;
    {$ENDIF}{$ENDIF}
  end;

  // ToDo: - Input and Output buffers should become circular buffers in this
  //         approach!

  TLowLatencyConvolution32 = class(TCustomLowLatencyConvolution)
  private
    function GetMaximumIRBlockSize: Integer;
    procedure SetMinimumIRBlockOrder(const Value: Byte);
    procedure SetMaximumIRBlockOrder(const Value: Byte);
    procedure SetIRSizePadded(const Value: Integer);

    function CalculatePaddedIRSize: Integer;
    procedure AllocatePaddedIRSizeDependentBuffers;
    procedure InputBufferSizeChanged;
    procedure OutputBufferSizeChanged;
    procedure CalculateLatency;
  protected
    FImpulseResponse     : PDAVSingleFixedArray;
    FConvStages          : array of TLowLatencyConvolutionStage32;
    FInputBuffer         : PDAVSingleFixedArray;
    FOutputBuffer        : PDAVSingleFixedArray;
    FInputBufferSize     : Integer;
    FOutputHistorySize   : Integer;
    FInputHistorySize    : Integer;
    FBlockPosition       : Integer;
    FIRSize              : Integer;
    FIRSizePadded        : Integer;
    FLatency             : Integer;
    FMinimumIRBlockOrder : Byte;
    FMaximumIRBlockOrder : Byte;

    procedure AssignTo(Dest: TPersistent); override;
    procedure BuildIRSpectrums; virtual;
    procedure MinimumIRBlockOrderChanged; virtual;
    procedure MaximumIRBlockOrderChanged; virtual;
    procedure PartitionizeIR; virtual;
    procedure PaddedIRSizeChanged; virtual;

    property MinimumIRBlockSize: Integer read FLatency;
    property MaximumIRBlockSize: Integer read GetMaximumIRBlockSize;
    property PaddedIRSize: Integer read FIRSizePadded write SetIRSizePadded;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure ProcessBlock(const Input, Output : PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure ProcessBlock(const Inplace: PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    function ProcessSample32(Input: Single): Single; virtual;
    procedure LoadImpulseResponse(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVSingleDynArray); overload; virtual;
  published
    property MinimumIRBlockOrder: Byte read FMinimumIRBlockOrder write SetMinimumIRBlockOrder;
    property MaximumIRBlockOrder: Byte read FMaximumIRBlockOrder write SetMaximumIRBlockOrder;
    property Latency: Integer read FLatency;
    property IRSize: Integer read FIRSize;
  end;

  TLowLatencyConvolutionStereo32 = class(TLowLatencyConvolution32)
  protected
    FInputBuffer2  : PDAVSingleFixedArray;
    FOutputBuffer2 : PDAVSingleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure PartitionizeIR; override;
    procedure PaddedIRSizeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock(const Left, Right: PDAVSingleFixedArray; const SampleFrames: Integer); reintroduce; virtual;
  end;

  TCustomLowLatencyConvolutionStage64 = class(TPersistent)
  private
    function GetCount: Integer;
  protected
    FOutputPos     : Integer;
    FLatency       : Integer;
    FMod, FModAnd  : Integer;

    FIRSpectrums   : array of PDAVComplexDoubleFixedArray;
    FSignalFreq    : PDAVComplexDoubleFixedArray;
    FConvolved     : PDAVComplexDoubleFixedArray;
    FConvolvedTime : PDAVDoubleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const IROrder: Byte; const StartPos, Latency, Count: Integer); virtual;
    destructor Destroy; override;

    procedure PerformConvolution(const SignalIn, SignalOut: PDAVDoubleFixedArray); virtual; abstract;
  published
    property Count: Integer read GetCount;
    property Latency: Integer read FLatency;
  end;

  TLowLatencyConvolutionStage64 = class(TCustomLowLatencyConvolutionStage64)
  protected
    {$IFDEF Use_IPPS}
    FFft           : TFftReal2ComplexIPPSFloat64;
    {$ELSE}
    FFft           : TFftReal2ComplexNativeFloat64;
    {$ENDIF}
    FFFTSize       : Integer;
    FFFTSizeHalf   : Integer;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const IROrder: Byte; const StartPos, Latency, Count: Integer);  override;
    destructor Destroy; override;
    procedure FFTOrderChanged; virtual;
    procedure PerformConvolution(const SignalIn, SignalOut: PDAVDoubleFixedArray); override;
    procedure CalculateIRSpectrums(const IR: PDAVDoubleFixedArray);
  published
    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat64 read FFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat64 read FFft;
    {$ENDIF}
  end;

  TLowLatencyConvolution64 = class(TCustomLowLatencyConvolution)
  private
    function GetMaximumIRBlockSize: Integer;
    procedure SetMinimumIRBlockOrder(const Value: Byte);
    procedure SetMaximumIRBlockOrder(const Value: Byte);
    procedure SetIRSizePadded(const Value: Integer);
    function CalculatePaddedIRSize: Integer;
    procedure AllocatePaddedIRSizeDependentBuffers;
    procedure InputBufferSizeChanged;
    procedure OutputBufferSizeChanged;
    procedure CalculateLatency;
  protected
    FImpulseResponse     : PDAVDoubleFixedArray;
    FConvStages          : array of TLowLatencyConvolutionStage64;
    FInputBuffer         : PDAVDoubleFixedArray;
    FOutputBuffer        : PDAVDoubleFixedArray;
    FInputBufferSize     : Integer;
    FOutputHistorySize   : Integer;
    FInputHistorySize    : Integer;
    FBlockPosition       : Integer;
    FIRSize              : Integer;
    FIRSizePadded        : Integer;
    FLatency             : Integer;
    FMinimumIRBlockOrder : Byte;
    FMaximumIRBlockOrder : Byte;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BuildIRSpectrums; virtual;
    procedure MinimumIRBlockOrderChanged; virtual;
    procedure MaximumIRBlockOrderChanged; virtual;
    procedure PartitionizeIR; virtual;
    procedure PaddedIRSizeChanged; virtual;
    property MinimumIRBlockSize: Integer read FLatency;
    property MaximumIRBlockSize: Integer read GetMaximumIRBlockSize;
    property PaddedIRSize: Integer read FIRSizePadded write SetIRSizePadded;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure ProcessBlock(const Input, Output : PDAVDoubleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure ProcessBlock(const Inplace : PDAVDoubleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: PDAVDoubleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVDoubleDynArray); overload; virtual;
  published
    property MinimumIRBlockOrder: Byte read FMinimumIRBlockOrder write SetMinimumIRBlockOrder;
    property MaximumIRBlockOrder: Byte read FMaximumIRBlockOrder write SetMaximumIRBlockOrder;
    property Latency: Integer read FLatency;
    property IRSize: Integer read FIRSize;
  end;

  TLowLatencyConvolutionStereo64 = class(TLowLatencyConvolution64)
  protected
    FInputBuffer2  : PDAVDoubleFixedArray;
    FOutputBuffer2 : PDAVDoubleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure PartitionizeIR; override;
    procedure PaddedIRSizeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Left, Right: PDAVDoubleFixedArray; const SampleFrames: Integer); reintroduce; virtual;
  end;

  TConvolution = TConvolution32;

implementation

uses
  SysUtils, Math, DAV_Math, DAV_BlockArithmetrics, DAV_BlockProcessing;

resourcestring
  RCStrIRBlockOrderError = 'Maximum IR block order must be larger or equal ' +
    'the minimum IR block order!';

{ TCustomConvolution }

constructor TCustomConvolution.Create;
begin
 inherited;
end;

destructor TCustomConvolution.Destroy;
begin
 FreeAndNil(FFft);
 inherited;
end;

procedure TCustomConvolution.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomConvolution then
  with TCustomConvolution(Dest) do
   begin
    FFFT.Assign(Self.FFFT);
    FFFTSize     := Self.FFFTSize;
    FFFTSizeHalf := Self.FFFTSizeHalf;
   end
  else inherited;
end;

procedure TCustomConvolution.CalculateFftSizeVariables;
begin
 FFFTSize     := FFft.FFTSize;
 FFFTSizeHalf := FFFTSize shr 1;
end;

function TCustomConvolution.GetFftOrder: Byte;
begin
 Result := FFft.Order;
end;

procedure TCustomConvolution.FFTOrderChanged;
begin
 CalculateFftSizeVariables;
 FFft.AutoScaleType := astDivideInvByN;
end;

procedure TCustomConvolution.SetFftOrder(const Value: Byte);
begin
 if FFft.Order <> Value then
  begin
   FFft.Order := Value;
   FFTOrderChanged;
  end;
end;

{ TConvolution32 }

constructor TConvolution32.Create;
begin
 FImpulseResponse    := nil;
 FSignalFreq         := nil;
 FConvolved          := nil;
 FConvolvedTime      := nil;
 FFreqRespBlockCount := 0;
 FIRSizePadded       := 0;
 FIRSize             := 0;

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

destructor TConvolution32.Destroy;
var
  i : Integer;
begin
 Dispose(FImpulseResponse);
 Dispose(FSignalFreq);
 Dispose(FConvolved);
 Dispose(FConvolvedTime);
 Dispose(FOutputBuffer);
 for i := 0 to Length(FFilterFreqs) - 1
  do Dispose(FFilterFreqs[i]);
 FreeAndNil(FFft);
 inherited;
end;

procedure TConvolution32.AssignTo(Dest: TPersistent);
begin
 if Dest is TConvolution32 then
  with TConvolution32(Dest) do
   begin
    inherited;
    FIRSize             := Self.FIRSize;
    FBlockPosition      := Self.FBlockPosition;
    FFreqRespBlockCount := Self.FFreqRespBlockCount;
    FIRSizePadded       := Self.FIRSizePadded;

(*
    FImpulseResponse    : PDAVSingleFixedArray;
    FFilterFreqs        : array of PDAVComplexSingleFixedArray;
    FSignalFreq         : PDAVComplexSingleFixedArray;
    FConvolved          : PDAVComplexSingleFixedArray;
    FConvolvedTime      : PDAVSingleFixedArray;
    FInputBuffer        : PDAVSingleFixedArray;
    FOutputBuffer       : PDAVSingleFixedArray;
*)

   end
 else inherited;
end;

procedure TConvolution32.CalculateFftSizeVariables;
begin
 inherited;
end;

procedure TConvolution32.ImpulseResponseChanged;
var
  TempIR     : PDAVSingleFixedArray;
  Blocks, sz : Integer;
begin
 // calculate number of blocks over the whole IR
 FFreqRespBlockCount := (FIRSize + FFFTSizeHalf - 1) div FFFTSizeHalf;

 // calculate the padded IR size (a multiply of FFT size / 2)
 IRSizePadded := FFreqRespBlockCount * FFFTSizeHalf;

 SetLength(FFilterFreqs, FFreqRespBlockCount);
 GetMem(TempIR, FFFTSize * SizeOf(Single));
 FillChar(TempIR^[0], FFFTSizeHalf * SizeOf(Single), 0);

 sz := IRSize;
 for Blocks := 0 to Length(FFilterFreqs) - 1 do
  begin
   assert(sz > 0);
   ReallocMem(FFilterFreqs[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplex32));

   if sz < FFFTSizeHalf then
    begin
     // build temporary IR part
     Move(FImpulseResponse^[Blocks * FFFTSizeHalf], TempIR^[FFFTSizeHalf], sz * SizeOf(Single));
     FillChar(TempIR^[FFFTSizeHalf + sz], (FFFTSizeHalf - sz) * SizeOf(Single), 0);
    end
   else Move(FImpulseResponse^[Blocks * FFFTSizeHalf], TempIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single));

   sz := sz - FFFTSizeHalf;

   // perform FFT
   FFft.PerformFFT(FFilterFreqs[Blocks], TempIR);
  end;
end;

procedure TConvolution32.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FInputBuffer, FFFTSize * SizeOf(Single));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Single));

 FillChar(FInputBuffer^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Single), 0);

 ImpulseResponseChanged;
end;

{$IFDEF Use_IPPS}
function TConvolution32.GetFft : TFftReal2ComplexIPPSFloat32;
begin
 Result := TFftReal2ComplexIPPSFloat32(FFft);
end;

{$ELSE} {$IFDEF Use_CUDA}

function TConvolution32.GetFft : TFftReal2ComplexCUDA32;
begin
 Result := TFftReal2ComplexCUDA32(FFft);
end;

{$ELSE}

function TConvolution32.GetFft : TFftReal2ComplexNativeFloat32;
begin
 Result := TFftReal2ComplexNativeFloat32(FFft);
end;
{$ENDIF}{$ENDIF}

procedure TConvolution32.IRSizePaddedChanged;
begin
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Single));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Single), 0);
end;

procedure TConvolution32.LoadImpulseResponse(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
  end
 else
  begin
   ReallocMem(FImpulseResponse, SampleFrames * SizeOf(Single));
   Move(Data^[0], FImpulseResponse^[0], SampleFrames * SizeOf(Single));
   FIRSize := SampleFrames;
   ImpulseResponseChanged;
  end;
end;

procedure TConvolution32.LoadImpulseResponse(const Data: TDAVSingleDynArray);
begin
 LoadImpulseResponse(@Data, Length(Data));
end;

procedure TConvolution32.PerformConvolution(SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Block  : Integer;
  Half   : Integer;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 Half := FFFTSizeHalf;

 for Block := 0 to FFreqRespBlockCount - 1 do
  begin
   // make a copy of the frequency respose
   Move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32));

   ComplexMultiplyBlock32(@FConvolved^[0], @FFilterFreqs[Block]^[0], Half);

   FFft.PerformIFFT(PDAVComplexSingleFixedArray(@FConvolved^[0]), FConvolvedTime);

   // copy and combine
   BlockAdditionInplace32(@SignalOut^[Block * Half], @FConvolvedTime^[0], Half);
  end;
end;

procedure TConvolution32.ProcessBlock(const Input,
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
    Break;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Single));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FFFTSizeHalf], FOutputBuffer^[0], (FIRSizePadded - FFFTSizeHalf) * SizeOf(Single));
    FillChar(FOutputBuffer^[(FIRSizePadded - FFFTSizeHalf)], FFFTSizeHalf * SizeOf(Single), 0);

    PerformConvolution(FInputBuffer, FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FFFTSizeHalf - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TConvolution32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
begin
 ProcessBlock(Data, Data, SampleCount);
end;

function TConvolution32.ProcessSample32(Input: Single): Single;
begin
 // copy to ring buffer only
 FInputBuffer^[FFFTSizeHalf + FBlockPosition] := Input;
 Result := FOutputBuffer^[FBlockPosition];

 // increase block position and Break
 Inc(FBlockPosition, 1);
 if FBlockPosition >= FFFTSizeHalf then
  begin
   // discard already used output buffer part and make space for new data
   Move(FOutputBuffer^[FFFTSizeHalf], FOutputBuffer^[0], (FIRSizePadded - FFFTSizeHalf) * SizeOf(Single));
   FillChar(FOutputBuffer^[(FIRSizePadded - FFFTSizeHalf)], FFFTSizeHalf * SizeOf(Single), 0);

   PerformConvolution(FInputBuffer, FOutputBuffer);

   // discard already used input buffer part to make space for new data
   Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Single));

   // increase current position and reset block position
   FBlockPosition := 0;
  end;
end;

procedure TConvolution32.SetIRSize(const Value: Integer);
begin
 if FIRSize < Value then
  begin
   ReallocMem(FImpulseResponse, Value * SizeOf(Single));
   FillChar(FImpulseResponse^[FIRSize], (Value - FIRSize) * SizeOf(Single), 0);
   FIRSize := Value;
   ImpulseResponseChanged;
  end
 else
  begin
   FIRSize := Value;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
  end;
end;

procedure TConvolution32.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   IRSizePaddedChanged;
  end;
end;


{ TConvolution64 }

constructor TConvolution64.Create;
begin
 FImpulseResponse    := nil;
 FSignalFreq         := nil;
 FConvolved          := nil;
 FConvolvedTime      := nil;
 FFreqRespBlockCount := 0;
 FIRSizePadded       := 0;
 FIRSize             := 0;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat64.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat64.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}
 FFTOrderChanged;
end;

destructor TConvolution64.Destroy;
var
  i : Integer;
begin
 Dispose(FImpulseResponse);
 Dispose(FSignalFreq);
 Dispose(FConvolved);
 Dispose(FConvolvedTime);
 Dispose(FOutputBuffer);
 for i := 0 to Length(FFilterFreqs) - 1
  do Dispose(FFilterFreqs[i]);
 FreeAndNil(FFft);
 inherited;
end;

procedure TConvolution64.AssignTo(Dest: TPersistent);
begin
 if Dest is TConvolution64 then
  with TConvolution64(Dest) do
   begin
    inherited;
    FIRSize             := Self.FIRSize;
    FBlockPosition      := Self.FBlockPosition;
    FFreqRespBlockCount := Self.FFreqRespBlockCount;
    FIRSizePadded       := Self.FIRSizePadded;

(*
    FImpulseResponse    : PDAVSingleFixedArray;
    FFilterFreqs        : array of PDAVComplexSingleFixedArray;
    FSignalFreq         : PDAVComplexSingleFixedArray;
    FConvolved          : PDAVComplexSingleFixedArray;
    FConvolvedTime      : PDAVSingleFixedArray;
    FInputBuffer        : PDAVSingleFixedArray;
    FOutputBuffer       : PDAVSingleFixedArray;
*)

   end
 else inherited;
end;

procedure TConvolution64.CalculateFftSizeVariables;
begin
 inherited;
end;

procedure TConvolution64.ImpulseResponseChanged;
var
  TempIR     : PDAVDoubleFixedArray;
  Blocks, sz : Integer;
begin
 // calculate number of blocks over the whole IR
 FFreqRespBlockCount := (FIRSize + FFFTSizeHalf - 1) div FFFTSizeHalf;

 // calculate the padded IR size (a multiply of FFT size / 2)
 IRSizePadded := FFreqRespBlockCount * FFFTSizeHalf;

 SetLength(FFilterFreqs, FFreqRespBlockCount);
 GetMem(TempIR, FFFTSize * SizeOf(Double));

 for Blocks := 0 to Length(FFilterFreqs) - 1 do
  begin
   ReallocMem(FFilterFreqs[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplex64));

   // calculate IR part size to be copied
   sz := IRSize - Blocks * FFFTSizeHalf;
   if sz > FFFTSizeHalf then sz := FFFTSizeHalf;

   // build temporary IR part
   Move(FImpulseResponse^[Blocks * FFFTSizeHalf], TempIR^[0], sz * SizeOf(Double));
   FillChar(TempIR^[sz], (FFFTSize - sz) * SizeOf(Double), 0);

   // perform FFT
   FFft.PerformFFT(FFilterFreqs[Blocks], TempIR);
  end;
end;

procedure TConvolution64.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FInputBuffer, FFFTSize * SizeOf(Double));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplex64));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Double));

 FillChar(FInputBuffer^[0], FFFTSize * SizeOf(Double), 0);
 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Double), 0);
end;

{$IFDEF Use_IPPS}
function TConvolution64.GetFft : TFftReal2ComplexIPPSFloat64;
begin
 Result := TFftReal2ComplexIPPSFloat64(FFft);
end;

{$ELSE}

function TConvolution64.GetFft : TFftReal2ComplexNativeFloat64;
begin
 Result := TFftReal2ComplexNativeFloat64(FFft);
end;
{$ENDIF}

procedure TConvolution64.IRSizePaddedChanged;
begin
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Double));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Double), 0);
end;

procedure TConvolution64.LoadImpulseResponse(const Data: PDAVDoubleFixedArray; const SampleFrames: Integer);
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   ImpulseResponseChanged;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   ImpulseResponseChanged;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Double));
  end
 else
  begin
   FIRSize := SampleFrames;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Double));
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   ImpulseResponseChanged;
  end;
end;

procedure TConvolution64.LoadImpulseResponse(const Data: TDAVDoubleDynArray);
begin
 LoadImpulseResponse(@Data, Length(Data));
end;

procedure TConvolution64.PerformConvolution(SignalIn,
  SignalOut: PDAVDoubleFixedArray);
var
  Block  : Integer;
  Half   : Integer;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 Half := FFFTSizeHalf;

 for Block := 0 to FFreqRespBlockCount - 1 do
  begin
   // make a copy of the frequency respose
   Move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64));

   ComplexMultiplyBlock64(@FConvolved^[0], @FFilterFreqs[Block]^[0], Half);

   FFft.PerformIFFT(PDAVComplexDoubleFixedArray(FConvolved), FConvolvedTime);

   // copy and combine
   BlockAdditionInplace64(@SignalOut^[Block * Half], @FConvolvedTime^[Half], Half);
  end;
end;

procedure TConvolution64.ProcessBlock(const Input,
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
    Move(FOutputBuffer^[FFFTSizeHalf], FOutputBuffer^[0], (FIRSizePadded - FFFTSizeHalf) * SizeOf(Double));
    FillChar(FOutputBuffer^[(FIRSizePadded - FFFTSizeHalf)], FFFTSizeHalf * SizeOf(Double), 0);

    PerformConvolution(FInputBuffer, FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Double));

    // increase current position and reset block position
    Inc(CurrentPosition, (FFFTSizeHalf - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TConvolution64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
begin
 ProcessBlock(Data, Data, SampleCount);
end;

function TConvolution64.ProcessSample64(Input: Double): Double;
begin
 // copy to ring buffer only
 FInputBuffer^[FFFTSizeHalf + FBlockPosition] := Input;
 Result := FOutputBuffer^[FBlockPosition];

 // increase block position and Break
 Inc(FBlockPosition, 1);
 if FBlockPosition >= FFFTSizeHalf then
  begin
   PerformConvolution(FInputBuffer, FOutputBuffer);

   // discard already used input buffer part to make space for new data
   Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Double));

   // increase current position and reset block position
   FBlockPosition := 0;
  end;
end;

procedure TConvolution64.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   IRSizePaddedChanged;
  end;
end;


{ TCustomLowLatencyConvolutionStage32 }

constructor TCustomLowLatencyConvolutionStage32.Create(const IROrder: Byte;
  const StartPos, Latency, Count: Integer);
begin
 FSignalFreq    := nil;
 FConvolvedTime := nil;
 FOutputPos     := StartPos;
 FLatency       := Latency;

 SetLength(FIRSpectrums, Count);
end;

destructor TCustomLowLatencyConvolutionStage32.Destroy;
var
  PartIndex : Integer;
begin
 FreeMem(FSignalFreq);
 FreeMem(FConvolved);
 FreeMem(FConvolvedTime);
 for PartIndex := 0 to Length(FIRSpectrums) - 1
  do FreeMem(FIRSpectrums[PartIndex]);

 inherited;
end;

procedure TCustomLowLatencyConvolutionStage32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomLowLatencyConvolutionStage32 then
  with TCustomLowLatencyConvolutionStage32(Dest) do
   begin
    inherited;
    FOutputPos     := Self.FOutputPos;
    FLatency       := Self.FLatency;
    FMod           := Self.FMod;
    FModAnd        := Self.FModAnd;

(*
    FIRSpectrums   := Self.FIRSpectrums;
    FSignalFreq    := Self.FSignalFreq;
    FConvolved     := Self.FConvolved;
    FConvolvedTime := Self.FConvolvedTime;
*)
   end else
 if Dest is TCustomLowLatencyConvolutionStage64 then
  with TCustomLowLatencyConvolutionStage64(Dest) do
   begin
    inherited;
    FOutputPos     := Self.FOutputPos;
    FLatency       := Self.FLatency;
    FMod           := Self.FMod;
    FModAnd        := Self.FModAnd;

(*
    FIRSpectrums   := Self.FIRSpectrums;
    FSignalFreq    := Self.FSignalFreq;
    FConvolved     := Self.FConvolved;
    FConvolvedTime := Self.FConvolvedTime;
*)
   end
 else inherited;
end;

function TCustomLowLatencyConvolutionStage32.GetCount: Integer;
begin
 Result := Length(FIRSpectrums);
end;


{ TLowLatencyConvolutionStage32 }

constructor TLowLatencyConvolutionStage32.Create(const IROrder: Byte; const StartPos, Latency, Count: Integer);
begin
 inherited Create(IROrder, StartPos, Latency, Count);

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(IROrder + 1);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(IROrder + 1);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(IROrder + 1);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}
 FFft.AutoScaleType := astDivideInvByN;
 FFTOrderChanged;
end;

destructor TLowLatencyConvolutionStage32.Destroy;
begin
 FreeAndNil(FFft);
 inherited;
end;

procedure TLowLatencyConvolutionStage32.FFTOrderChanged;
begin
 FFFTSize     := FFft.FFTSize;
 FFFTSizeHalf := FFFTSize shr 1;

 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Single));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Single), 0);
end;

procedure TLowLatencyConvolutionStage32.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TLowLatencyConvolutionStage32 then
  with TLowLatencyConvolutionStage32(Dest) do
   begin
    {$IFDEF Use_IPPS}
    FFft.Assign(Self.FFft);
    {$ELSE} {$IFDEF Use_CUDA}
    FFft.Assign(Self.FFft);
    {$ELSE}
    FFft.Assign(Self.FFft);
    {$ENDIF}{$ENDIF}
    FFFTSize       := Self.FFFTSize;
    FFFTSizeHalf   := Self.FFFTSizeHalf;
   end;
end;

procedure TLowLatencyConvolutionStage32.CalculateIRSpectrums(const IR: PDAVSingleFixedArray);
var
  TempIR   : PDAVSingleFixedArray;
  Blocks   : Integer;
begin
 Assert(FFFTSize = FFft.FFTSize);

 // get temporary buffer to store zero padded IR parts
 GetMem(TempIR, FFFTSize * SizeOf(Single));
 try
  // zeropad first half
  FillChar(TempIR^[0], FFFTSizeHalf * SizeOf(Single), 0);

  FModAnd  := (FFFTSizeHalf div FLatency) - 1;

  for Blocks := 0 to Length(FIRSpectrums) - 1 do
   begin
    ReallocMem(FIRSpectrums[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplex32));

    // build temporary IR part
    Move(IR^[FOutputPos + Blocks * FFFTSizeHalf], TempIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single));

    // perform FFT
    FFft.PerformFFT(FIRSpectrums[Blocks], TempIR);
   end;
 finally
  Dispose(TempIR);
 end;
end;

procedure TLowLatencyConvolutionStage32.PerformConvolution(const SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Block  : Integer;
  Half   : Integer;
  Dest   : PDAVComplexSingleFixedArray;
begin
 if FMod = 0 then
  begin
   Assert(Assigned(FSignalFreq));
   Assert(Assigned(SignalIn));
   Assert(Assigned(SignalOut));

   FFft.PerformFFT(FSignalFreq, @SignalIn[-FFFTSize]);
   Half := FFFTSizeHalf;

   if Length(FIRSpectrums) = 1 then Dest := FSignalFreq
    else
     begin
      Move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32));
      Dest := FConvolved;
     end;

   for Block := 0 to Length(FIRSpectrums) - 1 do
    begin
     // complex multiply with frequency response
     ComplexMultiplyBlock32(@FSignalFreq^[0], @FIRSpectrums[Block]^[0], Half, Dest);

     // transfer to frequency domain
     FFft.PerformIFFT(Dest, FConvolvedTime);

     // copy and combine
     BlockAdditionInplace32(@SignalOut^[FOutputPos + FLatency - FFFTSizeHalf + Block * Half], @FConvolvedTime^[0], Half);
    end;
  end;

 FMod := (FMod + 1) and FModAnd
end;


{ TLowLatencyConvolution32 }

constructor TLowLatencyConvolution32.Create;
begin
 inherited;
 FImpulseResponse      := nil;
 FIRSizePadded         := 0;
 FIRSize               := 0;
 FMinimumIRBlockOrder  := 7;
 FMaximumIRBlockOrder  := 16;
 FLatency              := 1 shl FMinimumIRBlockOrder;
 FInputBufferSize      := 2 shl FMaximumIRBlockOrder;
 InputBufferSizeChanged;
end;

destructor TLowLatencyConvolution32.Destroy;
var
  Stage : Integer;
begin
 Dispose(FImpulseResponse);
 Dispose(FOutputBuffer);
 Dispose(FInputBuffer);
 for Stage := 0 to Length(FConvStages) - 1
  do FreeAndNil(FConvStages[Stage]);
 inherited;
end;

procedure TLowLatencyConvolution32.InputBufferSizeChanged;
begin
 FInputHistorySize := FInputBufferSize - FLatency;
 ReallocMem(FInputBuffer, FInputBufferSize * SizeOf(Single));
 FillChar(FInputBuffer^[0], FInputBufferSize * SizeOf(Single), 0);
end;

procedure TLowLatencyConvolution32.OutputBufferSizeChanged;
begin
 FOutputHistorySize := (FIRSizePadded - FLatency);
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Single));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Single), 0);
end;

function TLowLatencyConvolution32.GetMaximumIRBlockSize: Integer;
begin
 Result := 1 shl FMaximumIRBlockOrder;
end;

procedure TLowLatencyConvolution32.SetMaximumIRBlockOrder(const Value: Byte);
begin
 if Value < FMinimumIRBlockOrder
  then raise Exception.Create(RCStrIRBlockOrderError);
 if FMaximumIRBlockOrder <> Value then
  begin
   FMaximumIRBlockOrder := Value;
   MaximumIRBlockOrderChanged;
  end;
end;

procedure TLowLatencyConvolution32.SetMinimumIRBlockOrder(const Value: Byte);
begin
 if FMinimumIRBlockOrder <> Value then
  begin
   FMinimumIRBlockOrder := Value;
   MinimumIRBlockOrderChanged;
  end;
end;

procedure TLowLatencyConvolution32.LoadImpulseResponse(
  const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
{$IFDEF CheckDataIntegrety}
var
  SampleIndex : Integer;
{$ENDIF}
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   {$IFDEF CheckDataIntegrety}
   for SampleIndex := 0 to FIRSize - 1 do
    if not IsNaN(Data^[SampleIndex])
     then FImpulseResponse^[SampleIndex] := Data^[SampleIndex];
   {$ELSE}
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   {$ENDIF}
   BuildIRSpectrums;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   {$IFDEF CheckDataIntegrety}
   for SampleIndex := 0 to FIRSize - 1 do
    if not IsNaN(Data^[SampleIndex])
     then FImpulseResponse^[SampleIndex] := Data^[SampleIndex];
   {$ELSE}
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   {$ENDIF}
   PaddedIRSize := CalculatePaddedIRSize;
   BuildIRSpectrums;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
  end
 else
  begin
   FIRSize := SampleFrames;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
   {$IFDEF CheckDataIntegrety}
   for SampleIndex := 0 to FIRSize - 1 do
    if not IsNaN(Data^[SampleIndex])
     then FImpulseResponse^[SampleIndex] := Data^[SampleIndex];
   {$ELSE}
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   {$ENDIF}
   PaddedIRSize := CalculatePaddedIRSize;
   BuildIRSpectrums;
  end;
end;

procedure TLowLatencyConvolution32.LoadImpulseResponse(
  const Data: TDAVSingleDynArray);
begin
 LoadImpulseResponse(@Data[0], Length(Data));
end;

procedure TLowLatencyConvolution32.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   PaddedIRSizeChanged;
  end;
end;

procedure TLowLatencyConvolution32.AllocatePaddedIRSizeDependentBuffers;
begin
 // zero pad filter
 ReallocMem(FImpulseResponse, FIRSizePadded * SizeOf(Single));
 if (FIRSizePadded - FIRSize) > 0
  then FillChar(FImpulseResponse^[FIRSize], (FIRSizePadded - FIRSize) * SizeOf(Single), 0);

 // reallocate output buffer
 OutputBufferSizeChanged;
end;

procedure TLowLatencyConvolution32.PaddedIRSizeChanged;
begin
 AllocatePaddedIRSizeDependentBuffers;

 // re partitionize IR
 PartitionizeIR;
end;

function TLowLatencyConvolution32.CalculatePaddedIRSize: Integer;
begin
 Result := MinimumIRBlockSize * ((IRSize + MinimumIRBlockSize - 1) div MinimumIRBlockSize);
end;

procedure TLowLatencyConvolution32.Clear;
begin
 FillChar(FInputBuffer^[0], FInputBufferSize * SizeOf(Single), 0);
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Single), 0);
end;

procedure TLowLatencyConvolution32.CalculateLatency;
begin
 FLatency           := 1 shl FMinimumIRBlockOrder;
 FInputHistorySize  := FInputBufferSize - FLatency;
 FOutputHistorySize := FIRSizePadded - FLatency;
end;

procedure TLowLatencyConvolution32.MinimumIRBlockOrderChanged;
begin
 CalculateLatency;

 if PaddedIRSize <> CalculatePaddedIRSize then
  begin
   PaddedIRSize := CalculatePaddedIRSize; // implicitely partitionize IR
   BuildIRSpectrums;
  end
 else
  begin
   PartitionizeIR;
   BuildIRSpectrums;
  end;
end;

procedure TLowLatencyConvolution32.MaximumIRBlockOrderChanged;
begin
 PartitionizeIR;
 BuildIRSpectrums;
end;

procedure TLowLatencyConvolution32.AssignTo(Dest: TPersistent);
begin
 if Dest is TLowLatencyConvolution32 then
  with TLowLatencyConvolution32(Dest) do
   begin
    inherited;
(*
    FImpulseResponse     : PDAVSingleFixedArray;
    FConvStages          : array of TLowLatencyConvolutionStage32;
    FInputBuffer         : PDAVSingleFixedArray;
    FOutputBuffer        : PDAVSingleFixedArray;
*)
    FInputBufferSize     := Self.FInputBufferSize;
    FOutputHistorySize   := Self.FOutputHistorySize;
    FInputHistorySize    := Self.FInputHistorySize;
    FBlockPosition       := Self.FBlockPosition;
    FIRSize              := Self.FIRSize;
    FIRSizePadded        := Self.FIRSizePadded;
    FLatency             := Self.FLatency;
    FMinimumIRBlockOrder := Self.FMinimumIRBlockOrder;
    FMaximumIRBlockOrder := Self.FMaximumIRBlockOrder;
   end
 else inherited;
end;

procedure TLowLatencyConvolution32.BuildIRSpectrums;
var
  Stage : Integer;
begin
 for Stage := 0 to Length(FConvStages) - 1
  do FConvStages[Stage].CalculateIRSpectrums(FImpulseResponse);
end;

function BitCountToBits(const BitCount: Byte): Integer;
begin
 Result := (2 shl BitCount) - 1;
end;

procedure TLowLatencyConvolution32.PartitionizeIR;
var
  c, cnt    : Integer;
  ResIRSize : Integer;
  StartPos  : Integer;
  MaxIROrd  : Byte;
begin
 // clear existing convolution stages
 for c := 0 to Length(FConvStages) - 1 do FreeAndNil(FConvStages[c]);
 if FIRSizePadded = 0 then Exit;

 Assert(FMaximumIRBlockOrder >= FMinimumIRBlockOrder);

 // calculate maximum FFT order (to create proper buffers later)
 MaxIROrd := TruncLog2(FIRSizePadded + MinimumIRBlockSize) - 1;

 // at least one block of each fft size is necessary
 ResIRSize := FIRSizePadded - (BitCountToBits(MaxIROrd) - BitCountToBits(FMinimumIRBlockOrder - 1));

 // check if highest block is only convolved once otherwise decrease
 if ((ResIRSize and (1 shl MaxIROrd)) shr MaxIROrd = 0) and (MaxIROrd > FMinimumIRBlockOrder)
  then Dec(MaxIROrd);

 // check if max. possible IR block order exceeds the bound and clip
 if MaxIROrd > FMaximumIRBlockOrder
  then MaxIROrd := FMaximumIRBlockOrder;

 // recalculate since MaxIROrd could have changed
 ResIRSize := FIRSizePadded - (BitCountToBits(MaxIROrd) - BitCountToBits(FMinimumIRBlockOrder - 1));

 // initialize convolution stage array
 SetLength(FConvStages, MaxIROrd - FMinimumIRBlockOrder + 1);

 StartPos := 0;
 for c := FMinimumIRBlockOrder to MaxIROrd - 1 do
  begin
   cnt := 1 + (ResIRSize and (1 shl c)) shr c;
   FConvStages[c - FMinimumIRBlockOrder] := TLowLatencyConvolutionStage32.Create(c, StartPos, FLatency, cnt);
   StartPos := StartPos + cnt * (1 shl c);
   ResIRSize := ResIRSize - (cnt - 1) * (1 shl c);
  end;

 // last stage
 cnt := 1 + ResIRSize div (1 shl MaxIROrd);
 FConvStages[Length(FConvStages) - 1] := TLowLatencyConvolutionStage32.Create(MaxIROrd, StartPos, FLatency, cnt);

 FInputBufferSize := 2 shl MaxIROrd;
 InputBufferSizeChanged;
end;

procedure TLowLatencyConvolution32.ProcessBlock(
  const Inplace: PDAVSingleFixedArray; const SampleFrames: Integer);
begin
 ProcessBlock(Inplace, Inplace, SampleFrames);
end;

procedure TLowLatencyConvolution32.ProcessBlock(const Input,
  Output: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Part            : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FLatency then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));

    // increase block position and Break
    Inc(FBlockPosition, SampleFrames - CurrentPosition);
    Break;
   end
  else
   begin
    Assert(FInputHistorySize + FBlockPosition + FLatency - FBlockPosition <= FInputBufferSize);
    Move(Input^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * SizeOf(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FLatency - FBlockPosition) * SizeOf(Single));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], FOutputHistorySize * SizeOf(Single));
    FillChar(FOutputBuffer^[FOutputHistorySize], FLatency * SizeOf(Single), 0);

    // actually perform partitioned convolution
    for Part := 0 to Length(FConvStages) - 1 do
     begin
      Assert(FInputBufferSize - FConvStages[Part].FFFTSize >= 0);
      FConvStages[Part].PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);
     end;

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FLatency], FInputBuffer[0], FInputHistorySize * SizeOf(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FLatency - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

function TLowLatencyConvolution32.ProcessSample32(Input: Single): Single;
var
  Part : Integer;
begin
 // copy to ring buffer only
 FInputBuffer^[FInputHistorySize + FBlockPosition] := Input;
 Result := FOutputBuffer^[FBlockPosition];

 // increase block position and Break
 Inc(FBlockPosition, 1);
 if FBlockPosition >= FLatency then
  begin
   // discard already used output buffer part and make space for new data
   Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], FOutputHistorySize * SizeOf(Single));
   FillChar(FOutputBuffer^[FOutputHistorySize], FLatency * SizeOf(Single), 0);

   // actually perform partitioned convolution
   for Part := 0 to Length(FConvStages) - 1
    do FConvStages[Part].PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);

   // discard already used input buffer part to make space for new data
   Move(FInputBuffer[FLatency], FInputBuffer[0], FInputHistorySize * SizeOf(Single));

   // reset block position
   FBlockPosition := 0;
  end;
end;


{ TLowLatencyConvolutionStereo32 }

constructor TLowLatencyConvolutionStereo32.Create;
begin
 inherited;
 FInputBuffer2 := nil;
 FOutputBuffer2 := nil;
end;

destructor TLowLatencyConvolutionStereo32.Destroy;
begin
 Dispose(FInputBuffer2);
 Dispose(FOutputBuffer2);
 inherited;
end;

procedure TLowLatencyConvolutionStereo32.AssignTo(Dest: TPersistent);
begin
 if Dest is TLowLatencyConvolutionStereo32 then
  with TLowLatencyConvolutionStereo32(Dest) do
   begin
    inherited;
(*
    FInputBuffer2  : PDAVSingleFixedArray;
    FOutputBuffer2 : PDAVSingleFixedArray;
*)
   end
 else inherited;
end;

procedure TLowLatencyConvolutionStereo32.PaddedIRSizeChanged;
begin
 inherited;
 ReallocMem(FOutputBuffer2, FIRSizePadded * SizeOf(Single));
 FillChar(FOutputBuffer2^[0], FIRSizePadded * SizeOf(Single), 0);
end;

procedure TLowLatencyConvolutionStereo32.PartitionizeIR;
begin
 inherited;
 ReallocMem(FInputBuffer2, FInputBufferSize * SizeOf(Single));
 FillChar(FInputBuffer2^, FInputBufferSize * SizeOf(Single), 0);
end;

procedure TLowLatencyConvolutionStereo32.ProcessBlock(const Left,
  Right: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Part            : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FLatency then
   begin
    // copy to ring buffer only
    Move(Left^[CurrentPosition], FInputBuffer2^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));
    Move(Right^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));
    Move(FOutputBuffer2^[FBlockPosition], Left^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));
    Move(FOutputBuffer^[FBlockPosition], Right^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));

    // increase block position and Break
    Inc(FBlockPosition, SampleFrames - CurrentPosition);
    Break;
   end
  else
   begin
    Move(Left^[CurrentPosition], FInputBuffer2^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * SizeOf(Single));
    Move(Right^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * SizeOf(Single));
    Move(FOutputBuffer2^[FBlockPosition], Left^[CurrentPosition], (FLatency - FBlockPosition) * SizeOf(Single));
    Move(FOutputBuffer^[FBlockPosition], Right^[CurrentPosition], (FLatency - FBlockPosition) * SizeOf(Single));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], FOutputHistorySize * SizeOf(Single));
    Move(FOutputBuffer2^[FLatency], FOutputBuffer2^[0], FOutputHistorySize * SizeOf(Single));
    FillChar(FOutputBuffer^[FOutputHistorySize], FLatency * SizeOf(Single), 0);
    FillChar(FOutputBuffer2^[FOutputHistorySize], FLatency * SizeOf(Single), 0);

    // actually perform partitioned convolution
    for Part := 0 to Length(FConvStages) - 1 do
     with FConvStages[Part] do
      begin
       PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);
       FMod := (FMod + FModAnd) and FModAnd;
       PerformConvolution(@FInputBuffer2[FInputBufferSize], FOutputBuffer2);
      end;

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FLatency], FInputBuffer[0], FInputHistorySize * SizeOf(Single));
    Move(FInputBuffer2[FLatency], FInputBuffer2[0], FInputHistorySize * SizeOf(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FLatency - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;


{ TCustomLowLatencyConvolutionStage64 }

constructor TCustomLowLatencyConvolutionStage64.Create(const IROrder: Byte;
  const StartPos, Latency, Count: Integer);
begin
 FSignalFreq    := nil;
 FConvolvedTime := nil;
 FOutputPos     := StartPos;
 FLatency       := Latency;

 SetLength(FIRSpectrums, Count);
end;

destructor TCustomLowLatencyConvolutionStage64.Destroy;
var
  PartIndex : Integer;
begin
 FreeMem(FSignalFreq);
 FreeMem(FConvolved);
 FreeMem(FConvolvedTime);
 for PartIndex := 0 to Length(FIRSpectrums) - 1
  do FreeMem(FIRSpectrums[PartIndex]);

 inherited;
end;

procedure TCustomLowLatencyConvolutionStage64.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomLowLatencyConvolutionStage64 then
  with TCustomLowLatencyConvolutionStage64(Dest) do
   begin
    inherited;
    FOutputPos     := Self.FOutputPos;
    FLatency       := Self.FLatency;
    FMod           := Self.FMod;
    FModAnd        := Self.FModAnd;

(*
    FIRSpectrums   := Self.FIRSpectrums;
    FSignalFreq    := Self.FSignalFreq;
    FConvolved     := Self.FConvolved;
    FConvolvedTime := Self.FConvolvedTime;
*)
   end else
 if Dest is TCustomLowLatencyConvolutionStage32 then
  with TCustomLowLatencyConvolutionStage32(Dest) do
   begin
    inherited;
    FOutputPos     := Self.FOutputPos;
    FLatency       := Self.FLatency;
    FMod           := Self.FMod;
    FModAnd        := Self.FModAnd;

(*
    FIRSpectrums   := Self.FIRSpectrums;
    FSignalFreq    := Self.FSignalFreq;
    FConvolved     := Self.FConvolved;
    FConvolvedTime := Self.FConvolvedTime;
*)
   end
 else inherited;
end;

function TCustomLowLatencyConvolutionStage64.GetCount: Integer;
begin
 Result := Length(FIRSpectrums);
end;


{ TLowLatencyConvolutionStage64 }

constructor TLowLatencyConvolutionStage64.Create(const IROrder: Byte; const StartPos, Latency, Count: Integer);
begin
 inherited Create(IROrder, StartPos, Latency, Count);

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat64.Create(IROrder + 1);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat64.Create(IROrder + 1);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}
 FFft.AutoScaleType := astDivideInvByN;
 FFTOrderChanged;
end;

destructor TLowLatencyConvolutionStage64.Destroy;
begin
 FreeAndNil(FFft);
 inherited;
end;

procedure TLowLatencyConvolutionStage64.FFTOrderChanged;
begin
 FFFTSize     := FFft.FFTSize;
 FFFTSizeHalf := FFFTSize shr 1;

 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplex64));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Double));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Double), 0);
end;

procedure TLowLatencyConvolutionStage64.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TLowLatencyConvolutionStage64 then
  with TLowLatencyConvolutionStage64(Dest) do
   begin
    {$IFDEF Use_IPPS}
    FFft.Assign(Self.FFft);
    {$ELSE} {$IFDEF Use_CUDA}
    FFft.Assign(Self.FFft);
    {$ELSE}
    FFft.Assign(Self.FFft);
    {$ENDIF}{$ENDIF}
    FFFTSize       := Self.FFFTSize;
    FFFTSizeHalf   := Self.FFFTSizeHalf;
   end;
end;

procedure TLowLatencyConvolutionStage64.CalculateIRSpectrums(const IR: PDAVDoubleFixedArray);
var
  TempIR   : PDAVDoubleFixedArray;
  Blocks   : Integer;
begin
 Assert(FFFTSize = FFft.FFTSize);

 // get temporary buffer to store zero padded IR parts
 GetMem(TempIR, FFFTSize * SizeOf(Double));
 try
  // zeropad first half
  FillChar(TempIR^[0], FFFTSizeHalf * SizeOf(Double), 0);

  FModAnd  := (FFFTSizeHalf div FLatency) - 1;

  for Blocks := 0 to Length(FIRSpectrums) - 1 do
   begin
    ReallocMem(FIRSpectrums[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplex64));

    // build temporary IR part
    Move(IR^[FOutputPos + Blocks * FFFTSizeHalf], TempIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Double));

    // perform FFT
    FFft.PerformFFT(FIRSpectrums[Blocks], TempIR);
   end;
 finally
  Dispose(TempIR);
 end;
end;

procedure TLowLatencyConvolutionStage64.PerformConvolution(const SignalIn,
  SignalOut: PDAVDoubleFixedArray);
var
  Block  : Integer;
  Half   : Integer;
  Dest   : PDAVComplexDoubleFixedArray;
begin
 if FMod = 0 then
  begin
   FFft.PerformFFT(FSignalFreq, @SignalIn[-FFFTSize]);
   Half := FFFTSizeHalf;

   if Length(FIRSpectrums) = 1 then Dest := FSignalFreq
    else
     begin
      Move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex64));
      Dest := FConvolved;
     end;

   for Block := 0 to Length(FIRSpectrums) - 1 do
    begin
     // complex multiply with frequency response
     ComplexMultiplyBlock64(@FSignalFreq^[0], @FIRSpectrums[Block]^[0], Half, Dest);

     // transfer to frequency domain
     FFft.PerformIFFT(Dest, FConvolvedTime);

     // copy and combine
     BlockAdditionInplace64(@SignalOut^[FOutputPos + FLatency - FFFTSizeHalf + Block * Half], @FConvolvedTime^[0], Half);
    end;
  end;

 FMod := (FMod + 1) and FModAnd
end;


{ TLowLatencyConvolution64 }

constructor TLowLatencyConvolution64.Create;
begin
 inherited;
 FImpulseResponse      := nil;
 FIRSizePadded         := 0;
 FIRSize               := 0;
 FMinimumIRBlockOrder  := 7;
 FMaximumIRBlockOrder  := 16;
 FLatency              := 1 shl FMinimumIRBlockOrder;
 FInputBufferSize      := 2 shl FMaximumIRBlockOrder;
 InputBufferSizeChanged;
end;

destructor TLowLatencyConvolution64.Destroy;
var
  Stage : Integer;
begin
 Dispose(FImpulseResponse);
 Dispose(FOutputBuffer);
 Dispose(FInputBuffer);
 for Stage := 0 to Length(FConvStages) - 1
  do FreeAndNil(FConvStages[Stage]);
 inherited;
end;

procedure TLowLatencyConvolution64.InputBufferSizeChanged;
begin
 FInputHistorySize     := FInputBufferSize - FLatency;
 ReallocMem(FInputBuffer, FInputBufferSize * SizeOf(Double));
 FillChar(FInputBuffer^[0], FInputBufferSize * SizeOf(Double), 0);
end;

procedure TLowLatencyConvolution64.OutputBufferSizeChanged;
begin
 FOutputHistorySize := (FIRSizePadded - FLatency);
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Double));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Double), 0);
end;

function TLowLatencyConvolution64.GetMaximumIRBlockSize: Integer;
begin
 Result := 1 shl FMaximumIRBlockOrder;
end;

procedure TLowLatencyConvolution64.SetMaximumIRBlockOrder(const Value: Byte);
begin
 if Value < FMinimumIRBlockOrder
  then raise Exception.Create(RCStrIRBlockOrderError);
 if FMaximumIRBlockOrder <> Value then
  begin
   FMaximumIRBlockOrder := Value;
   MaximumIRBlockOrderChanged;
  end;
end;

procedure TLowLatencyConvolution64.SetMinimumIRBlockOrder(const Value: Byte);
begin
 if FMinimumIRBlockOrder <> Value then
  begin
   FMinimumIRBlockOrder := Value;
   MinimumIRBlockOrderChanged;
  end;
end;

procedure TLowLatencyConvolution64.LoadImpulseResponse(
  const Data: PDAVDoubleFixedArray; const SampleFrames: Integer);
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   BuildIRSpectrums;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   PaddedIRSize := CalculatePaddedIRSize;
   BuildIRSpectrums;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Double));
  end
 else
  begin
   FIRSize := SampleFrames;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Double));
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   PaddedIRSize := CalculatePaddedIRSize;
   BuildIRSpectrums;
  end;
end;

procedure TLowLatencyConvolution64.LoadImpulseResponse(
  const Data: TDAVDoubleDynArray);
begin
 LoadImpulseResponse(@Data[0], Length(Data));
end;

procedure TLowLatencyConvolution64.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   PaddedIRSizeChanged;
  end;
end;

procedure TLowLatencyConvolution64.AllocatePaddedIRSizeDependentBuffers;
begin
 // zero pad filter
 ReallocMem(FImpulseResponse, FIRSizePadded * SizeOf(Double));
 FillChar(FImpulseResponse^[FIRSize], (FIRSizePadded - FIRSize) * SizeOf(Double), 0);

 // reallocate output buffer
 OutputBufferSizeChanged;
end;

procedure TLowLatencyConvolution64.PaddedIRSizeChanged;
begin
 AllocatePaddedIRSizeDependentBuffers;

 // re partitionize IR
 PartitionizeIR;
end;

function TLowLatencyConvolution64.CalculatePaddedIRSize: Integer;
begin
 Result := MinimumIRBlockSize * ((IRSize + MinimumIRBlockSize - 1) div MinimumIRBlockSize);
end;

procedure TLowLatencyConvolution64.Clear;
begin
 FillChar(FInputBuffer^[0], FInputBufferSize * SizeOf(Double), 0);
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Double), 0);
end;

procedure TLowLatencyConvolution64.CalculateLatency;
begin
 FLatency           := 1 shl FMinimumIRBlockOrder;
 FInputHistorySize  := FInputBufferSize - FLatency;
 FOutputHistorySize := FIRSizePadded - FLatency;
end;

procedure TLowLatencyConvolution64.MinimumIRBlockOrderChanged;
begin
 CalculateLatency;

 if PaddedIRSize <> CalculatePaddedIRSize then
  begin
   PaddedIRSize := CalculatePaddedIRSize; // implicitely partitionize IR
   BuildIRSpectrums;
  end
 else
  begin
   PartitionizeIR;
   BuildIRSpectrums;
  end;
end;

procedure TLowLatencyConvolution64.MaximumIRBlockOrderChanged;
begin
 PartitionizeIR;
 BuildIRSpectrums;
end;

procedure TLowLatencyConvolution64.AssignTo(Dest: TPersistent);
begin
 if Dest is TLowLatencyConvolution64 then
  with TLowLatencyConvolution64(Dest) do
   begin
    inherited;
   end
 else inherited;
end;

procedure TLowLatencyConvolution64.BuildIRSpectrums;
var
  Stage : Integer;
begin
 for Stage := 0 to Length(FConvStages) - 1
  do FConvStages[Stage].CalculateIRSpectrums(FImpulseResponse);
end;

procedure TLowLatencyConvolution64.PartitionizeIR;
var
  c, cnt    : Integer;
  ResIRSize : Integer;
  StartPos  : Integer;
  MaxIROrd  : Byte;
begin
 // clear existing convolution stages
 for c := 0 to Length(FConvStages) - 1 do FreeAndNil(FConvStages[c]);
 if FIRSizePadded = 0 then exit;

 assert(FMaximumIRBlockOrder >= FMinimumIRBlockOrder);

 // calculate maximum FFT order (to create proper buffers later)
 MaxIROrd := TruncLog2(FIRSizePadded + MinimumIRBlockSize) - 1;

 // at least one block of each fft size is necessary
 ResIRSize := FIRSizePadded - (BitCountToBits(MaxIROrd) - BitCountToBits(FMinimumIRBlockOrder - 1));

 // check if highest block is only convolved once otherwise decrease
 if ((ResIRSize and (1 shl MaxIROrd)) shr MaxIROrd = 0) and (MaxIROrd > FMinimumIRBlockOrder)
  then Dec(MaxIROrd);

 // check if max. possible IR block order exceeds the bound and clip
 if MaxIROrd > FMaximumIRBlockOrder
  then MaxIROrd := FMaximumIRBlockOrder;

 // recalculate since MaxIROrd could have changed
 ResIRSize := FIRSizePadded - (BitCountToBits(MaxIROrd) - BitCountToBits(FMinimumIRBlockOrder - 1));

 // initialize convolution stage array
 SetLength(FConvStages, MaxIROrd - FMinimumIRBlockOrder + 1);

 StartPos := 0;
 for c := FMinimumIRBlockOrder to MaxIROrd - 1 do
  begin
   cnt := 1 + (ResIRSize and (1 shl c)) shr c;
   FConvStages[c - FMinimumIRBlockOrder] := TLowLatencyConvolutionStage64.Create(c, StartPos, FLatency, cnt);
   StartPos := StartPos + cnt * (1 shl c);
   ResIRSize := ResIRSize - (cnt - 1) * (1 shl c);
  end;

 // last stage
 cnt := 1 + ResIRSize div (1 shl MaxIROrd);
 FConvStages[Length(FConvStages) - 1] := TLowLatencyConvolutionStage64.Create(MaxIROrd, StartPos, FLatency, cnt);

 FInputBufferSize := 2 shl MaxIROrd;
 InputBufferSizeChanged;
end;

procedure TLowLatencyConvolution64.ProcessBlock(
  const Inplace: PDAVDoubleFixedArray; const SampleFrames: Integer);
begin
 ProcessBlock(Inplace, Inplace, SampleFrames);
end;

procedure TLowLatencyConvolution64.ProcessBlock(const Input,
  Output: PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Part            : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FLatency then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));

    // increase block position and Break
    Inc(FBlockPosition, SampleFrames - CurrentPosition);
    Break;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * SizeOf(Double));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FLatency - FBlockPosition) * SizeOf(Double));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], FOutputHistorySize * SizeOf(Double));
    FillChar(FOutputBuffer^[FOutputHistorySize], FLatency * SizeOf(Double), 0);

    // actually perform partitioned convolution
    for Part := 0 to Length(FConvStages) - 1
     do FConvStages[Part].PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FLatency], FInputBuffer[0], FInputHistorySize * SizeOf(Double));

    // increase current position and reset block position
    Inc(CurrentPosition, (FLatency - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

{ TLowLatencyConvolutionStereo64 }

constructor TLowLatencyConvolutionStereo64.Create;
begin
 inherited;
 FInputBuffer2 := nil;
 FOutputBuffer2 := nil;
end;

destructor TLowLatencyConvolutionStereo64.Destroy;
begin
 Dispose(FInputBuffer2);
 Dispose(FOutputBuffer2);
 inherited;
end;

procedure TLowLatencyConvolutionStereo64.AssignTo(Dest: TPersistent);
begin
 if Dest is TLowLatencyConvolutionStereo64 then
  with TLowLatencyConvolutionStereo64(Dest) do
   begin
    inherited;
   end
 else inherited;
end;

procedure TLowLatencyConvolutionStereo64.PaddedIRSizeChanged;
begin
 inherited;
 ReallocMem(FOutputBuffer2, FIRSizePadded * SizeOf(Double));
 FillChar(FOutputBuffer2^[0], FIRSizePadded * SizeOf(Double), 0);
end;

procedure TLowLatencyConvolutionStereo64.PartitionizeIR;
begin
 inherited;
 ReallocMem(FInputBuffer2, FInputBufferSize * SizeOf(Double));
 FillChar(FInputBuffer2^, FInputBufferSize * SizeOf(Double), 0);
end;

procedure TLowLatencyConvolutionStereo64.ProcessBlock(const Left,
  Right: PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Part            : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FLatency then
   begin
    // copy to ring buffer only
    Move(Left^[CurrentPosition], FInputBuffer2^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));
    Move(Right^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));
    Move(FOutputBuffer2^[FBlockPosition], Left^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));
    Move(FOutputBuffer^[FBlockPosition], Right^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));

    // increase block position and Break
    Inc(FBlockPosition, SampleFrames - CurrentPosition);
    Break;
   end
  else
   begin
    Move(Left^[CurrentPosition], FInputBuffer2^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * SizeOf(Double));
    Move(Right^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * SizeOf(Double));
    Move(FOutputBuffer2^[FBlockPosition], Left^[CurrentPosition], (FLatency - FBlockPosition) * SizeOf(Double));
    Move(FOutputBuffer^[FBlockPosition], Right^[CurrentPosition], (FLatency - FBlockPosition) * SizeOf(Double));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], FOutputHistorySize * SizeOf(Double));
    Move(FOutputBuffer2^[FLatency], FOutputBuffer2^[0], FOutputHistorySize * SizeOf(Double));
    FillChar(FOutputBuffer^[FOutputHistorySize], FLatency * SizeOf(Double), 0);
    FillChar(FOutputBuffer2^[FOutputHistorySize], FLatency * SizeOf(Double), 0);

    // actually perform partitioned convolution
    for Part := 0 to Length(FConvStages) - 1 do
     with FConvStages[Part] do
      begin
       PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);
       FMod := (FMod + FModAnd) and FModAnd;
       PerformConvolution(@FInputBuffer2[FInputBufferSize], FOutputBuffer2);
      end;

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FLatency], FInputBuffer[0], FInputHistorySize * SizeOf(Double));
    Move(FInputBuffer2[FLatency], FInputBuffer2[0], FInputHistorySize * SizeOf(Double));

    // increase current position and reset block position
    Inc(CurrentPosition, (FLatency - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

initialization
  RegisterDspProcessor32(TConvolution32);
  RegisterDspProcessor64(TConvolution64);
  RegisterDspProcessor32(TLowLatencyConvolution32);
  RegisterDspProcessor64(TLowLatencyConvolution64);

end.
