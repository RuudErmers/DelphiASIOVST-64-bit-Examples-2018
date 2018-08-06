unit DAV_DspFftReal2Complex;

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
//  The code is based on the FFTReal code by Laurent de Soras, which is       //
//  can be found at http://ldesoras.free.fr/prod.html#src_fftreal            //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$IFDEF Darwin}
  {$DEFINE PUREPASCAL} // for OSX use pure pascal code
{$ENDIF}

{$IFDEF CPUx86_64}
  {$DEFINE PUREPASCAL}
{$ENDIF}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Classes,
  DAV_Types, DAV_Complex;

type
  TFftAutoScaleType = (astDivideFwdByN = 1, astDivideInvByN = 2,
    astDivideBySqrtN = 4, astDivideNoDivByAny = 8);

  TFftDataOrder = (doPackedRealImaginary, doPackedComplex, doComplex);

  TFftReal2Complex = class(TPersistent)
  private
    procedure SetBinCount(const Value: Integer);
    procedure SetFFTOrder(const Value: Integer);
    procedure SetFFTSize(Value: Integer);
    procedure SetAutoScaleType(const Value: TFftAutoScaleType);
    procedure SetDataOrder(const Value: TFftDataOrder);
  protected
    FBinCount      : Integer;
    FFftSize       : Integer;
    FFFTSizeInv    : Double;
    FAutoScaleType : TFftAutoScaleType;
    FDataOrder     : TFftDataOrder;
    FOrder         : Integer;
    FOnSizeChanged : TNotifyEvent;
    procedure AutoScaleTypeChanged; virtual;
    procedure CalculateOrderDependentValues;
    procedure DataOrderChanged; virtual;
    procedure FFTOrderChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload; virtual;
    constructor Create(const Order: Byte); overload; virtual;

    procedure ConvertSingleToDouble(Singles: PSingle; Doubles: PDouble);
    procedure ConvertDoubleToSingle(Doubles: PDouble; Singles: PSingle);

    procedure PerformFFT(const FrequencyDomain, TimeDomain: Pointer); virtual; abstract;
    procedure PerformIFFT(const FrequencyDomain, TimeDomain: Pointer); virtual; abstract;

    property AutoScaleType: TFftAutoScaleType read FAutoScaleType write SetAutoScaleType;
    property BinCount: Integer read FBinCount write SetBinCount stored False;
    property DataOrder: TFftDataOrder read FDataOrder write SetDataOrder;
    property FFTSize: Integer read FFftSize write SetFFTSize stored False;
    property FFTSizeInverse: Double read FFFTSizeInv;
    property Order: Integer read FOrder write SetFFTOrder default 13;
    property OnSizeChanged: TNotifyEvent read FOnSizeChanged write FOnSizeChanged;
  end;

  TFFTLUTBitReversed = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    LUT: array of Integer;
    constructor Create(const BitCount: Integer);
    destructor Destroy; override;
    function GetPointer: PInteger;
  end;

  TFFTLUTListObject = class(TPersistent)
  private
    FBrLUT   : TFFTLUTBitReversed;
    FFftSize : Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const xFFTSize: Integer);
    destructor Destroy; override;

    property BRLUT: TFFTLUTBitReversed read FBrLUT write FBrLUT;
    property FFTSize: Integer read FFftSize write FFftSize;
  end;

  TFftReal2ComplexNative = class(TFftReal2Complex)
  private
    procedure CalculateScaleFactor;
  protected
    FBitRevLUT   : TFFTLUTBitReversed;
    FScaleFactor : Double;
    procedure SetFFTFunctionPointers; virtual; abstract;
    procedure CalculateTrigoLUT; virtual; abstract;
    procedure FFTOrderChanged; override;
    procedure AutoScaleTypeChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload; override;
    constructor Create(const Order: Byte); overload; override;
    property DataOrder default doPackedRealImaginary;
  end;

  TPerform32PackedReIm = procedure(const FrequencyDomain, TimeDomain: PDAVSingleFixedArray) of object;
  TPerform64PackedReIm = procedure(const FrequencyDomain, TimeDomain: PDAVDoubleFixedArray) of object;
  TPerform32PackedComplex = procedure(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray) of object;
  TPerform64PackedComplex = procedure(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray)of object;

  TFftReal2ComplexNativeFloat32 = class(TFftReal2ComplexNative)
  protected
    FBuffer                   : PDAVSingleFixedArray;
    FPerformFFTPackedReIm     : TPerform32PackedReIm;
    FPerformIFFTPackedReIm    : TPerform32PackedReIm;
    FPerformFFTPackedComplex  : TPerform32PackedComplex;
    FPerformIFFTPackedComplex : TPerform32PackedComplex;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateTrigoLUT; override;
    procedure PerformFFTZero32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTZero32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTOne32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTOne32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTTwo32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTTwo32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTThree32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTThree32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTFour32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTFour32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTOdd32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTOdd32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTEven32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTEven32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTZero32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTZero32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTOne32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTOne32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTTwo32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTTwo32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTThree32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTThree32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTFour32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTFour32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTOdd32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTOdd32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTEven32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTEven32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure SetFFTFunctionPointers; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PerformFFT(const FrequencyDomain, TimeDomain: Pointer); override;
    procedure PerformIFFT(const FrequencyDomain, TimeDomain: Pointer); override;
    procedure Rescale(const Data: PDAVSingleFixedArray);
    procedure RescaleSqrt(const Data: PDAVSingleFixedArray);
  published
    property Order;
    property OnSizeChanged;
    property PerformFFTPackedComplex: TPerform32PackedComplex read FPerformFFTPackedComplex;
    property PerformIFFTPackedComplex: TPerform32PackedComplex read FPerformIFFTPackedComplex;
    property PerformFFTPackedReIm: TPerform32PackedReIm read FPerformFFTPackedReIm;
    property PerformIFFTPackedReIm: TPerform32PackedReIm read FPerformIFFTPackedReIm;
  end;

  TFftReal2ComplexNativeFloat64 = class(TFftReal2ComplexNative)
  protected
    FBuffer                   : PDAVDoubleFixedArray;
    FPerformFFTPackedReIm     : TPerform64PackedReIm;
    FPerformIFFTPackedReIm    : TPerform64PackedReIm;
    FPerformFFTPackedComplex  : TPerform64PackedComplex;
    FPerformIFFTPackedComplex : TPerform64PackedComplex;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateTrigoLUT; override;
    procedure PerformFFTZero64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTZero64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTOne64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTOne64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTTwo64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTTwo64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTThree64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTThree64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTFour64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTFour64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTOdd64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTOdd64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTEven64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTEven64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTZero64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTZero64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTOne64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTOne64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTTwo64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTTwo64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTThree64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTThree64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTOdd64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTOdd64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTEven64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTEven64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure SetFFTFunctionPointers; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PerformFFT(const FrequencyDomain, TimeDomain : Pointer); override;
    procedure PerformIFFT(const FrequencyDomain, TimeDomain : Pointer); override;
    procedure Rescale(const Data: PDAVDoubleFixedArray);
    procedure RescaleSqrt(const Data: PDAVDoubleFixedArray);
  published
    property Order;
    property OnSizeChanged;
    property PerformFFTPackedComplex: TPerform64PackedComplex read FPerformFFTPackedComplex;
    property PerformIFFTPackedComplex: TPerform64PackedComplex read FPerformIFFTPackedComplex;
    property PerformFFTPackedReIm: TPerform64PackedReIm read FPerformFFTPackedReIm;
    property PerformIFFTPackedReIm: TPerform64PackedReIm read FPerformIFFTPackedReIm;
  end;


implementation

uses
  Math, SysUtils;

resourcestring
  RCStrNotSupported = 'not supported yet';

var
  CSQRT2Div2 : Double;
  LUTList    : TList;
  TrigoLUT   : PDAVDoubleFixedArray;
  TrigoLvl   : Integer;

{ TFftReal2Complex }

constructor TFftReal2Complex.Create;
begin
  Create(13);
end;

constructor TFftReal2Complex.Create(const Order: Byte);
begin
  inherited Create;
  Assert(Order <> 0);
  FOrder := Order;
  FAutoScaleType := astDivideNoDivByAny;
  CalculateOrderDependentValues;
end;

procedure TFftReal2Complex.ConvertSingleToDouble(Singles: PSingle;
  Doubles: PDouble);
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to FFftSize - 1
  do PDAVDoubleFixedArray(Doubles)^[SampleIndex] := PDAVSingleFixedArray(Singles)^[SampleIndex];
{$ELSE}
asm
    PUSH    EBX
    MOV     EBX, Doubles
    MOV     ECX, [EAX.FFftSize]

@LoopStart:
    FLD     [EDX + ECX * 4 - 4].Single
    FSTP    [EBX + ECX * 8 - 8].Double
    LOOP    @LoopStart
    POP     EBX
{$ENDIF}
end;

procedure TFftReal2Complex.ConvertDoubleToSingle(Doubles: PDouble;
  Singles: PSingle);
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to FFftSize - 1
  do PDAVSingleFixedArray(Singles)^[SampleIndex] := PDAVDoubleFixedArray(Doubles)^[SampleIndex];
{$ELSE}
asm
    PUSH    EBX
    MOV     EBX, Singles
    MOV     ECX, [EAX.FFftSize]

@LoopStart:
    FLD     [EDX + ECX * 8 - 8].Double
    FSTP    [EBX + ECX * 4 - 4].Single
    LOOP    @LoopStart
    POP     EBX
{$ENDIF}
end;

procedure TFftReal2Complex.SetFFTSize(Value: Integer);
begin
  if FFftSize <> Value then
   begin
    if Abs(Round(Value) - Value) > 1E-10 then
      raise Exception.Create('This FFT only works for a size of 2^n');
    Order := Round(Log2(Value));
   end;
end;

procedure TFftReal2Complex.SetAutoScaleType(const Value: TFftAutoScaleType);
begin
  if FAutoScaleType <> Value then
   begin
    FAutoScaleType := Value;
    AutoScaleTypeChanged;
   end;
end;

procedure TFftReal2Complex.AssignTo(Dest: TPersistent);
begin
 if Dest is TFftReal2Complex then
  with TFftReal2Complex(Dest) do
   begin
    FBinCount      := Self.FBinCount;
    FFftSize       := Self.FFftSize;
    FFFTSizeInv    := Self.FFFTSizeInv;
    FAutoScaleType := Self.FAutoScaleType;
    FDataOrder     := Self.FDataOrder;
    FOrder         := Self.FOrder;
    FOnSizeChanged := Self.FOnSizeChanged;
   end
  else inherited;
end;

procedure TFftReal2Complex.AutoScaleTypeChanged;
begin
 // Nothing in here yet!
end;

procedure TFftReal2Complex.SetBinCount(const Value: Integer);
begin
  if FBinCount <> Value then FFTSize := 2 * (Value - 1);
end;

procedure TFftReal2Complex.DataOrderChanged;
begin
 // Nothing in here yet!
end;

procedure TFftReal2Complex.SetDataOrder(const Value: TFftDataOrder);
begin
 if FDataOrder <> Value then
  begin
   FDataOrder := Value;
   DataOrderChanged;
  end;
end;

procedure TFftReal2Complex.SetFFTOrder(const Value: Integer);
begin
  if FOrder <> Value then
   begin
    FOrder := Value;
    FFTOrderChanged;
   end;
end;

procedure TFftReal2Complex.CalculateOrderDependentValues;
begin
  FFftSize := 1 shl FOrder;
  FBinCount := FFftSize shr 1 + 1;
  FFFTSizeInv := 1 / FFftSize;
end;

procedure TFftReal2Complex.FFTOrderChanged;
begin
  CalculateOrderDependentValues;
  if Assigned(FOnSizeChanged) then FOnSizeChanged(Self);
end;


{ TFFTLUTBitReversed }

constructor TFFTLUTBitReversed.Create(const BitCount: Integer);
var
  Lngth   : Integer;
  Cnt     : Integer;
  BrIndex : Integer;
  Bit     : Integer;
begin
  inherited Create;
  Lngth := 1 shl BitCount;
  SetLength(LUT, Lngth);

  BrIndex := 0;
  LUT[0] := 0;
  for Cnt := 1 to Lngth - 1 do
   begin
    Bit := Lngth shr 1;
    BrIndex := BrIndex xor Bit;
    while BrIndex and Bit = 0 do
     begin
      Bit := Bit shr 1;
      BrIndex := BrIndex xor Bit;
     end;
    LUT[Cnt] := BrIndex;
   end;
end;

destructor TFFTLUTBitReversed.Destroy;
begin
  SetLength(LUT, 0);
  inherited;
end;

procedure TFFTLUTBitReversed.AssignTo(Dest: TPersistent);
begin
 if Dest is TFFTLUTBitReversed
  then TFFTLUTBitReversed(Dest).LUT := Self.LUT
  else inherited;
end;

function TFFTLUTBitReversed.GetPointer: PInteger;
begin
  Result := @LUT[0];
end;


{ TFFTLUTListObject }

constructor TFFTLUTListObject.Create(const xFFTSize: Integer);

  function CalcExt(Value: Integer): Integer;
  {$IFDEF PUREPASCAL}
  begin
    Result := Round(Log2(Value));
  {$ELSE}
  asm
    {$IFDEF CPUx86_64}
    XOR     EAX, EAX
@Start:
    INC     EAX
    TEST    ECX, $2
    JNZ     @End
    SHR     ECX, 1

    JMP     @Start
@End:
    {$ELSE}
    XOR     ECX, ECX
@Start:
    INC     ECX
    TEST    EAX, $2
    JNZ     @End
    SHR     EAX, 1

    JMP     @Start
@End:
    MOV     Result, ECX
    {$ENDIF}
  {$ENDIF}
  end;

begin
  FFftSize := xFFTSize;
  if FFftSize > 1 then FBrLUT := TFFTLUTBitReversed.Create(CalcExt(FFftSize));
end;

destructor TFFTLUTListObject.Destroy;
begin
  FreeAndNil(FBrLUT);
  inherited;
end;

procedure TFFTLUTListObject.AssignTo(Dest: TPersistent);
begin
 if Dest is TFFTLUTListObject then
  with TFFTLUTListObject(Dest) do
   begin
    FBrLUT.Assign(Self.FBrLUT);
    FFftSize := Self.FFftSize;
   end
  else inherited;
end;


procedure InitLUTList;
var
  i: Integer;
begin
  LUTList := TList.Create;
  for i := 1 to 15 do
    LUTList.Add(TFFTLUTListObject.Create(1 shl i));
end;

procedure DestroyLUTList;
begin
  while LUTList.Count > 0 do
   begin
    TFFTLUTListObject(LUTList.Items[0]).Free;
    LUTList.Delete(0);
   end;
  FreeAndNil(LUTList);
  Dispose(TrigoLUT);
end;

{ TFftReal2ComplexNative }

constructor TFftReal2ComplexNative.Create;
begin
  inherited Create;
  FFTOrderChanged;
  FDataOrder := doPackedRealImaginary;
end;

constructor TFftReal2ComplexNative.Create(const Order: Byte);
begin
  inherited Create(Order);
  FFTOrderChanged;
end;

procedure TFftReal2ComplexNative.AssignTo(Dest: TPersistent);
begin
 if Dest is TFftReal2ComplexNative then
  with TFftReal2ComplexNative(Dest) do
   begin
    inherited;
    FBitRevLUT.Assign(Self.FBitRevLUT);
    FScaleFactor := Self.FScaleFactor;
   end
  else inherited;
end;

procedure TFftReal2ComplexNative.AutoScaleTypeChanged;
begin
  inherited;
  CalculateScaleFactor;
end;

procedure TFftReal2ComplexNative.CalculateScaleFactor;
begin
  case FAutoScaleType of
    astDivideFwdByN,
    astDivideInvByN : FScaleFactor := 1 / FFftSize;
    astDivideBySqrtN : FScaleFactor := 1 / Sqrt(FFftSize);
  else FScaleFactor := 1;
  end;
end;

procedure TFftReal2ComplexNative.FFTOrderChanged;
var
  i   : Integer;
  tmp : TFFTLUTListObject;
begin
  inherited;
  CalculateTrigoLUT;
  for i := 0 to LUTList.Count - 1 do
    if TFFTLUTListObject(LUTList.Items[i]).FFTSize = FFftSize then
     begin
      FBitRevLUT := TFFTLUTListObject(LUTList.Items[i]).BRLUT;
      Break;
     end;
  if i >= LUTList.Count then
   begin
    tmp := TFFTLUTListObject.Create(FFftSize);
    FBitRevLUT := tmp.BRLUT;
    LUTList.Add(tmp);
   end;
  SetFFTFunctionPointers;
  CalculateScaleFactor;
end;

procedure DoTrigoLUT(Bits: Integer);
var
  Level, i  : Integer;
  Len, Offs : Integer;
  Mul       : Extended;
begin
  if (Bits > TrigoLvl) then
   begin
    ReallocMem(TrigoLUT, ((1 shl (Bits - 1)) - 4) * SizeOf(Double));

    for Level := TrigoLvl to Bits - 1 do
     begin
      Len  := 1 shl (Level - 1);
      Offs := (Len - 4);
      Mul  := PI / (Len shl 1);
      for i := 0 to Len - 1
       do TrigoLUT[i + Offs] := cos(i * Mul);
     end;

    TrigoLvl := Bits;
   end;
end;


{ TFftReal2ComplexNativeFloat32 }

constructor TFftReal2ComplexNativeFloat32.Create;
begin
 FBuffer := nil;
 inherited;
end;

destructor TFftReal2ComplexNativeFloat32.Destroy;
begin
  FreeMem(FBuffer);
  inherited;
end;

procedure TFftReal2ComplexNativeFloat32.SetFFTFunctionPointers;
begin
  ReallocMem(FBuffer, FFTSize * SizeOf(Single));
  case FOrder of
    0 :
     begin
      FPerformFFTPackedReIm  := PerformFFTZero32;
      FPerformIFFTPackedReIm := PerformIFFTZero32;
      FPerformFFTPackedComplex  := PerformFFTZero32;
      FPerformIFFTPackedComplex := PerformIFFTZero32;
     end;
    1 :
     begin
      FPerformFFTPackedReIm  := PerformFFTOne32;
      FPerformIFFTPackedReIm := PerformIFFTOne32;
      FPerformFFTPackedComplex  := PerformFFTOne32;
      FPerformIFFTPackedComplex := PerformIFFTOne32;
     end;
    2 :
     begin
      FPerformFFTPackedReIm  := PerformFFTTwo32;
      FPerformIFFTPackedReIm := PerformIFFTTwo32;
      FPerformFFTPackedComplex  := PerformFFTTwo32;
      FPerformIFFTPackedComplex := PerformIFFTTwo32;
     end;
    3 :
     begin
      FPerformFFTPackedReIm  := PerformFFTThree32;
      FPerformIFFTPackedReIm := PerformIFFTThree32;
      FPerformFFTPackedComplex  := PerformFFTThree32;
      FPerformIFFTPackedComplex := PerformIFFTThree32;
     end;
    4 :
     begin
      FPerformFFTPackedReIm  := PerformFFTFour32;
      FPerformIFFTPackedReIm := PerformIFFTFour32;
      FPerformFFTPackedComplex  := PerformFFTFour32;
      FPerformIFFTPackedComplex := PerformIFFTFour32;
     end
  else
    if FOrder and 1 <> 0 then
     begin
      FPerformFFTPackedReIm  := PerformFFTOdd32;
      FPerformIFFTPackedReIm := PerformIFFTOdd32;
      FPerformFFTPackedComplex  := PerformFFTOdd32;
      FPerformIFFTPackedComplex := PerformIFFTOdd32;
     end
    else
     begin
      FPerformFFTPackedReIm  := PerformFFTEven32;
      FPerformIFFTPackedReIm := PerformIFFTEven32;
      FPerformFFTPackedComplex  := PerformFFTEven32;
      FPerformIFFTPackedComplex := PerformIFFTEven32;
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFT(const FrequencyDomain,
  TimeDomain: Pointer);
begin
 case DataOrder of
  doPackedRealImaginary : FPerformFFTPackedReIm(FrequencyDomain, TimeDomain);
  doPackedComplex : FPerformFFTPackedComplex(FrequencyDomain, TimeDomain);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFT(const FrequencyDomain,
  TimeDomain: Pointer);
begin
 case DataOrder of
  doPackedRealImaginary : FPerformIFFTPackedReIm(FrequencyDomain, TimeDomain);
  doPackedComplex : FPerformIFFTPackedComplex(FrequencyDomain, TimeDomain);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

procedure TFftReal2ComplexNativeFloat32.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TFftReal2ComplexNativeFloat32 then
  with TFftReal2ComplexNativeFloat32(Dest) do
   begin
    inherited;
    Assert(FFftSize = Self.FFftSize);
    Move(FBuffer^, Self.FBuffer^, FftSize * SizeOf(Single));
    SetFFTFunctionPointers;
   end else
 if Dest is TFftReal2ComplexNativeFloat64 then
  with TFftReal2ComplexNativeFloat64(Dest) do
   begin
    inherited;
    Assert(FFftSize = Self.FFftSize);
    for Sample := 0 to FftSize - 1
     do FBuffer^[Sample] := Self.FBuffer^[Sample];
    SetFFTFunctionPointers;
   end
  else inherited;
end;

procedure TFftReal2ComplexNativeFloat32.CalculateTrigoLUT;
begin
  DoTrigoLUT(FOrder);
end;

procedure TFftReal2ComplexNativeFloat32.Rescale(const Data: PDAVSingleFixedArray);
var
  i : Integer;
  s : Double;
begin
 s :=  1 / FFTSize;
 for i := 0 to FFTSize - 1 do Data^[i] := s * Data^[i];
end;

procedure TFftReal2ComplexNativeFloat32.RescaleSqrt(const Data: PDAVSingleFixedArray);
var
  i : Integer;
  s : Double;
begin
 s := Sqrt(1 / FFTSize);
 for i := 0 to FFTSize - 1 do Data^[i] := s * Data^[i];
end;

{ FFT Routines }

procedure TFftReal2ComplexNativeFloat32.PerformFFTZero32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
begin
  FreqDomain[0] := TimeDomain[0];
end;
{$ELSE}
asm
    FLD     [TimeDomain].Single
    FSTP    [FreqDomain].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTZero32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
begin
  FreqDomain^[0].Re := TimeDomain^[0];
end;
{$ELSE}
asm
    FLD     [TimeDomain].Single
    FSTP    [FreqDomain].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTOne32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2SingleArray absolute TimeDomain;
  FD : PDAV2SingleArray absolute FreqDomain;
begin
 FD[0] := TD[0] + TD[1];
 FD[1] := TD[0] - TD[1];
end;
{$ELSE}
asm
    FLD     [TimeDomain     ].Single
    FLD     ST(0)
    FADD    [TimeDomain + $4].Single
    FSTP    [FreqDomain     ].Single
    FSUB    [TimeDomain + $4].Single
    FSTP    [FreqDomain + $4].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTOne32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2SingleArray absolute TimeDomain;
  FD : PComplex32 absolute FreqDomain;
begin
 FD.Re := TD[0] + TD[1];
 FD.Im := TD[0] - TD[1];
end;
{$ELSE}
asm
    FLD     [TimeDomain     ].Single
    FLD     ST(0)
    FADD    [TimeDomain + $4].Single
    FSTP    [FreqDomain     ].Single
    FSUB    [TimeDomain + $4].Single
    FSTP    [FreqDomain + $4].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTTwo32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Single;
  TD  : PDAV4SingleArray absolute TimeDomain;
  FD  : PDAV4SingleArray absolute FreqDomain;
begin
  FD[1]  := TD[0] - TD[2];
  FD[3]  := TD[1] - TD[3];
  Tmp[0] := TD[0] + TD[2];
  Tmp[1] := TD[1] + TD[3];
  FD[0]  := Tmp[0] + Tmp[1];
  FD[2]  := Tmp[0] - Tmp[1];
end;
{$ELSE}
asm
    FLD     [TimeDomain     ].Single
    FSUB    [TimeDomain + $8].Single
    FSTP    [FreqDomain + $4].Single
    FLD     [TimeDomain + $4].Single
    FSUB    [TimeDomain + $C].Single
    FSTP    [FreqDomain + $C].Single
    FLD     [TimeDomain     ].Single
    FADD    [TimeDomain + $8].Single
    FLD     [TimeDomain + $4].Single
    FADD    [TimeDomain + $C].Single
    FLD     ST(0)
    FADD    ST(0), ST(2)
    FSTP    [FreqDomain     ].Single
    FSUBP
    FSTP    [FreqDomain + $8].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTTwo32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Single;
  TD  : PDAV4SingleArray absolute TimeDomain;
  FD  : PDAV2ComplexSingleArray absolute FreqDomain;
begin
  FD[1].Re  := TD[0] - TD[2];
  FD[1].Im  := TD[1] - TD[3];
  Tmp[0]    := TD[0] + TD[2];
  Tmp[1]    := TD[1] + TD[3];
  FD[0].Re  := Tmp[0] + Tmp[1];
  FD[0].Im  := Tmp[0] - Tmp[1];
end;
{$ELSE}
asm
    FLD     [TimeDomain     ].Single
    FSUB    [TimeDomain + $8].Single
    FSTP    [FreqDomain + $4].Single
    FLD     [TimeDomain + $4].Single
    FSUB    [TimeDomain + $C].Single
    FSTP    [FreqDomain + $C].Single
    FLD     [TimeDomain     ].Single
    FADD    [TimeDomain + $8].Single
    FLD     [TimeDomain + $4].Single
    FADD    [TimeDomain + $C].Single
    FLD     ST(0)
    FADD    ST(0),ST(2)
    FSTP    [FreqDomain     ].Single
    FSUBP
    FSTP    [FreqDomain + $8].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTThree32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  TD  : PDAV8SingleArray absolute TimeDomain;
  FD  : PDAV8SingleArray absolute FreqDomain;
  Bf  : PDAV8SingleArray;
begin
  Bf := PDAV8SingleArray(FBuffer);
  Bf[0] := TD[0] - TD[4];
  Bf[1] := TD[0] + TD[4];
  Bf[2] := TD[2] - TD[6];
  Bf[3] := TD[2] + TD[6];
  Bf[4] := Bf[1] + Bf[3];
  FD[2] := Bf[1] - Bf[3];
  Bf[5] := TD[1] - TD[5];
  Bf[6] := TD[3] - TD[7];
  Bf[1] := TD[1] + TD[5];
  Bf[3] := TD[3] + TD[7];
  Bf[7] := Bf[1] + Bf[3];
  FD[6] := Bf[1] - Bf[3];
  FD[0] := Bf[4] + Bf[7];
  FD[4] := Bf[4] - Bf[7];
  Bf[1] := (Bf[5] - Bf[6]) * CSQRT2Div2;
  Bf[3] := (Bf[5] + Bf[6]) * CSQRT2Div2;
  FD[1] := Bf[0] + Bf[1];
  FD[3] := Bf[0] - Bf[1];
  FD[5] := Bf[3] + Bf[2];
  FD[7] := Bf[3] - Bf[2];
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTThree32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  TD  : PDAV8SingleArray absolute TimeDomain;
  FD  : PDAV4ComplexSingleArray absolute FreqDomain;
  Bf  : PDAV8SingleArray;
begin
  Bf := PDAV8SingleArray(FBuffer);
  Bf[1]    := TD[0] - TD[4];
  Bf[3]    := TD[2] - TD[6];
  Bf[2]    := TD[0] + TD[4];
  Bf[6]    := TD[2] + TD[6];
  Bf[0]    := Bf[2] + Bf[6];
  FD[2].Re := Bf[2] - Bf[6];
  Bf[5]    := TD[1] - TD[5];
  Bf[7]    := TD[3] - TD[7];
  Bf[2]    := TD[1] + TD[5];
  Bf[6]    := TD[3] + TD[7];
  Bf[4]    := Bf[2] + Bf[6];
  FD[2].Im := Bf[2] - Bf[6];
  FD[0].Re := Bf[0] + Bf[4];
  FD[0].Im := Bf[0] - Bf[4];
  Bf[2]    := (Bf[5] - Bf[7]) * CSQRT2Div2;
  Bf[6]    := (Bf[5] + Bf[7]) * CSQRT2Div2;
  FD[1].Re := Bf[1] + Bf[2];
  FD[3].Re := Bf[1] - Bf[2];
  FD[1].Im := Bf[6] + Bf[3];
  FD[3].Im := Bf[6] - Bf[3];
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTFour32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  ci, i      : Integer;
  NbrCoef    : Integer;
  NbrCoefH   : Integer;
  BitPos     : array [0..1] of Integer;
  c, s, v    : Double;
  TempBuffer : array [0..2] of PDAVSingleFixedArray;

begin
  TempBuffer[0] := @FreqDomain[0];
  TempBuffer[1] := @FBuffer^[0];

  ci := fFFTSize;
  repeat
    Dec(ci, 4);

    BitPos[0] := FBitRevLUT.LUT[ci    ];
    BitPos[1] := FBitRevLUT.LUT[ci + 1];
    TempBuffer[0][ci + 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci + 2];
    BitPos[1] := FBitRevLUT.LUT[ci + 3];
    TempBuffer[0][ci + 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    TempBuffer[0][ci    ] := s + c;
    TempBuffer[0][ci + 2] := s - c;
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2];
    TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6];

    v := (TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + v;
    TempBuffer[1][ci + 3] := TempBuffer[0][ci + 1] - v;

    v := (TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 5] := v + TempBuffer[0][ci + 3];
    TempBuffer[1][ci + 7] := v - TempBuffer[0][ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := (TempBuffer[1][0] + TempBuffer[1][NbrCoef]) * FScaleFactor;
    FreqDomain[0].Im := (TempBuffer[1][0] - TempBuffer[1][NbrCoef]) * FScaleFactor;

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH] * FScaleFactor;

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := (TempBuffer[1][i] + v) * FScaleFactor;
      FreqDomain[NbrCoef - i].Re := (TempBuffer[1][i] - v) * FScaleFactor;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := (v + TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
      FreqDomain[NbrCoef - i].Im := (v - TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
     end;
   end
  else 
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    FreqDomain[0].Im := TempBuffer[1][0] - TempBuffer[1][NbrCoef];

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH];
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := TempBuffer[1][i] + v;
      FreqDomain[NbrCoef - i].Re := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := v + TempBuffer[1][NbrCoefH + i];
      FreqDomain[NbrCoef - i].Im := v - TempBuffer[1][NbrCoefH + i];
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTFour32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  ci, i      : Integer;
  NbrCoef    : Integer;
  NbrCoefH   : Integer;
  BitPos     : array [0..1] of Integer;
  c, s, v    : Double;
  TempBuffer : array [0..2] of PDAVSingleFixedArray;

begin
  // first and second pass at once
  ci := fFFTSize;
  repeat
    BitPos[0] := FBitRevLUT.LUT[ci - 4];
    BitPos[1] := FBitRevLUT.LUT[ci - 3];
    FreqDomain[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 2];
    BitPos[1] := FBitRevLUT.LUT[ci - 1];
    FreqDomain[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FreqDomain[ci - 4] := s + c;
    FreqDomain[ci - 2] := s - c;

    Dec(ci, 4);
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    FBuffer^[ci    ] := FreqDomain[ci] + FreqDomain[ci + 4];
    FBuffer^[ci + 4] := FreqDomain[ci] - FreqDomain[ci + 4];
    FBuffer^[ci + 2] := FreqDomain[ci + 2];
    FBuffer^[ci + 6] := FreqDomain[ci + 6];

    v := (FreqDomain[ci + 5] - FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 1] := FreqDomain[ci + 1] + v;
    FBuffer^[ci + 3] := FreqDomain[ci + 1] - v;

    v := (FreqDomain[ci + 5] + FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 5] := v + FreqDomain[ci + 3];
    FBuffer^[ci + 7] := v - FreqDomain[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  TempBuffer[0] := @FreqDomain[0];
  TempBuffer[1] := @FBuffer^[0];

  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;
  ci       := 2 * NbrCoef;

  // Extreme coefficients are always real
  TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
  TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
  TempBuffer[0][          NbrCoefH] := TempBuffer[1][          NbrCoefH];
  TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

  // Others are conjugate complex numbers
  for i := 1 to NbrCoefH - 1 do
   begin
    c := TrigoLUT[NbrCoefH - 4 + i];
    s := TrigoLUT[NbrCoef  - 4 - i];

    v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
    TempBuffer[0][i] := TempBuffer[1][i] + v;
    TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

    v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
    TempBuffer[0][NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
    TempBuffer[0][ci - i] := v - TempBuffer[1][NbrCoefH + i];
   end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTOdd32(const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  BitPos      : array [0..1] of Integer;
  c, s, v     : Double;
  TempBuffer  : array [0..2] of PDAVSingleFixedArray;
begin
  // first and second pass at once
  ci := fFFTSize;
  repeat
    BitPos[0] := FBitRevLUT.LUT[ci - 4];
    BitPos[1] := FBitRevLUT.LUT[ci - 3];
    FBuffer^[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 2];
    BitPos[1] := FBitRevLUT.LUT[ci - 1];
    FBuffer^[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FBuffer^[ci - 4] := s + c;
    FBuffer^[ci - 2] := s - c;

    BitPos[0] := FBitRevLUT.LUT[ci - 8];
    BitPos[1] := FBitRevLUT.LUT[ci - 7];
    FBuffer^[ci - 7] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 6];
    BitPos[1] := FBitRevLUT.LUT[ci - 5];
    FBuffer^[ci - 5] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FBuffer^[ci - 8] := s + c;
    FBuffer^[ci - 6] := s - c;

    Dec(ci, 8);
  until (ci <= 0);

  // third pass at once
  ci := 0;
  repeat
    FreqDomain[ci] := FBuffer^[ci] + FBuffer^[ci + 4];
    FreqDomain[ci + 4] := FBuffer^[ci] - FBuffer^[ci + 4];
    FreqDomain[ci + 2] := FBuffer^[ci + 2];
    FreqDomain[ci + 6] := FBuffer^[ci + 6];

    v := (FBuffer^[ci + 5] - FBuffer^[ci + 7]) * CSQRT2Div2;
    FreqDomain[ci + 1] := FBuffer^[ci + 1] + v;
    FreqDomain[ci + 3] := FBuffer^[ci + 1] - v;
    v := (FBuffer^[ci + 5] + FBuffer^[ci + 7]) * CSQRT2Div2;
    FreqDomain[ci + 5] := v + FBuffer^[ci + 3];
    FreqDomain[ci + 7] := v - FBuffer^[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  TempBuffer[0] := @FBuffer^[0];
  TempBuffer[1] := @FreqDomain[0];
  for Pass := 3 to FOrder - 2 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;
    repeat
      // extreme coefficients are always real
      TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
      TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
      TempBuffer[0][          NbrCoefH] := TempBuffer[1][          NbrCoefH];
      TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

      // others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TempBuffer[0][          i] := TempBuffer[1][i] + v;
        TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

        v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TempBuffer[0][NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
        TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
       end;

      Inc(ci,            NbrCoef * 2);
      Inc(TempBuffer[0], NbrCoef * 2);
      Inc(TempBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TempBuffer[0], fFFTSize);
    Dec(TempBuffer[1], fFFTSize);

    // prepare to the next pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  // next pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // extreme coefficients are always real
    FreqDomain[0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef] * FScaleFactor;
    FreqDomain[NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef] * FScaleFactor;
    FreqDomain[          NbrCoefH] := TempBuffer[1][          NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH] * FScaleFactor;

    // others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[          i] := (TempBuffer[1][i] + v) * FScaleFactor;
      FreqDomain[NbrCoef - i] := (TempBuffer[1][i] - v) * FScaleFactor;

      v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
      FreqDomain[NbrCoef + i]     := (v + TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
      FreqDomain[2 * NbrCoef - i] := (v - TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
     end;
   end
  else
   begin
    // extreme coefficients are always real
    FreqDomain[0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    FreqDomain[NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
    FreqDomain[          NbrCoefH] := TempBuffer[1][          NbrCoefH];
    FreqDomain[NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

    // others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[          i] := TempBuffer[1][i] + v;
      FreqDomain[NbrCoef - i] := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
      FreqDomain[NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
      FreqDomain[2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTOdd32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  BitPos      : array [0..1] of Integer;
  c, s, v     : Double;
  TempBuffer  : array [0..2] of PDAVSingleFixedArray;
begin
  // first and second pass at once
  ci := FFFTSize;

  repeat
    BitPos[0] := FBitRevLUT.LUT[ci - 4];
    BitPos[1] := FBitRevLUT.LUT[ci - 3];

    FBuffer^[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 2];
    BitPos[1] := FBitRevLUT.LUT[ci - 1];
    FBuffer^[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FBuffer^[ci - 4] := s + c;
    FBuffer^[ci - 2] := s - c;

    BitPos[0] := FBitRevLUT.LUT[ci - 8];
    BitPos[1] := FBitRevLUT.LUT[ci - 7];
    FBuffer^[ci - 7] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 6];
    BitPos[1] := FBitRevLUT.LUT[ci - 5];
    FBuffer^[ci - 5] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FBuffer^[ci - 8] := s + c;
    FBuffer^[ci - 6] := s - c;

    Dec(ci, 8);
  until (ci <= 0);

  TempBuffer[0] := @FBuffer^[0];
  TempBuffer[1] := @FreqDomain[0];

  // third pass at once
  ci := 0;
  repeat
    TempBuffer[1][ci    ] := FBuffer^[ci] + FBuffer^[ci + 4];
    TempBuffer[1][ci + 4] := FBuffer^[ci] - FBuffer^[ci + 4];
    TempBuffer[1][ci + 2] := FBuffer^[ci + 2];
    TempBuffer[1][ci + 6] := FBuffer^[ci + 6];

    v := (FBuffer^[ci + 5] - FBuffer^[ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 1] := FBuffer^[ci + 1] + v;
    TempBuffer[1][ci + 3] := FBuffer^[ci + 1] - v;
    v := (FBuffer^[ci + 5] + FBuffer^[ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 5] := v + FBuffer^[ci + 3];
    TempBuffer[1][ci + 7] := v - FBuffer^[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  for Pass := 3 to FOrder - 2 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;
    repeat
      // extreme coefficients are always real
      TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
      TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
      TempBuffer[0][          NbrCoefH] := TempBuffer[1][NbrCoefH];
      TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

      // others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TempBuffer[0][          i] := TempBuffer[1][i] + v;
        TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

        v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TempBuffer[0][NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
        TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
       end;

      Inc(ci,            NbrCoef * 2);
      Inc(TempBuffer[0], NbrCoef * 2);
      Inc(TempBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TempBuffer[0], fFFTSize);
    Dec(TempBuffer[1], fFFTSize);

    // prepare to the next pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  // last pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := (TempBuffer[1][0] + TempBuffer[1][NbrCoef]) * FScaleFactor;
    FreqDomain[0].Im := (TempBuffer[1][0] - TempBuffer[1][NbrCoef]) * FScaleFactor;

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH] * FScaleFactor;

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := (TempBuffer[1][i] + v) * FScaleFactor;
      FreqDomain[NbrCoef - i].Re := (TempBuffer[1][i] - v) * FScaleFactor;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := (v + TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
      FreqDomain[NbrCoef - i].Im := (v - TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
     end;
   end
  else
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    FreqDomain[0].Im := TempBuffer[1][0] - TempBuffer[1][NbrCoef];

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH];
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := TempBuffer[1][i] + v;
      FreqDomain[NbrCoef - i].Re := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := v + TempBuffer[1][NbrCoefH + i];
      FreqDomain[NbrCoef - i].Im := v - TempBuffer[1][NbrCoefH + i];
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTEven32(const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  BitPos      : array [0..1] of Integer;
  c, s, v     : Double;
  TempBuffer  : array [0..2] of PDAVSingleFixedArray;
begin
  // first and second pass at once
  ci := fFFTSize;
  repeat
    BitPos[0] := FBitRevLUT.LUT[ci - 4];
    BitPos[1] := FBitRevLUT.LUT[ci - 3];
    FreqDomain[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 2];
    BitPos[1] := FBitRevLUT.LUT[ci - 1];
    FreqDomain[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FreqDomain[ci - 4] := s + c;
    FreqDomain[ci - 2] := s - c;

    Dec(ci, 4);
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    FBuffer^[ci    ] := FreqDomain[ci] + FreqDomain[ci + 4];
    FBuffer^[ci + 4] := FreqDomain[ci] - FreqDomain[ci + 4];
    FBuffer^[ci + 2] := FreqDomain[ci + 2];
    FBuffer^[ci + 6] := FreqDomain[ci + 6];

    v := (FreqDomain[ci + 5] - FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 1] := FreqDomain[ci + 1] + v;
    FBuffer^[ci + 3] := FreqDomain[ci + 1] - v;

    v := (FreqDomain[ci + 5] + FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 5] := v + FreqDomain[ci + 3];
    FBuffer^[ci + 7] := v - FreqDomain[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  TempBuffer[0] := @FreqDomain[0];
  TempBuffer[1] := @FBuffer^[0];

  for Pass := 3 to FOrder - 2 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;

    repeat
      // Extreme coefficients are always real
      TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
      TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
      TempBuffer[0][          NbrCoefH] := TempBuffer[1][          NbrCoefH];
      TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

      // Others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TempBuffer[0][          i] := TempBuffer[1][i] + v;
        TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

        v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TempBuffer[0][    NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
        TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
       end;

      Inc(ci, NbrCoef * 2);
      Inc(TempBuffer[0], NbrCoef * 2);
      Inc(TempBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TempBuffer[0], fFFTSize);
    Dec(TempBuffer[1], fFFTSize);

    // Prepare to the next Pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;
  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // Extreme coefficients are always real
    FreqDomain[0                 ] := (FBuffer^[0] + FBuffer^[NbrCoef]) * FScaleFactor;
    FreqDomain[NbrCoef           ] := (FBuffer^[0] - FBuffer^[NbrCoef]) * FScaleFactor;
    FreqDomain[          NbrCoefH] := FBuffer^[          NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoef + NbrCoefH] := FBuffer^[NbrCoef + NbrCoefH] * FScaleFactor;

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := FBuffer^[NbrCoef + i] * c - FBuffer^[NbrCoef + NbrCoefH + i] * s;
      FreqDomain[          i] := (FBuffer^[i] + v) * FScaleFactor;
      FreqDomain[NbrCoef - i] := (FBuffer^[i] - v) * FScaleFactor;

      v := FBuffer^[NbrCoef + i] * s + FBuffer^[NbrCoef + NbrCoefH + i] * c;
      FreqDomain[    NbrCoef + i] := (v + FBuffer^[NbrCoefH + i]) * FScaleFactor;
      FreqDomain[2 * NbrCoef - i] := (v - FBuffer^[NbrCoefH + i]) * FScaleFactor;
     end;
   end
  else
   begin
    // Extreme coefficients are always real
    FreqDomain[0                 ] := FBuffer^[0] + FBuffer^[NbrCoef];
    FreqDomain[NbrCoef           ] := FBuffer^[0] - FBuffer^[NbrCoef];
    FreqDomain[          NbrCoefH] := FBuffer^[          NbrCoefH];
    FreqDomain[NbrCoef + NbrCoefH] := FBuffer^[NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := FBuffer^[NbrCoef + i] * c - FBuffer^[NbrCoef + NbrCoefH + i] * s;
      FreqDomain[          i] := FBuffer^[i] + v;
      FreqDomain[NbrCoef - i] := FBuffer^[i] - v;

      v := FBuffer^[NbrCoef + i] * s + FBuffer^[NbrCoef + NbrCoefH + i] * c;
      FreqDomain[    NbrCoef + i] := v + FBuffer^[NbrCoefH + i];
      FreqDomain[2 * NbrCoef - i] := v - FBuffer^[NbrCoefH + i];
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTEven32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  NbrCoefD    : Integer;
  c, s, v     : Double;
  BitPos      : array [0..1] of Integer;
  TempBuffer  : array [0..2] of PDAVSingleFixedArray;
begin
  TempBuffer[0] := @FreqDomain[0];
  TempBuffer[1] := @FBuffer^[0];

  // first and second pass at once
  ci := fFFTSize;
  repeat
    Dec(ci, 4);

    BitPos[0] := FBitRevLUT.LUT[ci    ];
    BitPos[1] := FBitRevLUT.LUT[ci + 1];
    TempBuffer[0][ci + 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci + 2];
    BitPos[1] := FBitRevLUT.LUT[ci + 3];
    TempBuffer[0][ci + 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    TempBuffer[0][ci    ] := s + c;
    TempBuffer[0][ci + 2] := s - c;
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2];
    TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6];

    v := (TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + v;
    TempBuffer[1][ci + 3] := TempBuffer[0][ci + 1] - v;

    v := (TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 5] := v + TempBuffer[0][ci + 3];
    TempBuffer[1][ci + 7] := v - TempBuffer[0][ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  for Pass := 3 to FOrder - 2 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;
    ci := 0;

    repeat
      // Extreme coefficients are always real
      TempBuffer[0, 0                 ] := TempBuffer[1, 0] + TempBuffer[1][NbrCoef];
      TempBuffer[0, NbrCoef           ] := TempBuffer[1, 0] - TempBuffer[1][NbrCoef];
      TempBuffer[0,           NbrCoefH] := TempBuffer[1,           NbrCoefH];
      TempBuffer[0, NbrCoef + NbrCoefH] := TempBuffer[1, NbrCoef + NbrCoefH];

      // Others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TempBuffer[0][         + i] := TempBuffer[1][i] + v;
        TempBuffer[0][NbrCoef  - i] := TempBuffer[1][i] - v;

        v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
        TempBuffer[0][NbrCoef  + i] := v + TempBuffer[1][NbrCoefH + i];
        TempBuffer[0][NbrCoefD - i] := v - TempBuffer[1][NbrCoefH + i];
       end;

      Inc(ci, NbrCoef * 2);
      Inc(TempBuffer[0], NbrCoef * 2);
      Inc(TempBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TempBuffer[0], fFFTSize);
    Dec(TempBuffer[1], fFFTSize);

    // Prepare to the next Pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  // last pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := (TempBuffer[1][0] + TempBuffer[1][NbrCoef]) * FScaleFactor;
    FreqDomain[0].Im := (TempBuffer[1][0] - TempBuffer[1][NbrCoef]) * FScaleFactor;

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH] * FScaleFactor;

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := (TempBuffer[1][i] + v) * FScaleFactor;
      FreqDomain[NbrCoef - i].Re := (TempBuffer[1][i] - v) * FScaleFactor;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := (v + TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
      FreqDomain[NbrCoef - i].Im := (v - TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
     end;
   end
  else 
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    FreqDomain[0].Im := TempBuffer[1][0] - TempBuffer[1][NbrCoef];

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH];
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH];

    // others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := TempBuffer[1][i] + v;
      FreqDomain[NbrCoef - i].Re := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := v + TempBuffer[1][NbrCoefH + i];
      FreqDomain[NbrCoef - i].Im := v - TempBuffer[1][NbrCoefH + i];
     end;
   end;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// IFFT ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TFftReal2ComplexNativeFloat32.PerformIFFTZero32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
begin
  TimeDomain^[0] := FreqDomain^[0];
end;
{$ELSE}
asm
    FLD     [FreqDomain].Single
    FSTP    [TimeDomain].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTZero32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
begin
  TimeDomain^[0] := FreqDomain^[0].Re;
end;
{$ELSE}
asm
    FLD     [FreqDomain].Single
    FSTP    [TimeDomain].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTOne32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2SingleArray absolute TimeDomain;
  FD : PDAV2SingleArray absolute FreqDomain;
begin
  TD[0] := FD[0] + FD[1];
  TD[1] := FD[0] - FD[1];
end;
{$ELSE}
asm
    FLD     [FreqDomain    ].Single
    FLD     ST(0)
    FADD    [FreqDomain + 4].Single
    FSTP    [TimeDomain    ].Single
    FSUB    [FreqDomain + 4].Single
    FSTP    [TimeDomain + 4].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTOne32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2SingleArray absolute TimeDomain;
  FD : PComplex32 absolute FreqDomain;
begin
  TD[0] := FD.Re + FD.Im;
  TD[1] := FD.Re - FD.Im;
end;
{$ELSE}
asm
    FLD     [FreqDomain].Single
    FLD     ST(0)
    FADD    [FreqDomain + 4].Single
    FSTP    [TimeDomain    ].Single
    FSUB    [FreqDomain + 4].Single
    FSTP    [TimeDomain + 4].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTTwo32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Double;
  FD  : PDAV4SingleArray absolute FreqDomain;
  TD  : PDAV4SingleArray absolute TimeDomain;
begin
  Tmp[0] := FD[0] + FD[2];
  Tmp[1] := FD[0] - FD[2];

  TD[1]  := Tmp[1] + FD[3] * 2;
  TD[3]  := Tmp[1] - FD[3] * 2;
  TD[0]  := Tmp[0] + FD[1] * 2;
  TD[2]  := Tmp[0] - FD[1] * 2;
end;
{$ELSE}
const
  c2 : Double = 2;
asm
    FLD     [FreqDomain +  8].Single
    FLD     [FreqDomain].Single
    FLD     ST(0)
    FADD    ST(0), ST(2)
    FXCH    ST(2)
    FSUBP                             // b1, b0
    FLD     [FreqDomain + 12].Single  // f3, b1, b0
    FMUL    c2                        // 2 * f3, b1, b0
    FLD     ST(0)                     // 2 * f3, 2 * f3, b1, b0
    FADD    ST(0),ST(2)               // b1 + 2 * f3, 2 * f3, b1, b0
    FSTP    [TimeDomain +  4].Single  // 2 * f3, b1, b0
    FSUBP                             // b1 - 2 * f3, b0
    FSTP    [TimeDomain+ 12].Single   // b0
    FLD     [FreqDomain +  4].Single  // f1, b0
    FMUL    c2                        // 2 * f1, b0
    FLD     ST(0)                     // 2 * f1, 2 * f1, b0
    FADD    ST(0),ST(2)               // 2 * f1 + b0, 2 * f1, b0
    FSTP    [TimeDomain].Single       // 2 * f1, b0
    FSUBP                             // b0 - 2 * f1
    FSTP    [TimeDomain +  8].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTTwo32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Double;
  FD  : PDAV2ComplexSingleArray absolute FreqDomain;
  TD  : PDAV4SingleArray absolute TimeDomain;
begin
  Tmp[0] := FD[0].Re + FD[0].Im;
  Tmp[1] := FD[0].Re - FD[0].Im;

  TD[1]  := Tmp[1] + FD[1].Im * 2;
  TD[3]  := Tmp[1] - FD[1].Im * 2;
  TD[0]  := Tmp[0] + FD[1].Re * 2;
  TD[2]  := Tmp[0] - FD[1].Re * 2;
end;
{$ELSE}
const
  c2 : Double = 2;
asm
    FLD     [FreqDomain +  8].Single
    FLD     [FreqDomain].Single
    FLD     ST(0)
    FADD    ST(0),ST(2)
    FXCH    ST(2)
    FSUBP                            // b1, b0
    FLD     [FreqDomain + 12].Single // f3, b1, b0
    FMUL    c2                       // 2 * f3, b1, b0
    FLD     ST(0)                    // 2 * f3, 2 * f3, b1, b0
    FADD    ST(0), ST(2)             // b1 + 2 * f3, 2 * f3, b1, b0
    FSTP    [TimeDomain +  4].Single // 2 * f3, b1, b0
    FSUBP                            // b1 - 2 * f3, b0
    FSTP    [TimeDomain + 12].Single // b0
    FLD     [FreqDomain +  4].Single // f1, b0
    FMUL    c2                       // 2 * f1, b0
    FLD     ST(0)                    // 2 * f1, 2 * f1, b0
    FADD    ST(0),ST(2)              // 2 * f1 + b0, 2 * f1, b0
    FSTP    [TimeDomain].Single      // 2 * f1, b0
    FSUBP                            // b0 - 2 * f1
    FSTP    [TimeDomain + 8].Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTThree32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  FD  : PDAV8SingleArray absolute FreqDomain;
  TD  : PDAV8SingleArray absolute TimeDomain;
  Bf  : PDAV8SingleArray;
begin
  Bf := PDAV8SingleArray(FBuffer);
  Bf[0] := FD[0] + FD[4];
  Bf[4] := FD[0] - FD[4];
  Bf[1] := FD[1] + FD[3];
  Bf[3] := FD[5] - FD[7];
  Bf[2] := FD[1] - FD[3];
  Bf[6] := FD[5] + FD[7];
  Bf[5] := (Bf[2] + Bf[6]) * CSQRT2Div2;
  Bf[7] := (Bf[6] - Bf[2]) * CSQRT2Div2;
  Bf[2] := Bf[0] + FD[2] * 2;
  Bf[0] := Bf[0] - FD[2] * 2;
  Bf[6] := Bf[1] * 2;
  Bf[3] := Bf[3] * 2;
  TD[0] := Bf[2] + Bf[6];
  TD[4] := Bf[2] - Bf[6];
  TD[2] := Bf[0] + Bf[3];
  TD[6] := Bf[0] - Bf[3];
  Bf[2] := Bf[4] + FD[6] * 2;
  Bf[4] := Bf[4] - FD[6] * 2;
  Bf[6] := Bf[5] * 2;
  Bf[7] := Bf[7] * 2;
  TD[1] := Bf[2] + Bf[6];
  TD[5] := Bf[2] - Bf[6];
  TD[3] := Bf[4] + Bf[7];
  TD[7] := Bf[4] - Bf[7];
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTThree32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  FD  : PDAV4ComplexSingleArray absolute FreqDomain;
  TD  : PDAV8SingleArray absolute TimeDomain;
  Bf  : PDAV8SingleArray;
begin
  Bf := PDAV8SingleArray(FBuffer);
  Bf[0]  := FD[0].Re + FD[0].Im;
  Bf[4]  := FD[0].Re - FD[0].Im;
  Bf[1]  := FD[1].Re + FD[3].Re;
  Bf[3]  := FD[1].Im - FD[3].Im;
  Bf[2]  := FD[1].Re - FD[3].Re;
  Bf[6]  := FD[1].Im + FD[3].Im;
  Bf[5]  := (Bf[2] + Bf[6]) * CSQRT2Div2;
  Bf[7]  := (Bf[6] - Bf[2]) * CSQRT2Div2;
  Bf[2]  := Bf[0] + FD[2].Re * 2;
  Bf[0]  := Bf[0] - FD[2].Re * 2;
  Bf[6]  := Bf[1] * 2;
  Bf[3]  := Bf[3] * 2;
  TD[0]  := Bf[2] + Bf[6];
  TD[4]  := Bf[2] - Bf[6];
  TD[2]  := Bf[0] + Bf[3];
  TD[6]  := Bf[0] - Bf[3];
  Bf[2]  := Bf[4] + FD[2].Im * 2;
  Bf[4]  := Bf[4] - FD[2].Im * 2;
  Bf[6]  := Bf[5] * 2;
  Bf[7]  := Bf[7] * 2;
  TD[1]  := Bf[2] + Bf[6];
  TD[5]  := Bf[2] - Bf[6];
  TD[3]  := Bf[4] + Bf[7];
  TD[7]  := Bf[4] - Bf[7];
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTFour32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  NbrCoef  : Integer;
  NbrCoefH : Integer;
  NbrCoefD : Integer;
  ci       : Integer;
  c, s     : Double;
  Tmp      : array [0..3] of Double;
begin
  // Do the transformation in several passes

  // first pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;
  NbrCoefD := NbrCoef shl 1;

  // extreme coefficients are always real
  TimeDomain[0] := FreqDomain[0] + FreqDomain[NbrCoef];
  TimeDomain[NbrCoef] := FreqDomain[0] - FreqDomain[NbrCoef];
  TimeDomain[NbrCoefH] := FreqDomain[NbrCoefH] * 2;
  TimeDomain[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH + NbrCoef] * 2;

  // others are conjugate complex numbers

  for ci := 1 to NbrCoefH - 1 do
   begin
    TimeDomain[ci           ] := FreqDomain[          ci] + FreqDomain[NbrCoef  - ci];
    TimeDomain[ci + NbrCoefH] := FreqDomain[NbrCoef + ci] - FreqDomain[NbrCoefD - ci];

    c := TrigoLUT[NbrCoefH - 4 + ci]; // cos (i * PI / NbrCoef);
    s := TrigoLUT[NbrCoef  - 4 - ci]; // sin (i * PI / NbrCoef);

    Tmp[0] := FreqDomain[          ci] - FreqDomain[NbrCoef           - ci];
    Tmp[1] := FreqDomain[NbrCoef + ci] + FreqDomain[NbrCoef + NbrCoef - ci];

    TimeDomain[NbrCoef            + ci] := Tmp[0] * c + Tmp[1] * s;
    TimeDomain[NbrCoef + NbrCoefH + ci] := Tmp[1] * c - Tmp[0] * s;
   end;

  // antepenultimate pass
  ci := 0;
  repeat
    FBuffer^[ci    ] := TimeDomain[ci] + TimeDomain[ci + 4];
    FBuffer^[ci + 4] := TimeDomain[ci] - TimeDomain[ci + 4];
    FBuffer^[ci + 2] := TimeDomain[ci + 2] * 2;
    FBuffer^[ci + 6] := TimeDomain[ci + 6] * 2;

    FBuffer^[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
    FBuffer^[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

    Tmp[0] := TimeDomain[ci + 1] - TimeDomain[ci + 3];
    Tmp[1] := TimeDomain[ci + 5] + TimeDomain[ci + 7];

    FBuffer^[ci + 5] := (Tmp[0] + Tmp[1]) * CSQRT2Div2;
    FBuffer^[ci + 7] := (Tmp[1] - Tmp[0]) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);


  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := FBuffer^[ci] + FBuffer^[ci + 2];
    Tmp[2] := FBuffer^[ci] - FBuffer^[ci + 2];
    Tmp[1] := FBuffer^[ci + 1] * 2;
    Tmp[3] := FBuffer^[ci + 3] * 2;

    TimeDomain[FBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

    Tmp[0] := FBuffer^[ci + 4] + FBuffer^[ci + 6];
    Tmp[2] := FBuffer^[ci + 4] - FBuffer^[ci + 6];
    Tmp[1] := FBuffer^[ci + 5] * 2;
    Tmp[3] := FBuffer^[ci + 7] * 2;

    TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTFour32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  NbrCoef  : Integer;
  NbrCoefH : Integer;
  ci       : Integer;
  c, s     : Double;
  Tmp      : array [0..3] of Double;
begin
  // Do the transformation in several passes

  // first pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideInvByN, astDivideBySqrtN] then
   begin
    // extreme coefficients are always real
    TimeDomain[0]                  := (FreqDomain[0].Re + FreqDomain[0].Im) * FScaleFactor;
    TimeDomain[NbrCoef]            := (FreqDomain[0].Re - FreqDomain[0].Im) * FScaleFactor;
    TimeDomain[NbrCoefH]           := FreqDomain[NbrCoefH].Re * 2 * FScaleFactor;
    TimeDomain[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH].Im * 2 * FScaleFactor;

    // others are conjugate complex numbers
    for ci := 1 to NbrCoefH - 1 do
     begin
      TimeDomain[ci           ] := (FreqDomain[ci].Re + FreqDomain[NbrCoef - ci].Re) * FScaleFactor;
      TimeDomain[ci + NbrCoefH] := (FreqDomain[ci].Im - FreqDomain[NbrCoef - ci].Im) * FScaleFactor;

      c := TrigoLUT[NbrCoefH - 4 + ci];
      s := TrigoLUT[NbrCoef  - 4 - ci];

      Tmp[0] := (FreqDomain[ci].Re - FreqDomain[NbrCoef - ci].Re) * FScaleFactor;
      Tmp[1] := (FreqDomain[ci].Im + FreqDomain[NbrCoef - ci].Im) * FScaleFactor;

      TimeDomain[NbrCoef            + ci] := Tmp[0] * c + Tmp[1] * s;
      TimeDomain[NbrCoef + NbrCoefH + ci] := Tmp[1] * c - Tmp[0] * s;
     end;
   end
  else
   begin
    // extreme coefficients are always real
    TimeDomain[0]                  := FreqDomain[0].Re + FreqDomain[0].Im;
    TimeDomain[NbrCoef]            := FreqDomain[0].Re - FreqDomain[0].Im;
    TimeDomain[NbrCoefH]           := FreqDomain[NbrCoefH].Re * 2;
    TimeDomain[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH].Im * 2;

    // others are conjugate complex numbers
    for ci := 1 to NbrCoefH - 1 do
     begin
      TimeDomain[ci           ] := FreqDomain[ci].Re + FreqDomain[NbrCoef - ci].Re;
      TimeDomain[ci + NbrCoefH] := FreqDomain[ci].Im - FreqDomain[NbrCoef - ci].Im;

      c := TrigoLUT[NbrCoefH - 4 + ci];
      s := TrigoLUT[NbrCoef  - 4 - ci];

      Tmp[0] := FreqDomain[ci].Re - FreqDomain[NbrCoef - ci].Re;
      Tmp[1] := FreqDomain[ci].Im + FreqDomain[NbrCoef - ci].Im;

      TimeDomain[NbrCoef            + ci] := Tmp[0] * c + Tmp[1] * s;
      TimeDomain[NbrCoef + NbrCoefH + ci] := Tmp[1] * c - Tmp[0] * s;
     end;
   end;

  // antepenultimate pass
  ci := 0;
  repeat
    FBuffer^[ci    ] := TimeDomain[ci] + TimeDomain[ci + 4];
    FBuffer^[ci + 4] := TimeDomain[ci] - TimeDomain[ci + 4];
    FBuffer^[ci + 2] := TimeDomain[ci + 2] * 2;
    FBuffer^[ci + 6] := TimeDomain[ci + 6] * 2;

    FBuffer^[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
    FBuffer^[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

    Tmp[0] := TimeDomain[ci + 1] - TimeDomain[ci + 3];
    Tmp[1] := TimeDomain[ci + 5] + TimeDomain[ci + 7];

    FBuffer^[ci + 5] := (Tmp[0] + Tmp[1]) * CSQRT2Div2;
    FBuffer^[ci + 7] := (Tmp[1] - Tmp[0]) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := FBuffer^[ci] + FBuffer^[ci + 2];
    Tmp[1] := FBuffer^[ci] - FBuffer^[ci + 2];
    Tmp[2] := FBuffer^[ci + 1] * 2;
    Tmp[3] := FBuffer^[ci + 3] * 2;

    TimeDomain[FBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[2];
    TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[2];
    TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[1] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[1] - Tmp[3];

    Tmp[0] := FBuffer^[ci + 4] + FBuffer^[ci + 6];
    Tmp[1] := FBuffer^[ci + 4] - FBuffer^[ci + 6];
    Tmp[2] := FBuffer^[ci + 5] * 2;
    Tmp[3] := FBuffer^[ci + 7] * 2;

    TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[2];
    TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[2];
    TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[1] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[1] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTEven32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Pass        : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  NbrCoefD    : Integer;
  i, ci       : Integer;
  Tmp         : array [0..3] of Double;
  TempBuffer  : array [0..2] of PDAVSingleFixedArray;
begin
  // Do the transformation in several passes

  // first pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;
  NbrCoefD := NbrCoef shl 1;

  if FAutoScaleType in [astDivideInvByN, astDivideBySqrtN] then
   begin
    // extreme coefficients are always real
    TimeDomain[0                 ] := (FreqDomain[0] + FreqDomain[NbrCoef]) * FScaleFactor;
    TimeDomain[NbrCoef           ] := (FreqDomain[0] - FreqDomain[NbrCoef]) * FScaleFactor;
    TimeDomain[        + NbrCoefH] := (FreqDomain[NbrCoefH] * 2) * FScaleFactor;
    TimeDomain[NbrCoef + NbrCoefH] := (FreqDomain[NbrCoefH + NbrCoef] * 2) * FScaleFactor;

    // others are conjugate complex numbers

    for i := 1 to NbrCoefH - 1 do
     begin
      TimeDomain[i           ] := (FreqDomain[i] + FreqDomain[NbrCoef - i]) * FScaleFactor;
      TimeDomain[i + NbrCoefH] := (FreqDomain[NbrCoef + i] - FreqDomain[NbrCoefD - i]) * FScaleFactor;

      Tmp[0] := TrigoLUT[NbrCoefH - 4 + i]; // cos (i * PI / NbrCoef);
      Tmp[1] := TrigoLUT[NbrCoef  - 4 - i]; // sin (i * PI / NbrCoef);

      Tmp[2] := (FreqDomain[        + i] - FreqDomain[NbrCoef           - i]) * FScaleFactor;
      Tmp[3] := (FreqDomain[NbrCoef + i] + FreqDomain[NbrCoef + NbrCoef - i]) * FScaleFactor;

      TimeDomain[NbrCoef            + i] := Tmp[2] * Tmp[0] + Tmp[3] * Tmp[1];
      TimeDomain[NbrCoef + NbrCoefH + i] := Tmp[3] * Tmp[0] - Tmp[2] * Tmp[1];
     end;
   end
  else
   begin
    // extreme coefficients are always real
    TimeDomain[0                 ] := FreqDomain[0] + FreqDomain[NbrCoef];
    TimeDomain[NbrCoef           ] := FreqDomain[0] - FreqDomain[NbrCoef];
    TimeDomain[        + NbrCoefH] := FreqDomain[NbrCoefH] * 2;
    TimeDomain[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH + NbrCoef] * 2;

    // others are conjugate complex numbers

    for i := 1 to NbrCoefH - 1 do
     begin
      TimeDomain[i           ] := FreqDomain[        + i] + FreqDomain[NbrCoef           - i];
      TimeDomain[i + NbrCoefH] := FreqDomain[NbrCoef + i] - FreqDomain[NbrCoef + NbrCoef - i];

      Tmp[0] := TrigoLUT[NbrCoefH - 4 + i]; // cos (i * PI / NbrCoef);
      Tmp[1] := TrigoLUT[NbrCoef  - 4 - i]; // sin (i * PI / NbrCoef);

      Tmp[2] := FreqDomain[        + i] - FreqDomain[NbrCoef           - i];
      Tmp[3] := FreqDomain[NbrCoef + i] + FreqDomain[NbrCoef + NbrCoef - i];

      TimeDomain[NbrCoef            + i] := Tmp[2] * Tmp[0] + Tmp[3] * Tmp[1];
      TimeDomain[NbrCoef + NbrCoefH + i] := Tmp[3] * Tmp[0] - Tmp[2] * Tmp[1];
     end;
   end;

  // prepare to the next pass
  TempBuffer[0] := @TimeDomain[0];
  TempBuffer[1] := @FBuffer^[0];

  // first pass
  for Pass := FOrder - 2 downto 3 do
   begin
    ci := 0;
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;

    repeat
      // extreme coefficients are always real
      TempBuffer[1][ci                     ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoef           ] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci           + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
      TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

      // others are conjugate complex numbers

      for i := 1 to NbrCoefH - 1 do
       begin
        TempBuffer[1][ci + i           ] := TempBuffer[0][ci           + i] + TempBuffer[0][ci + NbrCoef           - i];
        TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        Tmp[0] := TrigoLUT[NbrCoefH - 4 + i]; // cos (i * PI / NbrCoef);
        Tmp[1] := TrigoLUT[NbrCoef  - 4 - i]; // sin (i * PI / NbrCoef);

        Tmp[2] := TempBuffer[0][ci           + i] - TempBuffer[0][ci + NbrCoef           - i];
        Tmp[3] := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        TempBuffer[1][ci + NbrCoef            + i] := Tmp[2] * Tmp[0] + Tmp[3] * Tmp[1];
        TempBuffer[1][ci + NbrCoef + NbrCoefH + i] := Tmp[3] * Tmp[0] - Tmp[2] * Tmp[1];
       end;

      Inc(ci, NbrCoefD);
    until (ci >= fFFTSize);

    // prepare to the next pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  // antepenultimate pass
  ci := 0;
  repeat
    TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2] * 2;
    TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6] * 2;

    TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + TempBuffer[0][ci + 3];
    TempBuffer[1][ci + 3] := TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7];

    Tmp[2] := TempBuffer[0][ci + 1] - TempBuffer[0][ci + 3];
    Tmp[3] := TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7];

    TempBuffer[1][ci + 5] := (Tmp[2] + Tmp[3]) * CSQRT2Div2;
    TempBuffer[1][ci + 7] := (Tmp[3] - Tmp[2]) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);


  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := TempBuffer[1][ci] + TempBuffer[1][ci + 2];
    Tmp[2] := TempBuffer[1][ci] - TempBuffer[1][ci + 2];
    Tmp[1] := TempBuffer[1][ci + 1] * 2;
    Tmp[3] := TempBuffer[1][ci + 3] * 2;

    TimeDomain[FBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

    Tmp[0] := TempBuffer[1][ci + 4] + TempBuffer[1][ci + 6];
    Tmp[2] := TempBuffer[1][ci + 4] - TempBuffer[1][ci + 6];
    Tmp[1] := TempBuffer[1][ci + 5] * 2;
    Tmp[3] := TempBuffer[1][ci + 7] * 2;

    TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTEven32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Tmp         : array [0..3] of Double;
  ci, i       : Integer;
  TempBuffer  : array [0..2] of PDAVSingleFixedArray;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  NbrCoefD    : Integer;
  Pass        : Integer;
begin
  // Do the transformation in several passes

  // first pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideInvByN, astDivideBySqrtN] then
   begin
    // extreme coefficients are always real
    TimeDomain[0]                  := (FreqDomain[0].Re + FreqDomain[0].Im) * FScaleFactor;
    TimeDomain[NbrCoef]            := (FreqDomain[0].Re - FreqDomain[0].Im) * FScaleFactor;
    TimeDomain[NbrCoefH]           := FreqDomain[NbrCoefH].Re * 2 * FScaleFactor;
    TimeDomain[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH].Im * 2 * FScaleFactor;

    // others are conjugate complex numbers
    for ci := 1 to NbrCoefH - 1 do
     begin
      TimeDomain[ci           ] := (FreqDomain[ci].Re + FreqDomain[NbrCoef - ci].Re) * FScaleFactor;
      TimeDomain[ci + NbrCoefH] := (FreqDomain[ci].Im - FreqDomain[NbrCoef - ci].Im) * FScaleFactor;

      Tmp[0] := TrigoLUT[NbrCoefH - 4 + ci];
      Tmp[1] := TrigoLUT[NbrCoef  - 4 - ci];

      Tmp[2] := (FreqDomain[ci].Re - FreqDomain[NbrCoef - ci].Re) * FScaleFactor;
      Tmp[3] := (FreqDomain[ci].Im + FreqDomain[NbrCoef - ci].Im) * FScaleFactor;

      TimeDomain[NbrCoef            + ci] := Tmp[2] * Tmp[0] + Tmp[3] * Tmp[1];
      TimeDomain[NbrCoef + NbrCoefH + ci] := Tmp[3] * Tmp[0] - Tmp[2] * Tmp[1];
     end;
   end
  else
   begin
    // extreme coefficients are always real
    TimeDomain[0]                  := FreqDomain[0].Re + FreqDomain[0].Im;
    TimeDomain[NbrCoef]            := FreqDomain[0].Re - FreqDomain[0].Im;
    TimeDomain[NbrCoefH]           := FreqDomain[NbrCoefH].Re * 2;
    TimeDomain[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH].Im * 2;

    // others are conjugate complex numbers
    for ci := 1 to NbrCoefH - 1 do
     begin
      TimeDomain[ci           ] := FreqDomain[ci].Re + FreqDomain[NbrCoef - ci].Re;
      TimeDomain[ci + NbrCoefH] := FreqDomain[ci].Im - FreqDomain[NbrCoef - ci].Im;

      Tmp[0] := TrigoLUT[NbrCoefH - 4 + ci];
      Tmp[1] := TrigoLUT[NbrCoef  - 4 - ci];

      Tmp[2] := FreqDomain[ci].Re - FreqDomain[NbrCoef - ci].Re;
      Tmp[3] := FreqDomain[ci].Im + FreqDomain[NbrCoef - ci].Im;

      TimeDomain[NbrCoef            + ci] := Tmp[2] * Tmp[0] + Tmp[3] * Tmp[1];
      TimeDomain[NbrCoef + NbrCoefH + ci] := Tmp[3] * Tmp[0] - Tmp[2] * Tmp[1];
     end;
   end;

  TempBuffer[0] := @TimeDomain[0];
  TempBuffer[1] := @FBuffer^[0];

  // second pass
  for Pass := FOrder - 2 downto 3 do
   begin
    ci := 0;
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;

    repeat
      // extreme coefficients are always real
      TempBuffer[1][0] := TempBuffer[0][0] + TempBuffer[0][NbrCoef];
      TempBuffer[1][NbrCoef] := TempBuffer[0][0] - TempBuffer[0][NbrCoef];
      TempBuffer[1][NbrCoefH] := TempBuffer[0][NbrCoefH] * 2;
      TempBuffer[1][NbrCoef + NbrCoefH] := TempBuffer[0][NbrCoefH + NbrCoef] * 2;

      // others are conjugate complex numbers

      for i := 1 to NbrCoefH - 1 do
       begin
        TempBuffer[1][i           ] := TempBuffer[0][        + i] + TempBuffer[0][NbrCoef  - i];
        TempBuffer[1][i + NbrCoefH] := TempBuffer[0][NbrCoef + i] - TempBuffer[0][NbrCoefD - i];

        Tmp[0] := TrigoLUT[NbrCoefH - 4 + i]; // cos (i * PI / NbrCoef);
        Tmp[1] := TrigoLUT[NbrCoef  - 4 - i]; // sin (i * PI / NbrCoef);

        Tmp[2] := TempBuffer[0][        + i] - TempBuffer[0][NbrCoef  - i];
        Tmp[3] := TempBuffer[0][NbrCoef + i] + TempBuffer[0][NbrCoefD - i];

        TempBuffer[1][NbrCoef            + i] := Tmp[2] * Tmp[0] + Tmp[3] * Tmp[1];
        TempBuffer[1][NbrCoef + NbrCoefH + i] := Tmp[3] * Tmp[0] - Tmp[2] * Tmp[1];
       end;

      Inc(ci, NbrCoefD);
      Inc(TempBuffer[0], NbrCoefD);
      Inc(TempBuffer[1], NbrCoefD);
    until (ci >= fFFTSize);
    Dec(TempBuffer[0], fFFTSize);
    Dec(TempBuffer[1], fFFTSize);

   // prepare to the next pass
   TempBuffer[2] := TempBuffer[0];
   TempBuffer[0] := TempBuffer[1];
   TempBuffer[1] := TempBuffer[2];
  end;

  // antepenultimate pass
  ci := 0;
  repeat
    TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2] * 2;
    TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6] * 2;

    TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + TempBuffer[0][ci + 3];
    TempBuffer[1][ci + 3] := TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7];

    Tmp[0] := TempBuffer[0][ci + 1] - TempBuffer[0][ci + 3];
    Tmp[1] := TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7];

    TempBuffer[1][ci + 5] := (Tmp[0] + Tmp[1]) * CSQRT2Div2;
    TempBuffer[1][ci + 7] := (Tmp[1] - Tmp[0]) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);


  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := TempBuffer[1][ci] + TempBuffer[1][ci + 2];
    Tmp[1] := TempBuffer[1][ci] - TempBuffer[1][ci + 2];
    Tmp[2] := TempBuffer[1][ci + 1] * 2;
    Tmp[3] := TempBuffer[1][ci + 3] * 2;

    TimeDomain[FBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[2];
    TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[2];
    TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[1] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[1] - Tmp[3];

    Tmp[0] := TempBuffer[1][ci + 4] + TempBuffer[1][ci + 6];
    Tmp[1] := TempBuffer[1][ci + 4] - TempBuffer[1][ci + 6];
    Tmp[2] := TempBuffer[1][ci + 5] * 2;
    Tmp[3] := TempBuffer[1][ci + 7] * 2;

    TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[2];
    TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[2];
    TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[1] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[1] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;


procedure TFftReal2ComplexNativeFloat32.PerformIFFTOdd32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Pass         : Integer;
  NbrCoef      : Integer;
  NbrCoefH     : Integer;
  NbrCoefD     : Integer;
  i, ci        : Integer;
  Tmp          : array [0..3] of Single;
  TempBuffer   : array [0..2] of PDAVSingleFixedArray;
begin
  // Do the transformation in several passes

  // first pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;
  NbrCoefD := NbrCoef shl 1;

  if FAutoScaleType in [astDivideInvByN, astDivideBySqrtN] then
   begin
    // extreme coefficients are always real
    FBuffer^[0                 ] := (FreqDomain[0] + FreqDomain[NbrCoef]) * FScaleFactor;
    FBuffer^[NbrCoef           ] := (FreqDomain[0] - FreqDomain[NbrCoef]) * FScaleFactor;
    FBuffer^[          NbrCoefH] := FreqDomain[NbrCoefH] * 2 * FScaleFactor;
    FBuffer^[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH + NbrCoef] * 2 * FScaleFactor;

    // others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      FBuffer^[i           ] := (FreqDomain[        + i] + FreqDomain[NbrCoef  - i]) * FScaleFactor;
      FBuffer^[i + NbrCoefH] := (FreqDomain[NbrCoef + i] - FreqDomain[NbrCoefD - i]) * FScaleFactor;

      Tmp[0] := TrigoLUT[NbrCoefH - 4  + i]; // cos (i * PI / NbrCoef);
      Tmp[1] := TrigoLUT[NbrCoef  - 4  - i]; // sin (i * PI / NbrCoef);

      Tmp[2] := (FreqDomain[        + i] - FreqDomain[NbrCoef           - i]) * FScaleFactor;
      Tmp[3] := (FreqDomain[NbrCoef + i] + FreqDomain[NbrCoef + NbrCoef - i]) * FScaleFactor;

      FBuffer^[NbrCoef            + i] := Tmp[2] * Tmp[0] + Tmp[3] * Tmp[1];
      FBuffer^[NbrCoef + NbrCoefH + i] := Tmp[3] * Tmp[0] - Tmp[2] * Tmp[1];
     end;
   end
  else
   begin
    // extreme coefficients are always real
    FBuffer^[0                 ] := FreqDomain[0] + FreqDomain[NbrCoef];
    FBuffer^[NbrCoef           ] := FreqDomain[0] - FreqDomain[NbrCoef];
    FBuffer^[          NbrCoefH] := FreqDomain[NbrCoefH] * 2;
    FBuffer^[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH + NbrCoef] * 2;

    // others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      FBuffer^[i           ] := FreqDomain[        + i] + FreqDomain[NbrCoef           - i];
      FBuffer^[i + NbrCoefH] := FreqDomain[NbrCoef + i] - FreqDomain[NbrCoef + NbrCoef - i];

      Tmp[0] := TrigoLUT[NbrCoefH - 4 + i]; // cos (i * PI / NbrCoef);
      Tmp[1] := TrigoLUT[NbrCoef  - 4 - i]; // sin (i * PI / NbrCoef);

      Tmp[2] := FreqDomain[        + i] - FreqDomain[NbrCoef           - i];
      Tmp[3] := FreqDomain[NbrCoef + i] + FreqDomain[NbrCoef + NbrCoef - i];

      FBuffer^[NbrCoef            + i] := Tmp[2] * Tmp[0] + Tmp[3] * Tmp[1];
      FBuffer^[NbrCoef + NbrCoefH + i] := Tmp[3] * Tmp[0] - Tmp[2] * Tmp[1];
     end;
   end;

  TempBuffer[0] := @FBuffer^[0];
  TempBuffer[1] := @TimeDomain[0];

  // second pass
  for Pass := FOrder - 2 downto 3 do
   begin
    ci := 0;
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;

    repeat
      // extreme coefficients are always real
      TempBuffer[1][ci] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
      TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

      // others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        TempBuffer[1][ci + i           ] := TempBuffer[0][ci           + i] + TempBuffer[0][ci + NbrCoef           - i];
        TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        Tmp[0] := TrigoLUT[NbrCoefH - 4 + i]; // cos (i * PI / NbrCoef);
        Tmp[1] := TrigoLUT[NbrCoef  - 4 - i]; // sin (i * PI / NbrCoef);

        Tmp[2] := TempBuffer[0][ci           + i] - TempBuffer[0][ci + NbrCoef           - i];
        Tmp[3] := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        TempBuffer[1][ci + NbrCoef            + i] := Tmp[2] * Tmp[0] + Tmp[3] * Tmp[1];
        TempBuffer[1][ci + NbrCoef + NbrCoefH + i] := Tmp[3] * Tmp[0] - Tmp[2] * Tmp[1];
       end;

      Inc(ci, NbrCoefD);
    until (ci >= fFFTSize);

    // prepare to the next pass
    if (Pass < FOrder - 1) then
     begin
      TempBuffer[2] := TempBuffer[0];
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := TempBuffer[2];
     end
    else
     begin
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := @TimeDomain[0];
     end
   end;

  // antepenultimate pass
  ci := 0;
  repeat
    FBuffer^[ci] := TimeDomain[ci] + TimeDomain[ci + 4];
    FBuffer^[ci + 4] := TimeDomain[ci] - TimeDomain[ci + 4];
    FBuffer^[ci + 2] := TimeDomain[ci + 2] * 2;
    FBuffer^[ci + 6] := TimeDomain[ci + 6] * 2;

    FBuffer^[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
    FBuffer^[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

    Tmp[2] := TimeDomain[ci + 1] - TimeDomain[ci + 3];
    Tmp[3] := TimeDomain[ci + 5] + TimeDomain[ci + 7];

    FBuffer^[ci + 5] := (Tmp[2] + Tmp[3]) * CSQRT2Div2;
    FBuffer^[ci + 7] := (Tmp[3] - Tmp[2]) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := FBuffer^[ci] + FBuffer^[ci + 2];
    Tmp[2] := FBuffer^[ci] - FBuffer^[ci + 2];
    Tmp[1] := FBuffer^[ci + 1] * 2;
    Tmp[3] := FBuffer^[ci + 3] * 2;

    TimeDomain[FBitRevLUT.LUT[ci]] := Tmp[0] + Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

    Tmp[0] := FBuffer^[ci + 4] + FBuffer^[ci + 6];
    Tmp[2] := FBuffer^[ci + 4] - FBuffer^[ci + 6];
    Tmp[1] := FBuffer^[ci + 5] * 2;
    Tmp[3] := FBuffer^[ci + 7] * 2;

    TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTOdd32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Pass         : Integer;
  NbrCoef      : Integer;
  NbrCoefH     : Integer;
  NbrCoefD     : Integer;
  tof, i, ci   : Integer;
  c, s, vr, vi : Double;
  Tmp          : array [0..3] of Single;
  TempBuffer   : array [0..2] of PDAVSingleFixedArray;
begin
  // first pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideInvByN, astDivideBySqrtN] then
   begin
    // extreme coefficients are always real
    FBuffer^[0]                  := (FreqDomain[0].Re + FreqDomain[0].Im) * FScaleFactor;
    FBuffer^[NbrCoef]            := (FreqDomain[0].Re - FreqDomain[0].Im) * FScaleFactor;
    FBuffer^[NbrCoefH]           := FreqDomain[NbrCoefH].Re * 2 * FScaleFactor;
    FBuffer^[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH].Im * 2 * FScaleFactor;

    // others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      FBuffer^[i           ] := (FreqDomain[i].Re + FreqDomain[NbrCoef - i].Re) * FScaleFactor;
      FBuffer^[i + NbrCoefH] := (FreqDomain[i].Im - FreqDomain[NbrCoef - i].Im) * FScaleFactor;

      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      vr := (FreqDomain[i].Re - FreqDomain[NbrCoef - i].Re) * FScaleFactor;
      vi := (FreqDomain[i].Im + FreqDomain[NbrCoef - i].Im) * FScaleFactor;

      FBuffer^[NbrCoef            + i] := vr * c + vi * s;
      FBuffer^[NbrCoef + NbrCoefH + i] := vi * c - vr * s;
     end;
   end
  else
   begin
    // extreme coefficients are always real
    FBuffer^[0]                  := FreqDomain[0].Re + FreqDomain[0].Im;
    FBuffer^[NbrCoef]            := FreqDomain[0].Re - FreqDomain[0].Im;
    FBuffer^[NbrCoefH]           := FreqDomain[NbrCoefH].Re * 2;
    FBuffer^[NbrCoef + NbrCoefH] := FreqDomain[NbrCoefH].Im * 2;

    // others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      TimeDomain[i           ] := FreqDomain[i].Re + FreqDomain[NbrCoef - i].Re;
      TimeDomain[i + NbrCoefH] := FreqDomain[i].Im - FreqDomain[NbrCoef - i].Im;

      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      vr := FreqDomain[i].Re - FreqDomain[NbrCoef - i].Re;
      vi := FreqDomain[i].Im + FreqDomain[NbrCoef - i].Im;

      FBuffer^[NbrCoef            + i] := vr * c + vi * s;
      FBuffer^[NbrCoef + NbrCoefH + i] := vi * c - vr * s;
     end;
   end;

  TempBuffer[0] := @FBuffer^[0];
  TempBuffer[1] := @TimeDomain[0];

  // first pass
  for Pass := FOrder - 2 downto 3 do
   begin
    ci := 0;
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;

    tof := NbrCoefH - 4;

    repeat
      // extreme coefficients are always real
      TempBuffer[1][ci] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
      TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

      // others are conjugate complex numbers

      for i := 1 to NbrCoefH - 1 do
       begin
        TempBuffer[1][ci + i           ] := TempBuffer[0][ci           + i] + TempBuffer[0][ci + NbrCoef           - i];
        TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        c := TrigoLUT[tof            + i]; // cos (i * PI / NbrCoef);
        s := TrigoLUT[tof + NbrCoefH - i]; // sin (i * PI / NbrCoef);

        vr := TempBuffer[0][ci           + i] - TempBuffer[0][ci + NbrCoef           - i];
        vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        TempBuffer[1][ci + NbrCoef            + i] := vr * c + vi * s;
        TempBuffer[1][ci + NbrCoef + NbrCoefH + i] := vi * c - vr * s;
       end;

      Inc(ci, NbrCoefD);
    until (ci >= fFFTSize);

    // prepare to the next pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  // antepenultimate pass
  ci := 0;
  repeat
    FBuffer^[ci    ] := TimeDomain[ci] + TimeDomain[ci + 4];
    FBuffer^[ci + 4] := TimeDomain[ci] - TimeDomain[ci + 4];
    FBuffer^[ci + 2] := TimeDomain[ci + 2] * 2;
    FBuffer^[ci + 6] := TimeDomain[ci + 6] * 2;

    FBuffer^[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
    FBuffer^[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

    vr := TimeDomain[ci + 1] - TimeDomain[ci + 3];
    vi := TimeDomain[ci + 5] + TimeDomain[ci + 7];

    FBuffer^[ci + 5] := (vr + vi) * CSQRT2Div2;
    FBuffer^[ci + 7] := (vi - vr) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := FBuffer^[ci] + FBuffer^[ci + 2];
    Tmp[2] := FBuffer^[ci] - FBuffer^[ci + 2];
    Tmp[1] := FBuffer^[ci + 1] * 2;
    Tmp[3] := FBuffer^[ci + 3] * 2;

    TimeDomain[FBitRevLUT.LUT[ci]] := Tmp[0] + Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

    Tmp[0] := FBuffer^[ci + 4] + FBuffer^[ci + 6];
    Tmp[2] := FBuffer^[ci + 4] - FBuffer^[ci + 6];
    Tmp[1] := FBuffer^[ci + 5] * 2;
    Tmp[3] := FBuffer^[ci + 7] * 2;

    TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
    TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
    TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;

{ TFftReal2ComplexNativeFloat64 }

constructor TFftReal2ComplexNativeFloat64.Create;
begin
 inherited;
 FBuffer := nil;
end;

destructor TFftReal2ComplexNativeFloat64.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TFftReal2ComplexNativeFloat64.SetFFTFunctionPointers;
begin
 ReallocMem(FBuffer, FFTSize * SizeOf(Double));
 case FOrder of
   0: begin
       FPerformFFTPackedReIm := PerformFFTZero64;
       FPerformIFFTPackedReIm := PerformIFFTZero64;
       FPerformFFTPackedComplex := PerformFFTZero64;
       FPerformIFFTPackedComplex := PerformIFFTZero64;
      end;
   1: begin
       FPerformFFTPackedReIm := PerformFFTOne64;
       FPerformIFFTPackedReIm := PerformIFFTOne64;
       FPerformFFTPackedComplex := PerformFFTOne64;
       FPerformIFFTPackedComplex := PerformIFFTOne64;
      end;
   2: begin
       FPerformFFTPackedReIm := PerformFFTTwo64;
       FPerformIFFTPackedReIm := PerformIFFTTwo64;
       FPerformFFTPackedComplex := PerformFFTTwo64;
       FPerformIFFTPackedComplex := PerformIFFTTwo64;
      end;
   3: begin
       FPerformFFTPackedReIm := PerformFFTThree64;
       FPerformIFFTPackedReIm := PerformIFFTThree64;
       FPerformFFTPackedComplex := PerformFFTThree64;
       FPerformIFFTPackedComplex := PerformIFFTThree64;
      end;
   4: begin
      FPerformFFTPackedReIm  := PerformFFTEven64;
      FPerformIFFTPackedReIm := PerformIFFTEven64;
      FPerformFFTPackedComplex := PerformFFTEven64;
      FPerformIFFTPackedComplex := PerformIFFTEven64;
      end;
  else
   begin
    if FOrder and 1 <> 0 then
     begin
      FPerformFFTPackedReIm  := PerformFFTOdd64;
      FPerformIFFTPackedReIm := PerformIFFTOdd64;
      FPerformFFTPackedComplex := PerformFFTOdd64;
      FPerformIFFTPackedComplex := PerformIFFTOdd64;
     end
    else
     begin
      FPerformFFTPackedReIm  := PerformFFTEven64;
      FPerformIFFTPackedReIm := PerformIFFTEven64;
      FPerformFFTPackedComplex := PerformFFTEven64;
      FPerformIFFTPackedComplex := PerformIFFTEven64;
     end;
   end;
 end;
end;

procedure TFftReal2ComplexNativeFloat64.Rescale(const Data: PDAVDoubleFixedArray);
var
  i : Integer;
  s : Double;
begin
 s :=  1 / FFTSize;
 for i := 0 to FFTSize - 1 do Data^[i] := s * Data^[i];
end;

procedure TFftReal2ComplexNativeFloat64.RescaleSqrt(const Data: PDAVDoubleFixedArray);
var
  i : Integer;
  s : Double;
begin
 s :=  sqrt(1 / FFTSize);
 for i := 0 to FFTSize - 1 do Data^[i] := s * Data^[i];
end;

procedure TFftReal2ComplexNativeFloat64.AssignTo(Dest: TPersistent);
var
  Sample : Integer; 
begin
 if Dest is TFftReal2ComplexNativeFloat32 then
  with TFftReal2ComplexNativeFloat32(Dest) do
   begin
    inherited;
    Assert(FFftSize = Self.FFftSize);
    for Sample := 0 to FftSize - 1
     do FBuffer^[Sample] := Self.FBuffer^[Sample];
    SetFFTFunctionPointers;
   end else
 if Dest is TFftReal2ComplexNativeFloat64 then
  with TFftReal2ComplexNativeFloat64(Dest) do
   begin
    inherited;
    Assert(FFftSize = Self.FFftSize);
    Move(FBuffer^, Self.FBuffer^, FftSize * SizeOf(Single));
    SetFFTFunctionPointers;
   end
  else inherited;
end;

procedure TFftReal2ComplexNativeFloat64.CalculateTrigoLUT;
begin
 DoTrigoLUT(FOrder);
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFT(const FrequencyDomain, TimeDomain : Pointer);
begin
 case DataOrder of
  doPackedRealImaginary : FPerformFFTPackedReIm(FrequencyDomain, TimeDomain);
  doPackedComplex : FPerformFFTPackedComplex(FrequencyDomain, TimeDomain);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFT(const FrequencyDomain, TimeDomain : Pointer);
begin
 case DataOrder of
  doPackedRealImaginary : FPerformFFTPackedReIm(FrequencyDomain, TimeDomain);
  doPackedComplex : FPerformFFTPackedComplex(FrequencyDomain, TimeDomain);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

{ FFT Routines }

procedure TFftReal2ComplexNativeFloat64.PerformFFTZero64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
begin
  FreqDomain[0].Re := TimeDomain[0];
end;
{$ELSE}
asm
    FLD    [TimeDomain].Double
    FSTP   [FreqDomain].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTZero64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
begin
  FreqDomain[0] := TimeDomain[0];
end;
{$ELSE}
asm
  FLD     [TimeDomain].Double
  FSTP    [FreqDomain].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTOne64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2DoubleArray absolute TimeDomain;
  FD : PComplex64 absolute FreqDomain;
begin
 FD.Re := TD[0] + TD[1];
 FD.Im := TD[0] - TD[1];
end;
{$ELSE}
asm
  FLD     [TimeDomain].Double
  FLD     ST(0)
  FADD    [TimeDomain + $8].Double
  FSTP    [FreqDomain].Double
  FSUB    [TimeDomain + $8].Double
  FSTP    [FreqDomain + $8].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTOne64(
  const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2DoubleArray absolute TimeDomain;
  FD : PDAV2DoubleArray absolute FreqDomain;
begin
 FD[0] := TD[0] + TD[1];
 FD[1] := TD[0] - TD[1];
end;
{$ELSE}
asm
    FLD     [TimeDomain].Double
    FLD     ST(0)
    FADD    [TimeDomain + $8].Double
    FSTP    [FreqDomain].Double
    FSUB    [TimeDomain + $8].Double
    FSTP    [FreqDomain + $8].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTTwo64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Double;
  TD  : PDAV4DoubleArray absolute TimeDomain;
  FD  : PDAV2ComplexDoubleArray absolute FreqDomain;
begin
  FD[1].Re := TD[0] - TD[2];
  FD[1].Im := TD[1] - TD[3];
  Tmp[0]   := TD[0] + TD[2];
  Tmp[1]   := TD[1] + TD[3];
  FD[0].Re := Tmp[0] + Tmp[1];
  FD[0].Im := Tmp[0] - Tmp[1];
end;
{$ELSE}
asm
    FLD     [TimeDomain      ].Double
    FSUB    [TimeDomain + $10].Double
    FSTP    [FreqDomain +  $8].Double
    FLD     [TimeDomain +  $8].Double
    FSUB    [TimeDomain + $18].Double
    FSTP    [FreqDomain + $18].Double
    FLD     [TimeDomain      ].Double
    FADD    [TimeDomain + $10].Double
    FLD     [TimeDomain +  $8].Double
    FADD    [TimeDomain + $18].Double
    FLD     ST(0)
    FADD    ST(0),ST(2)
    FSTP    [FreqDomain].Double
    FSUBP
    FSTP    [FreqDomain + $10].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTTwo64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Double;
  TD  : PDAV4DoubleArray absolute TimeDomain;
  FD  : PDAV4DoubleArray absolute FreqDomain;
begin
  FD[1]  := TD[0] - TD[2];
  FD[3]  := TD[1] - TD[3];
  Tmp[0] := TD[0] + TD[2];
  Tmp[1] := TD[1] + TD[3];
  FD[0]  := Tmp[0] + Tmp[1];
  FD[2]  := Tmp[0] - Tmp[1];
end;
{$ELSE}
asm
    FLD     [TimeDomain      ].Double
    FSUB    [TimeDomain + $10].Double
    FSTP    [FreqDomain +  $8].Double
    FLD     [TimeDomain +  $8].Double
    FSUB    [TimeDomain + $18].Double
    FSTP    [FreqDomain + $18].Double
    FLD     [TimeDomain      ].Double
    FADD    [TimeDomain + $10].Double
    FLD     [TimeDomain +  $8].Double
    FADD    [TimeDomain + $18].Double
    FLD     ST(0)
    FADD    ST(0),ST(2)
    FSTP    [FreqDomain].Double
    FSUBP
    FSTP    [FreqDomain + $10].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTThree64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
var
  TD  : PDAV8DoubleArray absolute TimeDomain;
  FD  : PDAV4ComplexDoubleArray absolute FreqDomain;
  Bf  : PDAV8DoubleArray;
begin
  Bf := PDAV8DoubleArray(FBuffer);
  Bf[1]    := TD[0] - TD[4];
  Bf[3]    := TD[2] - TD[6];
  Bf[2]    := TD[0] + TD[4];
  Bf[6]    := TD[2] + TD[6];
  Bf[0]    := Bf[2] + Bf[6];
  FD[2].Re := Bf[2] - Bf[6];
  Bf[5]    := TD[1] - TD[5];
  Bf[7]    := TD[3] - TD[7];
  Bf[2]    := TD[1] + TD[5];
  Bf[6]    := TD[3] + TD[7];
  Bf[4]    := Bf[2] + Bf[6];
  FD[2].Im := Bf[2] - Bf[6];
  FD[0].Re := Bf[0] + Bf[4];
  FD[0].Im := Bf[0] - Bf[4];
  Bf[2]    := (Bf[5] - Bf[7]) * CSQRT2Div2;
  Bf[6]    := (Bf[5] + Bf[7]) * CSQRT2Div2;
  FD[1].Re := Bf[1] + Bf[2];
  FD[3].Re := Bf[1] - Bf[2];
  FD[1].Im := Bf[6] + Bf[3];
  FD[3].Im := Bf[6] - Bf[3];
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTThree64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  TD  : PDAV8DoubleArray absolute TimeDomain;
  FD  : PDAV8DoubleArray absolute FreqDomain;
  Bf  : PDAV8DoubleArray;
begin
  Bf := PDAV8DoubleArray(FBuffer);
  Bf[0] := TD[0] - TD[4];
  Bf[1] := TD[0] + TD[4];
  Bf[2] := TD[2] - TD[6];
  Bf[3] := TD[2] + TD[6];
  Bf[4] := Bf[1] + Bf[3];
  FD[2] := Bf[1] - Bf[3];
  Bf[5] := TD[1] - TD[5];
  Bf[6] := TD[3] - TD[7];
  Bf[1] := TD[1] + TD[5];
  Bf[3] := TD[3] + TD[7];
  Bf[7] := Bf[1] + Bf[3];
  FD[6] := Bf[1] - Bf[3];
  FD[0] := Bf[4] + Bf[7];
  FD[4] := Bf[4] - Bf[7];
  Bf[1] := (Bf[5] - Bf[6]) * CSQRT2Div2;
  Bf[3] := (Bf[5] + Bf[6]) * CSQRT2Div2;
  FD[1] := Bf[0] + Bf[1];
  FD[3] := Bf[0] - Bf[1];
  FD[5] := Bf[3] + Bf[2];
  FD[7] := Bf[3] - Bf[2];
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTFour64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  ci        : Integer;
  NbrCoef   : Integer;
  NbrCoefH  : Integer;
  BitPos    : array [0..1] of Integer;
  c, s, v   : Double;
begin
  // first and second pass at once
  ci := fFFTSize;
  repeat
    BitPos[0] := FBitRevLUT.LUT[ci - 4];
    BitPos[1] := FBitRevLUT.LUT[ci - 3];
    FreqDomain[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 2];
    BitPos[1] := FBitRevLUT.LUT[ci - 1];
    FreqDomain[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FreqDomain[ci - 4] := s + c;
    FreqDomain[ci - 2] := s - c;

    Dec(ci, 4);
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    FBuffer^[ci    ] := FreqDomain[ci] + FreqDomain[ci + 4];
    FBuffer^[ci + 4] := FreqDomain[ci] - FreqDomain[ci + 4];
    FBuffer^[ci + 2] := FreqDomain[ci + 2];
    FBuffer^[ci + 6] := FreqDomain[ci + 6];

    v := (FreqDomain[ci + 5] - FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 1] := FreqDomain[ci + 1] + v;
    FBuffer^[ci + 3] := FreqDomain[ci + 1] - v;

    v := (FreqDomain[ci + 5] + FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 5] := v + FreqDomain[ci + 3];
    FBuffer^[ci + 7] := v - FreqDomain[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;
  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // Extreme coefficients are always real
    FreqDomain[0                 ] := (FBuffer^[0] + FBuffer^[NbrCoef]) * FScaleFactor;
    FreqDomain[NbrCoef           ] := (FBuffer^[0] - FBuffer^[NbrCoef]) * FScaleFactor;
    FreqDomain[          NbrCoefH] := FBuffer^[          NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoef + NbrCoefH] := FBuffer^[NbrCoef + NbrCoefH] * FScaleFactor;

    // Others are conjugate complex numbers
    for ci := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + ci];
      s := TrigoLUT[NbrCoef  - 4 - ci];

      v := FBuffer^[NbrCoef + ci] * c - FBuffer^[NbrCoef + NbrCoefH + ci] * s;
      FreqDomain[          ci] := (FBuffer^[ci] + v) * FScaleFactor;
      FreqDomain[NbrCoef - ci] := (FBuffer^[ci] - v) * FScaleFactor;

      v := FBuffer^[NbrCoef + ci] * s + FBuffer^[NbrCoef + NbrCoefH + ci] * c;
      FreqDomain[    NbrCoef + ci] := (v + FBuffer^[NbrCoefH + ci]) * FScaleFactor;
      FreqDomain[2 * NbrCoef - ci] := (v - FBuffer^[NbrCoefH + ci]) * FScaleFactor;
     end;
   end
  else
   begin
    // Extreme coefficients are always real
    FreqDomain[0                 ] := FBuffer^[0] + FBuffer^[NbrCoef];
    FreqDomain[NbrCoef           ] := FBuffer^[0] - FBuffer^[NbrCoef];
    FreqDomain[          NbrCoefH] := FBuffer^[          NbrCoefH];
    FreqDomain[NbrCoef + NbrCoefH] := FBuffer^[NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for ci := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + ci];
      s := TrigoLUT[NbrCoef  - 4 - ci];

      v := FBuffer^[NbrCoef + ci] * c - FBuffer^[NbrCoef + NbrCoefH + ci] * s;
      FreqDomain[          ci] := FBuffer^[ci] + v;
      FreqDomain[NbrCoef - ci] := FBuffer^[ci] - v;

      v := FBuffer^[NbrCoef + ci] * s + FBuffer^[NbrCoef + NbrCoefH + ci] * c;
      FreqDomain[    NbrCoef + ci] := v + FBuffer^[NbrCoefH + ci];
      FreqDomain[2 * NbrCoef - ci] := v - FBuffer^[NbrCoefH + ci];
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTFour64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  ci         : Integer;
  NbrCoef    : Integer;
  NbrCoefH   : Integer;
  c, s, v    : Double;
  BitPos     : array [0..1] of Integer;
  TempBuffer : PDAVDoubleFixedArray;
begin
  TempBuffer := @FreqDomain[0];

  // first and second pass at once
  ci := fFFTSize;
  repeat
    Dec(ci, 4);

    BitPos[0] := FBitRevLUT.LUT[ci    ];
    BitPos[1] := FBitRevLUT.LUT[ci + 1];
    TempBuffer[ci + 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci + 2];
    BitPos[1] := FBitRevLUT.LUT[ci + 3];
    TempBuffer[ci + 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    TempBuffer[ci    ] := s + c;
    TempBuffer[ci + 2] := s - c;
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    FBuffer^[ci    ] := TempBuffer[ci] + TempBuffer[ci + 4];
    FBuffer^[ci + 4] := TempBuffer[ci] - TempBuffer[ci + 4];
    FBuffer^[ci + 2] := TempBuffer[ci + 2];
    FBuffer^[ci + 6] := TempBuffer[ci + 6];

    v := (TempBuffer[ci + 5] - TempBuffer[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 1] := TempBuffer[ci + 1] + v;
    FBuffer^[ci + 3] := TempBuffer[ci + 1] - v;

    v := (TempBuffer[ci + 5] + TempBuffer[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 5] := v + TempBuffer[ci + 3];
    FBuffer^[ci + 7] := v - TempBuffer[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // last pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := (FBuffer^[0] + FBuffer^[NbrCoef]) * FScaleFactor;
    FreqDomain[0].Im := (FBuffer^[0] - FBuffer^[NbrCoef]) * FScaleFactor;

    FreqDomain[NbrCoefH].Re := FBuffer^[NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoefH].Im := FBuffer^[NbrCoef + NbrCoefH] * FScaleFactor;

    // Others are conjugate complex numbers
    for ci := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + ci];
      s := TrigoLUT[NbrCoef  - 4 - ci];

      v := FBuffer^[NbrCoef + ci] * c - FBuffer^[NbrCoef + NbrCoefH + ci] * s;
      FreqDomain[ci          ].Re := (FBuffer^[ci] + v) * FScaleFactor;
      FreqDomain[NbrCoef - ci].Re := (FBuffer^[ci] - v) * FScaleFactor;

      v := FBuffer^[NbrCoef + NbrCoefH + ci] * c + FBuffer^[NbrCoef + ci] * s;
      FreqDomain[ci          ].Im := (v + FBuffer^[NbrCoefH + ci]) * FScaleFactor;
      FreqDomain[NbrCoef - ci].Im := (v - FBuffer^[NbrCoefH + ci]) * FScaleFactor;
     end;
   end
  else 
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := FBuffer^[0] + FBuffer^[NbrCoef];
    FreqDomain[0].Im := FBuffer^[0] - FBuffer^[NbrCoef];

    FreqDomain[NbrCoefH].Re := FBuffer^[NbrCoefH];
    FreqDomain[NbrCoefH].Im := FBuffer^[NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for ci := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + ci];
      s := TrigoLUT[NbrCoef  - 4 - ci];

      v := FBuffer^[NbrCoef + ci] * c - FBuffer^[NbrCoef + NbrCoefH + ci] * s;
      FreqDomain[ci          ].Re := FBuffer^[ci] + v;
      FreqDomain[NbrCoef - ci].Re := FBuffer^[ci] - v;

      v := FBuffer^[NbrCoef + NbrCoefH + ci] * c + FBuffer^[NbrCoef + ci] * s;
      FreqDomain[ci          ].Im := v + FBuffer^[NbrCoefH + ci];
      FreqDomain[NbrCoef - ci].Im := v - FBuffer^[NbrCoefH + ci];
     end;
   end;
end;







procedure TFftReal2ComplexNativeFloat64.PerformFFTOdd64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  BitPos      : array [0..1] of Integer;
  c, s, v     : Double;
  TempBuffer  : array [0..2] of PDAVDoubleFixedArray;
begin
  // first and second pass at once
  ci := fFFTSize;
  repeat
    BitPos[0] := FBitRevLUT.LUT[ci - 4];
    BitPos[1] := FBitRevLUT.LUT[ci - 3];
    FBuffer^[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 2];
    BitPos[1] := FBitRevLUT.LUT[ci - 1];
    FBuffer^[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FBuffer^[ci - 4] := s + c;
    FBuffer^[ci - 2] := s - c;

    BitPos[0] := FBitRevLUT.LUT[ci - 8];
    BitPos[1] := FBitRevLUT.LUT[ci - 7];
    FBuffer^[ci - 7] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 6];
    BitPos[1] := FBitRevLUT.LUT[ci - 5];
    FBuffer^[ci - 5] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FBuffer^[ci - 8] := s + c;
    FBuffer^[ci - 6] := s - c;

    Dec(ci, 8);
  until (ci <= 0);

  // third pass at once
  ci := 0;
  repeat
    FreqDomain[ci] := FBuffer^[ci] + FBuffer^[ci + 4];
    FreqDomain[ci + 4] := FBuffer^[ci] - FBuffer^[ci + 4];
    FreqDomain[ci + 2] := FBuffer^[ci + 2];
    FreqDomain[ci + 6] := FBuffer^[ci + 6];

    v := (FBuffer^[ci + 5] - FBuffer^[ci + 7]) * CSQRT2Div2;
    FreqDomain[ci + 1] := FBuffer^[ci + 1] + v;
    FreqDomain[ci + 3] := FBuffer^[ci + 1] - v;
    v := (FBuffer^[ci + 5] + FBuffer^[ci + 7]) * CSQRT2Div2;
    FreqDomain[ci + 5] := v + FBuffer^[ci + 3];
    FreqDomain[ci + 7] := v - FBuffer^[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  TempBuffer[0] := @FBuffer^[0];
  TempBuffer[1] := @FreqDomain[0];
  for Pass := 3 to FOrder - 2 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;
    repeat
      // extreme coefficients are always real
      TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
      TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
      TempBuffer[0][          NbrCoefH] := TempBuffer[1][          NbrCoefH];
      TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

      // others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TempBuffer[0][          i] := TempBuffer[1][i] + v;
        TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

        v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TempBuffer[0][NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
        TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
       end;

      Inc(ci,            NbrCoef * 2);
      Inc(TempBuffer[0], NbrCoef * 2);
      Inc(TempBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TempBuffer[0], fFFTSize);
    Dec(TempBuffer[1], fFFTSize);

    // prepare to the next pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  // next pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // extreme coefficients are always real
    FreqDomain[0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef] * FScaleFactor;
    FreqDomain[NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef] * FScaleFactor;
    FreqDomain[          NbrCoefH] := TempBuffer[1][          NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH] * FScaleFactor;

    // others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[          i] := (TempBuffer[1][i] + v) * FScaleFactor;
      FreqDomain[NbrCoef - i] := (TempBuffer[1][i] - v) * FScaleFactor;

      v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
      FreqDomain[NbrCoef + i]     := (v + TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
      FreqDomain[2 * NbrCoef - i] := (v - TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
     end;
   end
  else
   begin
    // extreme coefficients are always real
    FreqDomain[0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    FreqDomain[NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
    FreqDomain[          NbrCoefH] := TempBuffer[1][          NbrCoefH];
    FreqDomain[NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

    // others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[          i] := TempBuffer[1][i] + v;
      FreqDomain[NbrCoef - i] := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
      FreqDomain[NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
      FreqDomain[2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTOdd64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  BitPos      : array [0..1] of Integer;
  c, s, v     : Double;
  TempBuffer  : array [0..2] of PDAVDoubleFixedArray;
begin
  // first and second pass at once
  ci := fFFTSize;
  repeat
    BitPos[0] := FBitRevLUT.LUT[ci - 4];
    BitPos[1] := FBitRevLUT.LUT[ci - 3];
    FBuffer^[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 2];
    BitPos[1] := FBitRevLUT.LUT[ci - 1];
    FBuffer^[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FBuffer^[ci - 4] := s + c;
    FBuffer^[ci - 2] := s - c;

    BitPos[0] := FBitRevLUT.LUT[ci - 8];
    BitPos[1] := FBitRevLUT.LUT[ci - 7];
    FBuffer^[ci - 7] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 6];
    BitPos[1] := FBitRevLUT.LUT[ci - 5];
    FBuffer^[ci - 5] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FBuffer^[ci - 8] := s + c;
    FBuffer^[ci - 6] := s - c;

    dec(ci, 8);
  until (ci <= 0);

  TempBuffer[0] := @FBuffer^[0];
  TempBuffer[1] := @FreqDomain[0];

  // third pass at once
  ci := 0;
  repeat
    TempBuffer[1][ci    ] := FBuffer^[ci] + FBuffer^[ci + 4];
    TempBuffer[1][ci + 4] := FBuffer^[ci] - FBuffer^[ci + 4];
    TempBuffer[1][ci + 2] := FBuffer^[ci + 2];
    TempBuffer[1][ci + 6] := FBuffer^[ci + 6];

    v := (FBuffer^[ci + 5] - FBuffer^[ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 1] := FBuffer^[ci + 1] + v;
    TempBuffer[1][ci + 3] := FBuffer^[ci + 1] - v;
    v := (FBuffer^[ci + 5] + FBuffer^[ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 5] := v + FBuffer^[ci + 3];
    TempBuffer[1][ci + 7] := v - FBuffer^[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  for Pass := 3 to FOrder - 2 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;
    repeat
      // extreme coefficients are always real
      TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
      TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
      TempBuffer[0][          NbrCoefH] := TempBuffer[1][NbrCoefH];
      TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

      // others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TempBuffer[0][          i] := TempBuffer[1][i] + v;
        TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

        v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TempBuffer[0][NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
        TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
       end;

      Inc(ci,            NbrCoef * 2);
      Inc(TempBuffer[0], NbrCoef * 2);
      Inc(TempBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TempBuffer[0], fFFTSize);
    Dec(TempBuffer[1], fFFTSize);

    // prepare to the next pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  // last pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := (TempBuffer[1][0] + TempBuffer[1][NbrCoef]) * FScaleFactor;
    FreqDomain[0].Im := (TempBuffer[1][0] - TempBuffer[1][NbrCoef]) * FScaleFactor;

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH] * FScaleFactor;

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := (TempBuffer[1][i] + v) * FScaleFactor;
      FreqDomain[NbrCoef - i].Re := (TempBuffer[1][i] - v) * FScaleFactor;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := (v + TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
      FreqDomain[NbrCoef - i].Im := (v - TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
     end;
   end
  else
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    FreqDomain[0].Im := TempBuffer[1][0] - TempBuffer[1][NbrCoef];

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH];
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := TempBuffer[1][i] + v;
      FreqDomain[NbrCoef - i].Re := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := v + TempBuffer[1][NbrCoefH + i];
      FreqDomain[NbrCoef - i].Im := v - TempBuffer[1][NbrCoefH + i];
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTEven64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  BitPos      : array [0..1] of Integer;
  c, s, v     : Double;
  TempBuffer  : array [0..2] of PDAVDoubleFixedArray;
begin
  // first and second pass at once
  ci := fFFTSize;
  repeat
    BitPos[0] := FBitRevLUT.LUT[ci - 4];
    BitPos[1] := FBitRevLUT.LUT[ci - 3];
    FreqDomain[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci - 2];
    BitPos[1] := FBitRevLUT.LUT[ci - 1];
    FreqDomain[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FreqDomain[ci - 4] := s + c;
    FreqDomain[ci - 2] := s - c;

    Dec(ci, 4);
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    FBuffer^[ci    ] := FreqDomain[ci] + FreqDomain[ci + 4];
    FBuffer^[ci + 4] := FreqDomain[ci] - FreqDomain[ci + 4];
    FBuffer^[ci + 2] := FreqDomain[ci + 2];
    FBuffer^[ci + 6] := FreqDomain[ci + 6];

    v := (FreqDomain[ci + 5] - FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 1] := FreqDomain[ci + 1] + v;
    FBuffer^[ci + 3] := FreqDomain[ci + 1] - v;

    v := (FreqDomain[ci + 5] + FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer^[ci + 5] := v + FreqDomain[ci + 3];
    FBuffer^[ci + 7] := v - FreqDomain[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  TempBuffer[0] := @FreqDomain[0];
  TempBuffer[1] := @FBuffer^[0];

  for Pass := 3 to FOrder - 1 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;

    repeat
      // Extreme coefficients are always real
      TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
      TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
      TempBuffer[0][          NbrCoefH] := TempBuffer[1][          NbrCoefH];
      TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

      // Others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TempBuffer[0][          i] := TempBuffer[1][i] + v;
        TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

        v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TempBuffer[0][    NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
        TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
       end;

      Inc(ci, NbrCoef * 2);
      Inc(TempBuffer[0], NbrCoef * 2);
      Inc(TempBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TempBuffer[0], fFFTSize);
    Dec(TempBuffer[1], fFFTSize);

    // Prepare to the next Pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;
  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // Extreme coefficients are always real
    FreqDomain[0                 ] := (FBuffer^[0] + FBuffer^[NbrCoef]) * FScaleFactor;
    FreqDomain[NbrCoef           ] := (FBuffer^[0] - FBuffer^[NbrCoef]) * FScaleFactor;
    FreqDomain[          NbrCoefH] := FBuffer^[          NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoef + NbrCoefH] := FBuffer^[NbrCoef + NbrCoefH] * FScaleFactor;

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := FBuffer^[NbrCoef + i] * c - FBuffer^[NbrCoef + NbrCoefH + i] * s;
      FreqDomain[          i] := (FBuffer^[i] + v) * FScaleFactor;
      FreqDomain[NbrCoef - i] := (FBuffer^[i] - v) * FScaleFactor;

      v := FBuffer^[NbrCoef + i] * s + FBuffer^[NbrCoef + NbrCoefH + i] * c;
      FreqDomain[    NbrCoef + i] := (v + FBuffer^[NbrCoefH + i]) * FScaleFactor;
      FreqDomain[2 * NbrCoef - i] := (v - FBuffer^[NbrCoefH + i]) * FScaleFactor;
     end;
   end
  else
   begin
    // Extreme coefficients are always real
    FreqDomain[0                 ] := FBuffer^[0] + FBuffer^[NbrCoef];
    FreqDomain[NbrCoef           ] := FBuffer^[0] - FBuffer^[NbrCoef];
    FreqDomain[          NbrCoefH] := FBuffer^[          NbrCoefH];
    FreqDomain[NbrCoef + NbrCoefH] := FBuffer^[NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := FBuffer^[NbrCoef + i] * c - FBuffer^[NbrCoef + NbrCoefH + i] * s;
      FreqDomain[          i] := FBuffer^[i] + v;
      FreqDomain[NbrCoef - i] := FBuffer^[i] - v;

      v := FBuffer^[NbrCoef + i] * s + FBuffer^[NbrCoef + NbrCoefH + i] * c;
      FreqDomain[    NbrCoef + i] := v + FBuffer^[NbrCoefH + i];
      FreqDomain[2 * NbrCoef - i] := v - FBuffer^[NbrCoefH + i];
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTEven64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  NbrCoefD    : Integer;
  c, s, v     : Double;
  BitPos      : array [0..1] of Integer;
  TempBuffer  : array [0..2] of PDAVDoubleFixedArray;
begin
  TempBuffer[0] := @FreqDomain[0];
  TempBuffer[1] := @FBuffer^[0];

  // first and second pass at once
  ci := fFFTSize;
  repeat
    Dec(ci, 4);

    BitPos[0] := FBitRevLUT.LUT[ci    ];
    BitPos[1] := FBitRevLUT.LUT[ci + 1];
    TempBuffer[0][ci + 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := FBitRevLUT.LUT[ci + 2];
    BitPos[1] := FBitRevLUT.LUT[ci + 3];
    TempBuffer[0][ci + 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    TempBuffer[0][ci    ] := s + c;
    TempBuffer[0][ci + 2] := s - c;
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2];
    TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6];

    v := (TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + v;
    TempBuffer[1][ci + 3] := TempBuffer[0][ci + 1] - v;

    v := (TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7]) * CSQRT2Div2;
    TempBuffer[1][ci + 5] := v + TempBuffer[0][ci + 3];
    TempBuffer[1][ci + 7] := v - TempBuffer[0][ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  for Pass := 3 to FOrder - 2 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;
    ci := 0;

    repeat
      // Extreme coefficients are always real
      TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
      TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
      TempBuffer[0][          NbrCoefH] := TempBuffer[1][          NbrCoefH];
      TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

      // Others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TempBuffer[0][         + i] := TempBuffer[1][i] + v;
        TempBuffer[0][NbrCoef  - i] := TempBuffer[1][i] - v;

        v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
        TempBuffer[0][NbrCoef  + i] := v + TempBuffer[1][NbrCoefH + i];
        TempBuffer[0][NbrCoefD - i] := v - TempBuffer[1][NbrCoefH + i];
       end;

      Inc(ci, NbrCoef * 2);
      Inc(TempBuffer[0], NbrCoef * 2);
      Inc(TempBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TempBuffer[0], fFFTSize);
    Dec(TempBuffer[1], fFFTSize);

    // Prepare to the next Pass
    TempBuffer[2] := TempBuffer[0];
    TempBuffer[0] := TempBuffer[1];
    TempBuffer[1] := TempBuffer[2];
   end;

  // last pass
  NbrCoef := 1 shl (FOrder - 1);
  NbrCoefH := NbrCoef shr 1;

  if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := (TempBuffer[1][0] + TempBuffer[1][NbrCoef]) * FScaleFactor;
    FreqDomain[0].Im := (TempBuffer[1][0] - TempBuffer[1][NbrCoef]) * FScaleFactor;

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH] * FScaleFactor;
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH] * FScaleFactor;

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := (TempBuffer[1][i] + v) * FScaleFactor;
      FreqDomain[NbrCoef - i].Re := (TempBuffer[1][i] - v) * FScaleFactor;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := (v + TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
      FreqDomain[NbrCoef - i].Im := (v - TempBuffer[1][NbrCoefH + i]) * FScaleFactor;
     end;
   end
  else 
   begin
    // Extreme coefficients are always real
    FreqDomain[0].Re := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    FreqDomain[0].Im := TempBuffer[1][0] - TempBuffer[1][NbrCoef];

    FreqDomain[NbrCoefH].Re := TempBuffer[1][NbrCoefH];
    FreqDomain[NbrCoefH].Im := TempBuffer[1][NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      FreqDomain[i          ].Re := TempBuffer[1][i] + v;
      FreqDomain[NbrCoef - i].Re := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + NbrCoefH + i] * c + TempBuffer[1][NbrCoef + i] * s;
      FreqDomain[i          ].Im := v + TempBuffer[1][NbrCoefH + i];
      FreqDomain[NbrCoef - i].Im := v - TempBuffer[1][NbrCoefH + i];
     end;
   end;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// IFFT ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TFftReal2ComplexNativeFloat64.PerformIFFTZero64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
begin
 TimeDomain^[0] := FreqDomain^[0];
end;
{$ELSE}
asm
    FLD     [FreqDomain].Double
    FSTP    [TimeDomain].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTZero64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
begin
 TimeDomain^[0] := FreqDomain^[0].Re;
end;
{$ELSE}
asm
    FLD     [FreqDomain].Double
    FSTP     [TimeDomain].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTOne64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  FD : PComplex64 absolute FreqDomain;
  TD : PDAV2DoubleArray absolute TimeDomain;
begin
 TD[0] := FD.Re + FD.Im;
 TD[1] := FD.Re - FD.Im;
end;
{$ELSE}
asm
    FLD     [FreqDomain].Double
    FLD     ST(0)
    FADD    [FreqDomain + 8].Double
    FSTP    [TimeDomain].Double
    FSUB    [FreqDomain + 8].Double
    FSTP    [TimeDomain + 8].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTOne64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  FD : PDAV2DoubleArray absolute FreqDomain;
  TD : PDAV2DoubleArray absolute TimeDomain;
begin
 TD[0] := FD[0] + FD[1];
 TD[1] := FD[0] - FD[1];
end;
{$ELSE}
asm
    FLD     [FreqDomain].Double
    FLD     ST(0)
    FADD    [FreqDomain + 8].Double
    FSTP    [TimeDomain].Double
    FSUB    [FreqDomain + 8].Double
    FSTP    [TimeDomain + 8].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTTwo64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Double;
  FD  : PDAV2ComplexDoubleArray absolute FreqDomain;
  TD  : PDAV4DoubleArray absolute TimeDomain;
begin
 Tmp[0] := FD[0].Re + FD[0].Im;
 Tmp[1] := FD[0].Re - FD[0].Im;

 TD[1] := Tmp[1] + FD[1].Im * 2;
 TD[3] := Tmp[1] - FD[1].Im * 2;
 TD[0] := Tmp[0] + FD[1].Re * 2;
 TD[2] := Tmp[0] - FD[1].Re * 2;
end;
{$ELSE}
const
  c2 : Double = 2 ;
asm
    FLD     [FreqDomain + $10].Double
    FLD     [FreqDomain].Double
    FLD     ST(0)
    FADD    ST(0), ST(2)
    FXCH    ST(2)
    FSUBP
    FLD     [FreqDomain + $18].Double
    FMUL    c2
    FLD     ST(0)
    FADD    ST(0), ST(2)
    FSTP    [TimeDomain + $8].Double
    FSUBP
    FSTP    [TimeDomain + $18].Double
    FLD     [FreqDomain + $8].Double
    FMUL    c2
    FLD     ST(0)
    FADD    ST(0), ST(2)
    FSTP    [TimeDomain].Double
    FSUBP
    FSTP    [TimeDomain + $10].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTTwo64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Double;
  FD  : PDAV4DoubleArray absolute FreqDomain;
  TD  : PDAV4DoubleArray absolute TimeDomain;
begin
 Tmp[0] := FD[0] + FD[2];
 Tmp[1] := FD[0] - FD[2];

 TD[1] := Tmp[1] + FD[3] * 2;
 TD[3] := Tmp[1] - FD[3] * 2;
 TD[0] := Tmp[0] + FD[1] * 2;
 TD[2] := Tmp[0] - FD[1] * 2;
end;
{$ELSE}
const
  c2 : Double = 2 ;
asm
  FLD     [FreqDomain + $10].Double
  FLD     [FreqDomain].Double
  FLD     ST(0)
  FADD    ST(0), ST(2)
  FXCH    ST(2)
  FSUBP
  FLD     [FreqDomain + $18].Double
  FMUL    c2
  FLD     ST(0)
  FADD    ST(0), ST(2)
  FSTP    [TimeDomain +  $8].Double
  FSUBP
  FSTP    [TimeDomain + $18].Double
  FLD     [FreqDomain +  $8].Double
  FMUL    c2
  FLD     ST(0)
  FADD    ST(0), ST(2)
  FSTP    [TimeDomain].Double
  FSUBP
  FSTP    [TimeDomain + $10].Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTThree64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
var
  FD  : PDAV4ComplexDoubleArray absolute FreqDomain;
  TD  : PDAV8DoubleArray absolute TimeDomain;
  Bf  : PDAV8DoubleArray;
begin
  Bf := PDAV8DoubleArray(FBuffer);
  Bf[0]  := FD[0].Re + FD[0].Im;
  Bf[4]  := FD[0].Re - FD[0].Im;
  Bf[1]  := FD[1].Re + FD[3].Re;
  Bf[3]  := FD[1].Im - FD[3].Im;
  Bf[2]  := FD[1].Re - FD[3].Re;
  Bf[6]  := FD[1].Im + FD[3].Im;
  Bf[5]  := (Bf[2] + Bf[6]) * CSQRT2Div2;
  Bf[7]  := (Bf[6] - Bf[2]) * CSQRT2Div2;
  Bf[2]  := Bf[0] + FD[2].Re * 2;
  Bf[0]  := Bf[0] - FD[2].Re * 2;
  Bf[6]  := Bf[1] * 2;
  Bf[3]  := Bf[3] * 2;
  TD[0]  := Bf[2] + Bf[6];
  TD[4]  := Bf[2] - Bf[6];
  TD[2]  := Bf[0] + Bf[3];
  TD[6]  := Bf[0] - Bf[3];
  Bf[2]  := Bf[4] + FD[2].Im * 2;
  Bf[4]  := Bf[4] - FD[2].Im * 2;
  Bf[6]  := Bf[5] * 2;
  Bf[7]  := Bf[7] * 2;
  TD[1]  := Bf[2] + Bf[6];
  TD[5]  := Bf[2] - Bf[6];
  TD[3]  := Bf[4] + Bf[7];
  TD[7]  := Bf[4] - Bf[7];
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTThree64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  FD  : PDAV8DoubleArray absolute FreqDomain;
  TD  : PDAV8DoubleArray absolute TimeDomain;
  Bf  : PDAV8DoubleArray;
begin
  Bf := PDAV8DoubleArray(FBuffer);
  Bf[0] := FD[0] + FD[4];
  Bf[4] := FD[0] - FD[4];
  Bf[1] := FD[1] + FD[3];
  Bf[3] := FD[5] - FD[7];
  Bf[2] := FD[1] - FD[3];
  Bf[6] := FD[5] + FD[7];
  Bf[5] := (Bf[2] + Bf[6]) * CSQRT2Div2;
  Bf[7] := (Bf[6] - Bf[2]) * CSQRT2Div2;
  Bf[2] := Bf[0] + FD[2] * 2;
  Bf[0] := Bf[0] - FD[2] * 2;
  Bf[6] := Bf[1] * 2;
  Bf[3] := Bf[3] * 2;
  TD[0] := Bf[2] + Bf[6];
  TD[4] := Bf[2] - Bf[6];
  TD[2] := Bf[0] + Bf[3];
  TD[6] := Bf[0] - Bf[3];
  Bf[2] := Bf[4] + FD[6] * 2;
  Bf[4] := Bf[4] - FD[6] * 2;
  Bf[6] := Bf[5] * 2;
  Bf[7] := Bf[7] * 2;
  TD[1] := Bf[2] + Bf[6];
  TD[5] := Bf[2] - Bf[6];
  TD[3] := Bf[4] + Bf[7];
  TD[7] := Bf[4] - Bf[7];
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTEven64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Pass          : Integer;
  NbrCoef       : Integer;
  NbrCoefH      : Integer;
  NbrCoefD      : Integer;
  tof, i, ci    : Integer;
  c, s, vr, vi  : Double;
  Tmp           : array [0..3] of Double;
  TempBuffer    : array [0..2] of PDAVDoubleFixedArray;
begin
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @TimeDomain[0];

 // Do the transformation in several Pass

 // First Pass
 for Pass := FOrder - 1 downto 3 do
  begin
   ci := 0;
   NbrCoef := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   NbrCoefD := NbrCoef shl 1;

   tof := NbrCoefH - 4;

   repeat
    // Extreme coefficients are always real
    TempBuffer[1][ci           ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci            + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
    TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

    // Others are conjugate complex numbers

    for i := 1 to NbrCoefH-1 do
     begin
      TempBuffer[1][ci + i             ] := TempBuffer[0][ci            + i] + TempBuffer[0][ci + NbrCoef            - i];    // + sfr [NbrCoef - i]
      TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

      c := TrigoLUT[tof           +i]; // cos (i*PI/NbrCoef);
      s := TrigoLUT[tof+NbrCoefH-i]; // sin (i*PI/NbrCoef);

      vr := TempBuffer[0][ci+i] - TempBuffer[0][ci + NbrCoef-i];    // - sfr [NbrCoef - i]
      vi := TempBuffer[0][ci + NbrCoef+i] + TempBuffer[0][ci + NbrCoef+NbrCoef - i];

      TempBuffer[1][ci + NbrCoef+i] := vr * c + vi * s;
      TempBuffer[1][ci + NbrCoef + NbrCoefH+i] := vi * c - vr * s;
     end;

    Inc(ci, NbrCoefD);
   until (ci >= fFFTSize);

   // Prepare to the next Pass
   if (Pass < FOrder - 1) then
    begin
     TempBuffer[2] := TempBuffer[0];
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := TempBuffer[2];
    end
   else
    begin
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := @FBuffer^[0];
    end
  end;

  // Antepenultimate Pass
  ci := 0;
  repeat
   TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
   TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
   TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2] * 2;
   TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6] * 2;

   TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + TempBuffer[0][ci + 3];
   TempBuffer[1][ci + 3] := TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7];

   vr := TempBuffer[0][ci + 1] - TempBuffer[0][ci + 3];
   vi := TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7];

   TempBuffer[1][ci + 5] := (vr + vi) * CSQRT2Div2;
   TempBuffer[1][ci + 7] := (vi - vr) * CSQRT2Div2;

   Inc(ci, 8);
  until (ci >= fFFTSize);


  // Penultimate and last Pass at once
  ci := 0;
  repeat
   Tmp[0] := TempBuffer[1][ci    ] + TempBuffer[1][ci + 2];
   Tmp[2] := TempBuffer[1][ci    ] - TempBuffer[1][ci + 2];
   Tmp[1] := TempBuffer[1][ci + 1] * 2;
   Tmp[3] := TempBuffer[1][ci + 3] * 2;

   TimeDomain[FBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
   TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

   Tmp[0] := TempBuffer[1][ci + 4] + TempBuffer[1][ci + 6];
   Tmp[2] := TempBuffer[1][ci + 4] - TempBuffer[1][ci + 6];
   Tmp[1] := TempBuffer[1][ci + 5] * 2;
   Tmp[3] := TempBuffer[1][ci + 7] * 2;

   TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
   TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

   Inc(ci, 8);
 until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTEven64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  Pass          : Integer;
  NbrCoef       : Integer;
  NbrCoefH      : Integer;
  NbrCoefD      : Integer;
  tof, i, ci    : Integer;
  c, s, vr, vi  : Double;
  Tmp           : array [0..3] of Double;
  TempBuffer    : array [0..2] of PDAVDoubleFixedArray;
begin
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @TimeDomain[0];

 // Do the transformation in several Pass

 // First Pass
 for Pass := FOrder - 1 downto 3 do
  begin
   ci := 0;
   NbrCoef := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   NbrCoefD := NbrCoef shl 1;

   tof := NbrCoefH - 4;

   repeat
    // Extreme coefficients are always real
    TempBuffer[1][ci          ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci           + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
    TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

    // Others are conjugate complex numbers

    for i := 1 to NbrCoefH-1 do
     begin
      TempBuffer[1][ci + i           ] := TempBuffer[0][ci           + i] + TempBuffer[0][ci + NbrCoef           - i];    // + sfr [NbrCoef - i]
      TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

      c := TrigoLUT[tof            + i]; // cos (i * PI / NbrCoef);
      s := TrigoLUT[tof + NbrCoefH - i]; // sin (i * PI / NbrCoef);

      vr := TempBuffer[0][ci           + i] - TempBuffer[0][ci + NbrCoef           - i];
      vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef + NbrCoef - i];

      TempBuffer[1][ci + NbrCoef+i] := vr * c + vi * s;
      TempBuffer[1][ci + NbrCoef + NbrCoefH+i] := vi * c - vr * s;
     end;

    Inc(ci, NbrCoefD);
   until (ci >= fFFTSize);

   // Prepare to the next Pass
   if (Pass < FOrder - 1) then
    begin
     TempBuffer[2] := TempBuffer[0];
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := TempBuffer[2];
    end
   else
    begin
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := @FBuffer^[0];
    end
  end;

  // Antepenultimate Pass
  ci := 0;
  repeat
   TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
   TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
   TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2] * 2;
   TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6] * 2;

   TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + TempBuffer[0][ci + 3];
   TempBuffer[1][ci + 3] := TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7];

   vr := TempBuffer[0][ci + 1] - TempBuffer[0][ci + 3];
   vi := TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7];

   TempBuffer[1][ci + 5] := (vr + vi) * CSQRT2Div2;
   TempBuffer[1][ci + 7] := (vi - vr) * CSQRT2Div2;

   Inc(ci, 8);
  until (ci >= fFFTSize);


  // Penultimate and last Pass at once
  ci := 0;
  repeat
   Tmp[0] := TempBuffer[1][ci    ] + TempBuffer[1][ci + 2];
   Tmp[2] := TempBuffer[1][ci    ] - TempBuffer[1][ci + 2];
   Tmp[1] := TempBuffer[1][ci + 1] * 2;
   Tmp[3] := TempBuffer[1][ci + 3] * 2;

   TimeDomain[FBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
   TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

   Tmp[0] := TempBuffer[1][ci + 4] + TempBuffer[1][ci + 6];
   Tmp[2] := TempBuffer[1][ci + 4] - TempBuffer[1][ci + 6];
   Tmp[1] := TempBuffer[1][ci + 5] * 2;
   Tmp[3] := TempBuffer[1][ci + 7] * 2;

   TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
   TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

   Inc(ci, 8);
 until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTOdd64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  Pass          : Integer;
  NbrCoef       : Integer;
  NbrCoefH      : Integer;
  NbrCoefD      : Integer;
  tof, i, ci    : Integer;
  c, s, vr, vi  : Double;
  Tmp           : array [0..3] of Double;
  TempBuffer    : array [0..2] of PDAVDoubleFixedArray;
begin
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @FBuffer^[0];

 // do the transformation in several pass

 // first pass
 for Pass := FOrder - 1 downto 3 do
  begin
   ci := 0;
   NbrCoef  := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   NbrCoefD := NbrCoef shl 1;

   tof := NbrCoefH - 4;

   repeat
    // Extreme coefficients are always real
    TempBuffer[1][ci          ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci           + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
    TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

    // Others are conjugate complex numbers

    for i := 1 to NbrCoefH - 1 do
     begin
      TempBuffer[1][ci + i] := TempBuffer[0][ci + i] + TempBuffer[0][ci + NbrCoef - i];    // + sfr [NbrCoef - i]
      TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

      c := TrigoLUT[tof            + i]; // cos (i*PI/NbrCoef);
      s := TrigoLUT[tof + NbrCoefH - i]; // sin (i*PI/NbrCoef);

      vr := TempBuffer[0][ci + i] - TempBuffer[0][ci + NbrCoef - i];   
      vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef+NbrCoef - i];

      TempBuffer[1][ci + NbrCoef + i] := vr * c + vi * s;
      TempBuffer[1][ci + NbrCoef + NbrCoefH + i] := vi * c - vr * s;
     end;

    Inc(ci, NbrCoefD);
   until (ci >= fFFTSize);

   // Prepare to the next Pass
   if (Pass < FOrder - 1) then
    begin
     TempBuffer[2] := TempBuffer[0];
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := TempBuffer[2];
    end
   else
    begin
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := @TimeDomain[0];
    end
  end;

  // Antepenultimate Pass
  ci := 0;

  repeat
   FBuffer^[ci    ] := TimeDomain[ci    ] + TimeDomain[ci + 4];
   FBuffer^[ci + 4] := TimeDomain[ci    ] - TimeDomain[ci + 4];
   FBuffer^[ci + 2] := TimeDomain[ci + 2] * 2;
   FBuffer^[ci + 6] := TimeDomain[ci + 6] * 2;

   FBuffer^[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
   FBuffer^[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

   vr := TimeDomain[ci + 1] - TimeDomain[ci + 3];
   vi := TimeDomain[ci + 5] + TimeDomain[ci + 7];

   FBuffer^[ci + 5] := (vr + vi) * CSQRT2Div2;
   FBuffer^[ci + 7] := (vi - vr) * CSQRT2Div2;

   Inc(ci, 8);
  until (ci >= fFFTSize);

  // Penultimate and last Pass at once
  ci := 0;

  repeat
   Tmp[0] := FBuffer^[ci    ] + FBuffer^[ci+2];
   Tmp[2] := FBuffer^[ci    ] - FBuffer^[ci+2];
   Tmp[1] := FBuffer^[ci + 1] * 2;
   Tmp[3] := FBuffer^[ci + 3] * 2;

   TimeDomain[FBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
   TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

   Tmp[0] := FBuffer^[ci + 4] + FBuffer^[ci + 6];
   Tmp[2] := FBuffer^[ci + 4] - FBuffer^[ci + 6];
   Tmp[1] := FBuffer^[ci + 5] * 2;
   Tmp[3] := FBuffer^[ci + 7] * 2;

   TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
   TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

   Inc(ci, 8);
 until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTOdd64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Pass          : Integer;
  NbrCoef       : Integer;
  NbrCoefH      : Integer;
  NbrCoefD      : Integer;
  tof, i, ci    : Integer;
  c, s, vr, vi  : Double;
  Tmp           : array [0..3] of Double;
  TempBuffer    : array [0..2] of PDAVDoubleFixedArray;
begin
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @FBuffer^[0];

 // Do the transformation in several Pass

 // First Pass
 for Pass := FOrder - 1 downto 3 do
  begin
   ci := 0;
   NbrCoef := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   NbrCoefD := NbrCoef shl 1;

   tof := NbrCoefH - 4;

   repeat
    // Extreme coefficients are always real
    TempBuffer[1][ci           ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci            + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
    TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

    // Others are conjugate complex numbers

    for i := 1 to NbrCoefH - 1 do
     begin
      TempBuffer[1][ci + i] := TempBuffer[0][ci + i] + TempBuffer[0][ci + NbrCoef-i];    // + sfr [NbrCoef - i]
      TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef+i] - TempBuffer[0][ci + NbrCoef+NbrCoef - i];

      c := TrigoLUT[tof            + i]; // cos (i*PI/NbrCoef);
      s := TrigoLUT[tof + NbrCoefH - i]; // sin (i*PI/NbrCoef);

      vr := TempBuffer[0][ci + i] - TempBuffer[0][ci + NbrCoef - i];    // - sfr [NbrCoef - i]
      vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef+NbrCoef - i];

      TempBuffer[1][ci + NbrCoef+i] := vr * c + vi * s;
      TempBuffer[1][ci + NbrCoef + NbrCoefH+i] := vi * c - vr * s;
     end;

    Inc(ci, NbrCoefD);
   until (ci >= fFFTSize);

   // Prepare to the next Pass
   if (Pass < FOrder - 1) then
    begin
     TempBuffer[2] := TempBuffer[0];
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := TempBuffer[2];
    end
   else
    begin
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := @TimeDomain[0];
    end
  end;

  // Antepenultimate Pass
  ci := 0;

  repeat
   FBuffer^[ci    ] := TimeDomain[ci    ] + TimeDomain[ci + 4];
   FBuffer^[ci + 4] := TimeDomain[ci    ] - TimeDomain[ci + 4];
   FBuffer^[ci + 2] := TimeDomain[ci + 2] * 2;
   FBuffer^[ci + 6] := TimeDomain[ci + 6] * 2;

   FBuffer^[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
   FBuffer^[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

   vr := TimeDomain[ci + 1] - TimeDomain[ci + 3];
   vi := TimeDomain[ci + 5] + TimeDomain[ci + 7];

   FBuffer^[ci + 5] := (vr + vi) * CSQRT2Div2;
   FBuffer^[ci + 7] := (vi - vr) * CSQRT2Div2;

   Inc(ci, 8);
  until (ci >= fFFTSize);

  // Penultimate and last Pass at once
  ci := 0;

  repeat
   Tmp[0] := FBuffer^[ci    ] + FBuffer^[ci+2];
   Tmp[2] := FBuffer^[ci    ] - FBuffer^[ci+2];
   Tmp[1] := FBuffer^[ci + 1] * 2;
   Tmp[3] := FBuffer^[ci + 3] * 2;

   TimeDomain[FBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
   TimeDomain[FBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

   Tmp[0] := FBuffer^[ci + 4] + FBuffer^[ci + 6];
   Tmp[2] := FBuffer^[ci + 4] - FBuffer^[ci + 6];
   Tmp[1] := FBuffer^[ci + 5] * 2;
   Tmp[3] := FBuffer^[ci + 7] * 2;

   TimeDomain[FBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
   TimeDomain[FBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
   TimeDomain[FBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

   Inc(ci, 8);
 until (ci >= fFFTSize);
end;

initialization
  CSQRT2Div2 := Sqrt(2) * 0.5;
  TrigoLvl := 3;
  TrigoLUT := nil;
  DoTrigoLUT(5);
  InitLUTList;

finalization
  DestroyLUTList;

end.
