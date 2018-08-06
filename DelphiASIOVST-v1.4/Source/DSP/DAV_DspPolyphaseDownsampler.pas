unit DAV_DspPolyphaseDownsampler;

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
//  The code is based on the HIIR code by Laurent de Soras, which             //
//  can be found at http://ldesoras.free.fr/prod.html#src_hiir                //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{-$UNDEF UseAlignedMemory}

{$IFDEF CPUx86_64}
  {$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Classes, DAV_Types, DAV_MemoryUtils, DAV_DspPolyphaseFilter;

type
  TProcessSample32 = function(const Output: TDAV2SingleArray): Single of object;
  TProcessSample64 = function(const Output: TDAV2DoubleArray): Double of object;
  TProcessSampleSplit32 = procedure(out Low, High: Single; Input: TDAV2SingleArray) of object;
  TProcessSampleSplit64 = procedure(out Low, High: Double; Input: TDAV2DoubleArray) of object;

  TCustomPolyphaseDownsampler = class(TCustomPolyphaseFilter)
  protected
    procedure NumberOfCoeffsChanged; override;
    procedure AllocateBuffer; virtual; abstract;
    procedure ClearBuffers; virtual; abstract;
    procedure ResetStates; virtual; abstract;
    procedure PushStates; virtual; abstract;
    procedure PopStates; virtual; abstract;
  public
    constructor Create; override;
    constructor Create(const NumberOfCoefficients: Integer;
      const Transition: Double); override;
  end;

  TPolyphaseDownsampler32 = class(TCustomPolyphaseDownsampler)
  private
    FX, FY                : PDAVSingleFixedArray;
    FStateStack           : PDAVSingleFixedArray;
    FProcessSample32      : TProcessSample32;
    FProcessSampleSplit32 : TProcessSampleSplit32;

    function ProcessSample1(const Input: TDAV2SingleArray): Single;
    function ProcessSample2(const Input: TDAV2SingleArray): Single;
    function ProcessSample3(const Input: TDAV2SingleArray): Single;
    function ProcessSample4(const Input: TDAV2SingleArray): Single;
    function ProcessSampleLarge(const Input: TDAV2SingleArray): Single;

    procedure ProcessSampleSplit1(out Low, High: Single; Input: TDAV2SingleArray);
    procedure ProcessSampleSplit2(out Low, High: Single; Input: TDAV2SingleArray);
    procedure ProcessSampleSplit3(out Low, High: Single; Input: TDAV2SingleArray);
    procedure ProcessSampleSplit4(out Low, High: Single; Input: TDAV2SingleArray);
    procedure ProcessSampleSplitLarge(out Low, High: Single; Input: TDAV2SingleArray);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure AllocateBuffer; override;
    procedure ChooseProcedures; override;
  public
    constructor Create; override;
    constructor Create(const NumberOfCoefficients: Integer;
      const Transition: Double); override;
    destructor Destroy; override;

    procedure ClearBuffers; override;
    procedure ResetStates; override;
    procedure PushStates; override;
    procedure PopStates; override;

    procedure ProcessBlock(const Input, Output: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure ProcessBlockSplit(const OutputL, OutputH, Input: PDAVSingleFixedArray; SampleFrames: Integer);
    function ProcessSample32(const Input: TDAV2SingleArray): Single;

    property ProcessSample: TProcessSample32 read FProcessSample32;
    property ProcessSampleSplit: TProcessSampleSplit32 read FProcessSampleSplit32;
  end;

  TPolyphaseDownsampler64 = class(TCustomPolyphaseDownsampler)
  private
    FX, FY                : PDAVDoubleFixedArray;
    FStateStack           : PDAVDoubleFixedArray;
    FProcessSample64      : TProcessSample64;
    FProcessSampleSplit64 : TProcessSampleSplit64;

    function ProcessSample1(const Input: TDAV2DoubleArray): Double;
    function ProcessSample2(const Input: TDAV2DoubleArray): Double;
    function ProcessSample3(const Input: TDAV2DoubleArray): Double;
    function ProcessSample4(const Input: TDAV2DoubleArray): Double;
    function ProcessSampleLarge(const Input: TDAV2DoubleArray): Double;

    procedure ProcessSampleSplit1(out Low, High: Double; Input: TDAV2DoubleArray);
    procedure ProcessSampleSplit2(out Low, High: Double; Input: TDAV2DoubleArray);
    procedure ProcessSampleSplit3(out Low, High: Double; Input: TDAV2DoubleArray);
    procedure ProcessSampleSplit4(out Low, High: Double; Input: TDAV2DoubleArray);
    procedure ProcessSampleSplitLarge(out Low, High: Double; Input: TDAV2DoubleArray);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure AllocateBuffer; override;
    procedure ChooseProcedures; override;
  public
    constructor Create; override;
    constructor Create(const NumberOfCoefficients: Integer;
      const Transition: Double); override;
    destructor Destroy; override;

    procedure ClearBuffers; override;
    procedure ResetStates; override;
    procedure PushStates; override;
    procedure PopStates; override;

    procedure ProcessBlock(const Input, Output: PDAVDoubleFixedArray; SampleFrames: Integer);
    procedure ProcessBlockSplit(const OutputL, OutputH, Input: PDAVDoubleFixedArray; SampleFrames: Integer);
    function ProcessSample64(const Input: TDAV2DoubleArray): Double;

    property ProcessSample: TProcessSample64 read FProcessSample64;
    property ProcessSampleSplit: TProcessSampleSplit64 read FProcessSampleSplit64;
  end;

implementation

const
  CHalf32: Single = 0.5;
  CHalf64: Double = 0.5;


{ TCustomPolyphaseDownsampler }

constructor TCustomPolyphaseDownsampler.Create;
begin
 inherited;
 AllocateBuffer;
 ChooseProcedures;
 ClearBuffers;
end;

constructor TCustomPolyphaseDownsampler.Create(
  const NumberOfCoefficients: Integer; const Transition: Double);
begin
 inherited;
 AllocateBuffer;
 ChooseProcedures;
 ClearBuffers;
end;

procedure TCustomPolyphaseDownsampler.NumberOfCoeffsChanged;
begin
 inherited;
 AllocateBuffer;
 ChooseProcedures;
 ClearBuffers;
end;


{ TPolyphaseDownsampler32 }

constructor TPolyphaseDownsampler32.Create;
begin
 FX := nil;
 FY := nil;
 FStateStack := nil;
 inherited;
end;

constructor TPolyphaseDownsampler32.Create(const NumberOfCoefficients: Integer;
  const Transition: Double);
begin
 FX := nil;
 FY := nil;
 FStateStack := nil;
 inherited;
end;

destructor TPolyphaseDownsampler32.Destroy;
begin
 {$IFDEF UseAlignedMemory}
 FreeAlignedMemory(FX);
 FreeAlignedMemory(FY);
 {$ELSE}
 Dispose(FX);
 Dispose(FY);
 {$ENDIF}
 Dispose(FStateStack);
 inherited;
end;

procedure TPolyphaseDownsampler32.AssignTo(Dest: TPersistent);
var
  Index : Integer;
begin
 inherited;
 if Dest is TPolyphaseDownsampler32 then
  with TPolyphaseDownsampler32(Dest) do
   begin
    inherited;
    FProcessSample32      := Self.FProcessSample32;
    FProcessSampleSplit32 := Self.FProcessSampleSplit32;

    Assert(FNumberOfCoeffs = Self.FNumberOfCoeffs);
    Move(Self.FX^, FX^, FNumberOfCoeffs * SizeOf(Single));
    Move(Self.FY^, FY^, FNumberOfCoeffs * SizeOf(Single));
    Move(Self.FStateStack^, FStateStack^, 2 * FNumberOfCoeffs * SizeOf(Single));
   end else
 if Dest is TPolyphaseDownsampler64 then
  with TPolyphaseDownsampler64(Dest) do
   begin
    inherited;
    Assert(FNumberOfCoeffs = Self.FNumberOfCoeffs);
    ChooseProcedures;

    for Index := 0 to FNumberOfCoeffs - 1 do
     begin
      FX^[Index] := Self.FX^[Index];
      FY^[Index] := Self.FY^[Index];
      FStateStack^[Index] := Self.FStateStack^[Index];
     end;
   end;
end;

procedure TPolyphaseDownsampler32.AllocateBuffer;
begin
 {$IFDEF UseAlignedMemory}
 ReallocateAlignedMemory(Pointer(FX), FNumberOfCoeffs * SizeOf(Single));
 ReallocateAlignedMemory(Pointer(FY), FNumberOfCoeffs * SizeOf(Single));
 {$ELSE}
 ReallocMem(Pointer(FX), FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(Pointer(FY), FNumberOfCoeffs * SizeOf(Single));
 {$ENDIF}
 ReallocMem(FStateStack, 2 * FNumberOfCoeffs * SizeOf(Single));
end;

procedure TPolyphaseDownsampler32.ChooseProcedures;
begin
 case FNumberOfCoeffs of
   1 :
    begin
     FProcessSample32      := ProcessSample1;
     FProcessSampleSplit32 := ProcessSampleSplit1;
    end;
   2 :
    begin
     FProcessSample32      := ProcessSample2;
     FProcessSampleSplit32 := ProcessSampleSplit2;
    end;
   3 :
    begin
     FProcessSample32      := ProcessSample3;
     FProcessSampleSplit32 := ProcessSampleSplit3;
    end;
   4 :
    begin
     FProcessSample32      := ProcessSample4;
     FProcessSampleSplit32 := ProcessSampleSplit4;
    end;
  else
   begin
    FProcessSample32      := ProcessSampleLarge;
    FProcessSampleSplit32 := ProcessSampleSplitLarge;
   end;
 end;
end;

procedure TPolyphaseDownsampler32.ClearBuffers;
begin
 FillChar(FX[0], FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FY[0], FNumberOfCoeffs * SizeOf(Single), 0);
end;

procedure TPolyphaseDownsampler32.ResetStates;
begin
 FillChar(FX[0], FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FY[0], FNumberOfCoeffs * SizeOf(Single), 0);
end;

procedure TPolyphaseDownsampler32.PushStates;
begin
  Move(FX[0], FStateStack[0], FNumberOfCoeffs * SizeOf(Single));
  Move(FY[0], FStateStack[FNumberOfCoeffs], FNumberOfCoeffs * SizeOf(Single));
end;

procedure TPolyphaseDownsampler32.PopStates;
begin
  Move(FStateStack[0], FX[0], FNumberOfCoeffs * SizeOf(Single));
  Move(FStateStack[FNumberOfCoeffs], FY[0], FNumberOfCoeffs * SizeOf(Single));
end;

function TPolyphaseDownsampler32.ProcessSample1(const Input: TDAV2SingleArray): Single;
{$IFDEF PUREPASCAL}
begin
 // calculate filter
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0];
 FX[0] := Input[1];

 // calculate result
 Result := CHalf32 * (FY[0] + Input[0]);
{$ELSE}
asm
    PUSH    EDI
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     ECX, [EAX.FX]            // ESI = FX
    MOV     EAX, [EAX.FCoefficients] // ECX = FCoefficients
    FLD     [ECX].Single             // FX[0]
    FLD     [Input + 4].Single       // Input[1], FX[0]
    FST     [ECX].Single             // FX[0] := Input[1];
    FSUB    [EDI].Single             // (Input[1] - FY[0])
    FMUL    [EAX].Double             // (Input[1] - FY[0]) * FCoefficients[0]
    FADDP                            // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
    FST     [EDI].Single
    FLD     [Input].Single           // Input[0], FY[0]
    FADDP                            // FY[1] + FY[0]
    FMUL    CHalf32                  // (FY[1] + FY[0]) * 0.5
    POP     EDI
{$ENDIF}
end;

function TPolyphaseDownsampler32.ProcessSample2(const Input: TDAV2SingleArray): Single;
{$IFDEF PUREPASCAL}
var
  LocalX : PDAV2SingleArray;
  LocalY : PDAV2SingleArray;
  Coeffs : PDAV2DoubleArray;
begin
 // initialize local variables
 LocalX := PDAV2SingleArray(FX);
 LocalY := PDAV2SingleArray(FY);
 Coeffs := PDAV2DoubleArray(FCoefficients);

 // process filter
 LocalY[0] := (Input[1] - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input[1];
 LocalY[1] := (Input[0] - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input[0];

 // calculate result
 Result := CHalf32 * (LocalY[0] + LocalY[1]);
{$ELSE}
asm
    PUSH    EDI
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     ECX, [EAX.FX]            // ESI = FX
    MOV     EAX, [EAX.FCoefficients] // ECX = FCoefficients

    FLD     [ECX].Single             // FX[0]
    FLD     [Input + 4].Single       // Input[1], FX[0]
    FST     [ECX].Single             // FX[0] := Input[1];
    FSUB    [EDI].Single             // (Input[1] - FY[0])
    FMUL    [EAX].Double             // (Input[1] - FY[0]) * FCoefficients[0]
    FADDP                            // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
    FST     [EDI].Single

    FLD     [ECX + 4].Single         // FX[1], FY[0]
    FLD     [Input].Single           // Input[0], FX[1], FY[0]
    FST     [ECX + 4].Single         // FX[1] := Input[0];
    FSUB    [EDI + 4].Single         // (Input[0] - FY[1]), FY[0]
    FMUL    [EAX + 8].Double         // (Input[0] - FY[1]) * FCoefficients[1], FY[0]
    FADDP                            // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
    FST     [EDI + 4].Single

    FADDP                            // FY[1] + FY[0]
    FMUL    CHalf32                  // (FY[1] + FY[0]) * 0.5
    POP     EDI
{$ENDIF}
end;

function TPolyphaseDownsampler32.ProcessSample3(const Input: TDAV2SingleArray): Single;
{$IFDEF PUREPASCAL}
var
  LocalX : PDAV3SingleArray;
  LocalY : PDAV3SingleArray;
  Coeffs : PDAV3DoubleArray;
begin
 // initialize local variables
 LocalX := PDAV3SingleArray(FX);
 LocalY := PDAV3SingleArray(FY);
 Coeffs := PDAV3DoubleArray(FCoefficients);

 // process filter
 LocalY[0] := (Input[1] - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input[1];
 LocalY[1] := (Input[0] - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input[0];
 LocalY[2] := (LocalY[0] - LocalY[2]) * Coeffs[2] + LocalX[2];
 LocalX[2] := LocalY[0];

 // calculate result
 Result := CHalf32 * (LocalY[1] + LocalY[2]);
{$ELSE}
asm
    PUSH    EDI
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     ECX, [EAX.FX]            // ESI = FX
    MOV     EAX, [EAX.FCoefficients] // ECX = FCoefficients

    FLD     [ECX].Single             // FX[0]
    FLD     [Input + 4].Single       // Input[1], FX[0]
    FST     [ECX].Single             // FX[0] := Input[1];
    FSUB    [EDI].Single             // (Input[1] - FY[0])
    FMUL    [EAX].Double             // (Input[1] - FY[0]) * FCoefficients[0]
    FADDP                            // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Single

    FLD     [ECX + 4].Single         // FX[1]
    FLD     [Input].Single           // Input[0], FX[1]
    FST     [ECX + 4].Single         // FX[1] := Input[0];
    FSUB    [EDI + 4].Single         // (Input[0] - FY[1])
    FMUL    [EAX + 8].Double         // (Input[0] - FY[1]) * FCoefficients[1]
    FADDP                            // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
    FST     [EDI + 4].Single

    FLD     [ECX +  8].Single        // FX[2], FY[1]
    FLD     [EDI].Single             // FY[0], FX[2], FY[1]
    FST     [ECX +  8].Single        // FX[2] := FY[0];
    FSUB    [EDI +  8].Single        // (FY[0] - FY[2]), FY[1]
    FMUL    [EAX + 16].Double        // (FY[0] - FY[2]) * FCoefficients[2], FY[1]
    FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FST     [EDI +  8].Single

    FADDP                            // FY[2] + FY[1]
    FMUL    CHalf32                  // (FY[2] + FY[1]) * 0.5
    POP     EDI
{$ENDIF}
end;

function TPolyphaseDownsampler32.ProcessSample4(const Input: TDAV2SingleArray): Single;
{$IFDEF PUREPASCAL}
var
  LocalX : PDAV4SingleArray;
  LocalY : PDAV4SingleArray;
  Coeffs : PDAV4DoubleArray;
begin
 // initialize local variables
 LocalX := PDAV4SingleArray(FX);
 LocalY := PDAV4SingleArray(FY);
 Coeffs := PDAV4DoubleArray(FCoefficients);

 // process filter
 LocalY[0] := (Input[1] - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input[1];
 LocalY[1] := (Input[0] - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input[0];
 LocalY[2] := (LocalY[0] - LocalY[2]) * Coeffs[2] + LocalX[2];
 LocalX[2] := LocalY[0];
 LocalY[3] := (LocalY[1] - LocalY[3]) * Coeffs[3] + LocalX[3];
 LocalX[3] := LocalY[1];

 // calculate result
 Result := CHalf32 * (LocalY[2] + LocalY[3]);
{$ELSE}
asm
    PUSH    EDI
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     ECX, [EAX.FX]            // ESI = FX
    MOV     EAX, [EAX.FCoefficients] // ECX = FCoefficients

    FLD     [ECX].Single             // FX[0]
    FLD     [Input + 4].Single       // Input[1], FX[0]
    FST     [ECX].Single             // FX[0] := Input[1];
    FSUB    [EDI].Single             // (Input[1] - FY[0])
    FMUL    [EAX].Double             // (Input[1] - FY[0]) * FCoefficients[0]
    FADDP                            // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Single

    FLD     [ECX + 4].Single         // FX[1]
    FLD     [Input].Single           // Input[0], FX[1]
    FST     [ECX + 4].Single         // FX[1] := Input[0];
    FSUB    [EDI + 4].Single         // (Input[0] - FY[1])
    FMUL    [EAX + 8].Double         // (Input[0] - FY[1]) * FCoefficients[1]
    FADDP                            // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [EDI + 4].Single

    FLD     [ECX +  8].Single        // FX[2]
    FLD     [EDI].Single             // FY[0], FX[2]
    FST     [ECX +  8].Single        // FX[2] := FY[0];
    FSUB    [EDI +  8].Single        // (FY[0] - FY[2])
    FMUL    [EAX + 16].Double        // (FY[0] - FY[2]) * FCoefficients[2]
    FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FST     [EDI + 8].Single

    FLD     [ECX + 12].Single        // FX[2], FY[2]
    FLD     [EDI + 4].Single         // FY[0], FX[2], FY[2]
    FST     [ECX + 12].Single        // FX[2] := FY[0];
    FSUB    [EDI + 12].Single        // (FY[0] - FY[2]), FY[2]
    FMUL    [EAX + 24].Double        // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI + 12].Single

    FADDP                            // FY[3] + FY[2]
    FMUL    CHalf32                  // (FY[3] + FY[2]) * 0.5
    POP     EDI
{$ENDIF}
end;

function TPolyphaseDownsampler32.ProcessSampleLarge(const Input: TDAV2SingleArray): Single;
{$IFDEF PUREPASCAL}
var
  Index  : Integer;
  LocalX : PDAV2SingleArray;
  LocalY : PDAV2SingleArray;
  Coeffs : PDAV2DoubleArray;
begin
 // initialize local variables
 LocalX := PDAV2SingleArray(FX);
 LocalY := PDAV2SingleArray(FY);
 Coeffs := PDAV2DoubleArray(FCoefficients);

 // process first filters
 LocalY[0] := (Input[1] - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input[1];
 LocalY[1] := (Input[0] - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input[0];

 // process other filters
 for Index := 2 to FNumberOfCoeffs - 1 do
  begin
   LocalY[Index] := (LocalY[Index - 2] - LocalY[Index]) * Coeffs[Index] + LocalX[Index];
   LocalX[Index] :=  LocalY[Index - 2];
  end;

 // calculate result
 Result := CHalf32 * (LocalY[FNumberOfCoeffs - 1] + LocalY[FNumberOfCoeffs - 2]);
{$ELSE}
asm
    PUSH    EBX
    PUSH    ESI
    PUSH    EDI
    MOV     ESI, [EAX.FX]             // ESI = FX
    MOV     EDI, [EAX.FY]             // EDI = FY
    MOV     EBX, [EAX.FCoefficients]  // ECX = FCoefficients

    FLD     [ESI].Single              // FX[0]
    FLD     [Input + 4].Single        // Input[1], FX[0]
    FST     [ESI].Single              // FX[0] := Input[1];
    FSUB    [EDI].Single              // (Input[1] - FY[0])
    FMUL    [EBX].Double              // (Input[1] - FY[0]) * FCoefficients[0]
    FADDP                             // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Single

    FLD     [ESI + 4].Single          // FX[1]
    FLD     [Input].Single            // Input[0], FX[1]
    FST     [ESI + 4].Single          // FX[1] := Input[0];
    FSUB    [EDI + 4].Single          // (Input[0] - FY[1])
    FMUL    [EBX + 8].Double          // (Input[0] - FY[1]) * FCoefficients[1]
    FADDP                             // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [EDI + 4].Single

    MOV     ECX,[EAX.FNumberOfCoeffs] // ECX=self.FNumberOfCoeffs
    SUB     ECX, 4                    // "Den Rest mach ich selber"

@Loopy:
    FLD     [ESI +  8].Single         // FX[a]
    FLD     [EDI].Single              // FY[b], FX[a]
    FST     [ESI +  8].Single         // FX[a] := FY[b];
    FSUB    [EDI +  8].Single         // (FY[b] - FY[a])
    FMUL    [EBX + 16].Double         // (FY[b] - FY[a]) * FCoefficients[a]
    FADDP                             // (FY[b] - FY[a]) * FCoefficients[a] + FX[a]
    FSTP    [EDI +  8].Single

    ADD     ESI, 4
    ADD     EDI, 4
    ADD     EBX, 8                    // Weiter geht's
    LOOP    @Loopy

    FLD     [ESI + 8].Single          // FX[a]
    FLD     [EDI].Single              // FY[b], FX[a]
    FST     [ESI +  8].Single         // FX[a] := FY[b];
    FSUB    [EDI +  8].Single         // (FY[b] - FY[a])
    FMUL    [EBX + 16].Double         // (FY[b] - FY[a]) * FCoefficients[a]
    FADDP                             // (FY[b] - FY[a]) * FCoefficients[a] + FX[a]
    FST     [EDI +  8].Single

    FLD     [ESI + 12].Single         // FX[a], FY[a]
    FLD     [EDI + 4].Single          // FY[b], FX[a], FY[a]
    FST     [ESI + 12].Single         // FX[a] := FY[b];
    FSUB    [EDI + 12].Single         // (FY[b] - FY[a]), FY[a]
    FMUL    [EBX + 24].Double         // (FY[b] - FY[a]) * FCoefficients[a], FY[a]
    FADDP                             // (FY[b] - FY[a]) * FCoefficients[a] + FX[a], FY[a]
    FST     [EDI + 12].Single

    FADDP                             // FY[a] + FY[aalt]
    FMUL    CHalf32                   // (FY[a] + FY[aalt]) * 0.5
    POP     EDI
    POP     ESI
    POP     EBX
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   Name: ProcessSampleSplit                                                 //
//   ------------------------                                                 //
//                                                                            //
//   Description:                                                             //
//     Split (spectrum-wise) in half a pair of samples. The lower part of the //
//     spectrum is a classic downsampling, equivalent to the output of        //
//     process_sample().                                                      //
//     The higher part is the complementary signal: original filter response  //
//     is flipped from left to right, becoming a high-pass filter with the    //
//     same cutoff frequency. This signal is then critically sampled          //
//    (decimation by 2), flipping the spectrum: Fs/4...Fs/2 becomes Fs/4...0. //
//                                                                            //
//  Input parameters:                                                         //
//  - Input: pointer on the pair of input samples                             //
//                                                                            //
//  Output parameters:                                                        //
//  - low: output sample, lower part of the spectrum (downsampling)           //
//  - high: output sample, higher part of the spectrum.                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseDownsampler32.ProcessSampleSplit1(out Low, High: Single; Input: TDAV2SingleArray);
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0];
 FX[0] := Input[1];

 Low   := CHalf32 * (FY[0] + Input[0]);
 High  := FY[0] - Low;
end;

procedure TPolyphaseDownsampler32.ProcessSampleSplit2(out Low, High: Single; Input: TDAV2SingleArray);
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0];
 FX[0] := Input[1];

 PDav2SingleArray(FY)^[1] := (Input[0] - PDav2SingleArray(FY)^[1]) *
   PDav2DoubleArray(FCoefficients)^[1] + PDav2SingleArray(FX)^[1];
 PDav2SingleArray(FX)^[1] := Input[0];
 Low := CHalf32 * (PDav2SingleArray(FY)^[0] + PDav2SingleArray(FY)^[1]);
 High := PDav2SingleArray(FY)^[1] - Low;
end;

procedure TPolyphaseDownsampler32.ProcessSampleSplit3(out Low, High: Single; Input: TDAV2SingleArray);
begin
 FY[0] := (Input[1] - FY[0]) * PDav4DoubleArray(FCoefficients)^[0] + FX[0];
 FX[0] := Input[1];

 PDav4SingleArray(FY)^[1] := (Input[0] - PDav4SingleArray(FY)^[1]) *
   PDav4DoubleArray(FCoefficients)^[1] + PDav4SingleArray(FX)^[1];
 PDav4SingleArray(FX)^[1] := Input[0];
 PDav4SingleArray(FY)^[2] := (PDav4SingleArray(FY)^[0] - PDav4SingleArray(FY)^[2]) *
   PDav4DoubleArray(FCoefficients)^[2] + PDav4SingleArray(FX)^[2];
 PDav4SingleArray(FX)^[2] := PDav4SingleArray(FY)^[0];
 Low := CHalf32 * (PDav4SingleArray(FY)^[2] + PDav4SingleArray(FY)^[1]);
 High := PDav4SingleArray(FY)^[2] - Low;
end;

procedure TPolyphaseDownsampler32.ProcessSampleSplit4(out Low, High: Single; Input: TDAV2SingleArray);
begin
  FY[0] := (Input[1] - FY[0]) * PDav4DoubleArray(FCoefficients)^[0] + FX[0];
  FX[0] := Input[1];

  PDav4SingleArray(FY)^[1] := (Input[0] - PDav4SingleArray(FY)^[1]) *
    PDav4DoubleArray(FCoefficients)^[1] + PDav4SingleArray(FX)^[1];
  PDav4SingleArray(FX)^[1] := Input[0];
  PDav4SingleArray(FY)^[2] := (PDav4SingleArray(FY)^[0] - PDav4SingleArray(FY)^[2]) *
    PDav4DoubleArray(FCoefficients)^[2] + PDav4SingleArray(FX)^[2];
  PDav4SingleArray(FX)^[2] := PDav4SingleArray(FY)^[0];
  PDav4SingleArray(FY)^[3] := (PDav4SingleArray(FY)^[1] - PDav4SingleArray(FY)^[3]) *
    PDav4DoubleArray(FCoefficients)^[3] + PDav4SingleArray(FX)^[3];
  PDav4SingleArray(FX)^[3] := PDav4SingleArray(FY)^[1];
  Low := CHalf32 * (PDav4SingleArray(FY)^[3] + PDav4SingleArray(FY)^[2]);
  High := PDav4SingleArray(FY)^[3] - Low;
end;

procedure TPolyphaseDownsampler32.ProcessSampleSplitLarge(
  out Low, High: Single; Input: TDAV2SingleArray);
var
  Index : Integer;
begin
  FY[0] := (Input[1] - FY[0]) * PDav4DoubleArray(FCoefficients)^[0] + FX[0]; FX[0] := Input[1];
  PDav2SingleArray(FY)^[1] := (Input[0] - PDav2SingleArray(FY)^[1]) *
    PDav2DoubleArray(FCoefficients)^[1] + PDav2SingleArray(FX)^[1];
  PDav2SingleArray(FX)^[1] := Input[0];
  for Index := 2 to FNumberOfCoeffs - 1 do
   begin
    FY[Index] := (FY[Index - 2] - FY[Index]) * FCoefficients[Index] + FX[Index];
    FX[Index] := FY[Index - 2];
   end;

  Low := 0.5 * (FY[FNumberOfCoeffs - 1] + FY[FNumberOfCoeffs - 2]);
  High := FY[FNumberOfCoeffs - 1] - Low;
end;

procedure TPolyphaseDownsampler32.ProcessBlockSplit(
  const OutputL, OutputH, Input: PDAVSingleFixedArray; SampleFrames: Integer);
var
  Index : Integer;
begin
  Assert(SampleFrames > 0);
  Index := 0;
  repeat
    ProcessSampleSplit(OutputL[Index], OutputH[Index], PDAV2SingleArray(
      @Input[Index * 2])^);
    Inc(Index);
  until (Index >= SampleFrames div 2);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ProcessBlock                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Downsamples (x2) a block of samples.                                    //
//    Input and output blocks may overlap, see Assert() for details.          //
//                                                                            //
//  Input parameters:                                                         //
//    - Input: Input array, containing SampleFrames * 2 samples.              //
//    - SampleFrames: Number of samples to output, > 0                        //
//                                                                            //
//  Output parameters:                                                        //
//    - Output: Array for the output samples, capacity: SampleFrames samples. //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseDownsampler32.ProcessBlock(
  const Input, Output: PDAVSingleFixedArray; SampleFrames: Integer);
var
  Index: Integer;
begin
 for Index := 0 to SampleFrames - 1
  do Output[Index] := FProcessSample32(PDAV2SingleArray(@Input[Index * 2])^);
end;

function TPolyphaseDownsampler32.ProcessSample32(
  const Input: TDAV2SingleArray): Single;
begin
 Result := FProcessSample32(Input);
end;


{ TPolyphaseDownsampler64 }

constructor TPolyphaseDownsampler64.Create;
begin
 FX := nil;
 FY := nil;
 FStateStack := nil;
 inherited;
end;

constructor TPolyphaseDownsampler64.Create(const NumberOfCoefficients: Integer;
  const Transition: Double);
begin
 FX := nil;
 FY := nil;
 FStateStack := nil;
 inherited;
end;

destructor TPolyphaseDownsampler64.Destroy;
begin
 {$IFDEF UseAlignedMemory}
 FreeAlignedMemory(FX);
 FreeAlignedMemory(FY);
 {$ELSE}
 Dispose(FX);
 Dispose(FY);
 {$ENDIF}
 Dispose(FStateStack);
 inherited;
end;

procedure TPolyphaseDownsampler64.AssignTo(Dest: TPersistent);
var
  Index : Integer;
begin
 inherited;
 if Dest is TPolyphaseDownsampler64 then
  with TPolyphaseDownsampler64(Dest) do
   begin
    inherited;
    FProcessSample64      := Self.FProcessSample64;
    FProcessSampleSplit64 := Self.FProcessSampleSplit64;

    Assert(FNumberOfCoeffs = Self.FNumberOfCoeffs);
    Move(Self.FX^, FX^, FNumberOfCoeffs * SizeOf(Double));
    Move(Self.FY^, FY^, FNumberOfCoeffs * SizeOf(Double));
    Move(Self.FStateStack^, FStateStack^, 2 * FNumberOfCoeffs * SizeOf(Double));
   end else
 if Dest is TPolyphaseDownsampler32 then
  with TPolyphaseDownsampler32(Dest) do
   begin
    inherited;
    Assert(FNumberOfCoeffs = Self.FNumberOfCoeffs);
    ChooseProcedures;

    for Index := 0 to FNumberOfCoeffs - 1 do
     begin
      FX^[Index] := Self.FX^[Index];
      FY^[Index] := Self.FY^[Index];
      FStateStack^[Index] := Self.FStateStack^[Index];
     end;
   end;
end;

procedure TPolyphaseDownsampler64.AllocateBuffer;
begin
 {$IFDEF UseAlignedMemory}
 ReallocateAlignedMemory(Pointer(FX), FNumberOfCoeffs * SizeOf(Double));
 ReallocateAlignedMemory(Pointer(FY), FNumberOfCoeffs * SizeOf(Double));
 {$ELSE}
 ReallocMem(Pointer(FX), FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(Pointer(FY), FNumberOfCoeffs * SizeOf(Double));
 {$ENDIF}
 ReallocMem(FStateStack, 2 * FNumberOfCoeffs * SizeOf(Double));
end;

procedure TPolyphaseDownsampler64.ChooseProcedures;
begin
  case FNumberOfCoeffs of
    1 :
     begin
      FProcessSample64      := ProcessSample1;
      FProcessSampleSplit64 := ProcessSampleSplit1;
     end;
    2 :
     begin
      FProcessSample64      := ProcessSample2;
      FProcessSampleSplit64 := ProcessSampleSplit2;
     end;
    3 :
     begin
      FProcessSample64      := ProcessSample3;
      FProcessSampleSplit64 := ProcessSampleSplit3;
     end;
    4 :
     begin
      FProcessSample64      := ProcessSample4;
      FProcessSampleSplit64 := ProcessSampleSplit4;
     end;
  else
   begin
    FProcessSample64      := ProcessSampleLarge;
    FProcessSampleSplit64 := ProcessSampleSplitLarge;
   end;
 end;
end;

function TPolyphaseDownsampler64.ProcessSample1(
  const Input: TDAV2DoubleArray): Double;
{$IFDEF PUREPASCAL}
begin
 FY[0]  := (Input[1] - FY[0]) * FCoefficients[0] + FX[0];
 FX[0]  := Input[1];
 Result := 0.5 * (FY[0] + Input[0]);
{$ELSE}
asm
  PUSHAD
  MOV     ESI, [EAX.FX]            // ESI = FX
  MOV     EDI, [EAX.FY]            // EDI = FY
  MOV     ECX, [EAX.FCoefficients] // ECX = FCoefficients

  FLD     [ESI].Double             // FX[0]
  FLD     [Input + 8].Double       // Input[1], FX[0]
  FST     [ESI].Double             // FX[0] := Input[1];
  FSUB    [EDI].Double             // (Input[1] - FY[0])
  FMUL    [ECX].Double             // (Input[1] - FY[0]) * FCoefficients[0]
  FADDP                            // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
  FST     [EDI].Double
// FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
  FLD     [Input].Double           // Input[0], FY[0]

  FADDP                            // FY[1] + FY[0]
  FMUL    CHalf64                  // (FY[1] + FY[0]) * 0.5
  POPAD
{$ENDIF}
end;

function TPolyphaseDownsampler64.ProcessSample2(
  const Input: TDAV2DoubleArray): Double;
{$IFDEF PUREPASCAL}
var
  LocalX : PDAV2DoubleArray;
  LocalY : PDAV2DoubleArray;
  Coeffs : PDAV2DoubleArray;
begin
 // initialize local variables
 LocalX := PDAV2DoubleArray(FX);
 LocalY := PDAV2DoubleArray(FY);
 Coeffs := PDAV2DoubleArray(FCoefficients);

 // process filter
 LocalY[0] := (Input[1] - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input[1];
 LocalY[1] := (Input[0] - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input[0];

 // calculate result
 Result := CHalf64 * (LocalY[0] + LocalY[1]);
{$ELSE}
asm
  PUSHAD
  MOV     ESI, [EAX.FX]            // ESI = FX
  MOV     EDI, [EAX.FY]            // EDI = FY
  MOV     ECX, [EAX.FCoefficients] // ECX = FCoefficients

  FLD     [ESI].Double             // FX[0]
  FLD     [Input + 8].Double       // Input[1], FX[0]
  FST     [ESI].Double             // FX[0] := Input[1];
  FSUB    [EDI].Double             // (Input[1] - FY[0])
  FMUL    [ECX].Double             // (Input[1] - FY[0]) * FCoefficients[0]
  FADDP                            // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
  FST     [EDI].Double
// FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]

  FLD     [ESI + 8].Double         // FX[1], FY[0]
  FLD     [Input].Double           // Input[0], FX[1], FY[0]
  FST     [ESI + 8].Double         // FX[1] := Input[0];
  FSUB    [EDI + 8].Double         // (Input[0] - FY[1]), FY[0]
  FMUL    [ECX + 8].Double         // (Input[0] - FY[1]) * FCoefficients[1], FY[0]
  FADDP                            // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
  FST     [EDI + 8].Double
// FY[1] := (Input[0] - FY[1]) * FCoefficients[1] + FX[1]

  FADDP                            // FY[1] + FY[0]
  FMUL    CHalf64                  // (FY[1] + FY[0]) * 0.5
  POPAD
{$ENDIF}
end;

function TPolyphaseDownsampler64.ProcessSample3(
  const Input: TDAV2DoubleArray): Double;
{$IFDEF PUREPASCAL}
var
  LocalX : PDAV3DoubleArray;
  LocalY : PDAV3DoubleArray;
  Coeffs : PDAV3DoubleArray;
begin
 // initialize local variables
 LocalX := PDAV3DoubleArray(FX);
 LocalY := PDAV3DoubleArray(FY);
 Coeffs := PDAV3DoubleArray(FCoefficients);

 // process filter
 LocalY[0] := (Input[1] - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input[1];
 LocalY[1] := (Input[0] - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input[0];
 LocalY[2] := (LocalY[0] - LocalY[2]) * Coeffs[2] + LocalX[2];
 LocalX[2] := LocalY[0];

 // calculate result
 Result := 0.5 * (LocalY[1] + LocalY[2]);
{$ELSE}
asm
  PUSHAD
  MOV     ESI, [EAX.FX]            // ESI = FX
  MOV     EDI, [EAX.FY]            // EDI = FY
  MOV     ECX, [EAX.FCoefficients] // ECX = FCoefficients

  FLD     [ESI].Double             // FX[0]
  FLD     [Input + 8].Double       // Input[1], FX[0]
  FST     [ESI].Double             // FX[0] := Input[1];
  FSUB    [EDI].Double             // (Input[1] - FY[0])
  FMUL    [ECX].Double             // (Input[1] - FY[0]) * FCoefficients[0]
  FADDP                            // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
  FSTP    [EDI].Double

  FLD     [ESI + 8].Double         // FX[1]
  FLD     [Input].Double           // Input[0], FX[1]
  FST     [ESI + 8].Double         // FX[1] := Input[0];
  FSUB    [EDI + 8].Double         // (Input[0] - FY[1])
  FMUL    [ECX + 8].Double         // (Input[0] - FY[1]) * FCoefficients[1]
  FADDP                            // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
  FST     [EDI + 8].Double

  FLD     [ESI + 16].Double        // FX[2], FY[1]
  FLD     [EDI].Double             // FY[0], FX[2], FY[1]
  FST     [ESI + 16].Double        // FX[2] := FY[0];
  FSUB    [EDI + 16].Double        // (FY[0] - FY[2]), FY[1]
  FMUL    [ECX + 16].Double        // (FY[0] - FY[2]) * FCoefficients[2], FY[1]
  FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
  FST     [EDI + 16].Double

  FADDP                            // FY[2] + FY[1]
  FMUL    CHalf64                  // (FY[2] + FY[1]) * 0.5
  POPAD
{$ENDIF}
end;

{-$DEFINE PUREPASCAL}

function TPolyphaseDownsampler64.ProcessSample4(
  const Input: TDAV2DoubleArray): Double;
{$IFDEF PUREPASCAL}
var
  LocalX : PDAV4DoubleArray;
  LocalY : PDAV4DoubleArray;
  Coeffs : PDAV4DoubleArray;
begin
 // initialize local variables
 LocalX := PDAV4DoubleArray(FX);
 LocalY := PDAV4DoubleArray(FY);
 Coeffs := PDAV4DoubleArray(FCoefficients);

 // process filter
 LocalY[0] := (Input[1] - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input[1];
 LocalY[1] := (Input[0] - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input[0];
 LocalY[2] := (LocalY[0] - LocalY[2]) * Coeffs[2] + LocalX[2];
 LocalX[2] := LocalY[0];
 LocalY[3] := (LocalY[1] - LocalY[3]) * Coeffs[3] + LocalX[3];
 LocalX[3] := LocalY[1];

 // calculate output
 Result := 0.5 * (LocalY[2] + LocalY[3]);
{$ELSE}
asm
  PUSH    EBX
  MOV     EBX, [EAX.FY]            // EBX = FY
  MOV     ECX, [EAX.FX]            // ESI = FX
  MOV     EAX, [EAX.FCoefficients] // ECX = FCoefficients

  FLD     [ECX].Double             // FX[0]
  FLD     [Input + 8].Double       // Input[1], FX[0]
  FST     [ECX].Double             // FX[0] := Input[1] , FX[0]
  FSUB    [EBX].Double             // (Input[1] - FY[0]), FX[0]
  FMUL    [EAX].Double             // (Input[1] - FY[0]) * FCoefficients[0], FX[0]
  FADDP                            // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
  FSTP    [EBX].Double
// FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]

  FLD     [ECX + 8].Double         // FX[1]
  FLD     [Input].Double           // Input[0], FX[1]
  FST     [ECX + 8].Double         // FX[1] := Input[0], FX[1]
  FSUB    [EBX + 8].Double         // (Input[0] - FY[1]), FX[1]
  FMUL    [EAX + 8].Double         // (Input[0] - FY[1]) * FCoefficients[1], FX[1]
  FADDP                            // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
  FSTP    [EBX + 8].Double
// FY[1] := (Input[0] - FY[1]) * FCoefficients[1] + FX[1]

  FLD     [ECX + 16].Double        // FX[2]
  FLD     [EBX].Double             // FY[0], FX[2]
  FST     [ECX + 16].Double        // FX[2] := FY[0], FX[2]
  FSUB    [EBX + 16].Double        // (FY[0] - FY[2]), FX[2]
  FMUL    [EAX + 16].Double        // (FY[0] - FY[2]) * FCoefficients[2], FX[2]
  FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
  FST     [EBX + 16].Double        // FY[2]
// FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]

  FLD     [ECX + 24].Double        // FX[3], FY[2]
  FLD     [EBX + 8].Double         // FY[1], FX[3], FY[2]
  FST     [ECX + 24].Double        // FX[3] := FY[1], FX[3], FY[2]
  FSUB    [EBX + 24].Double        // (FY[1] - FY[3]), FX[3], FY[2]
  FMUL    [EAX + 24].Double        // (FY[1] - FY[3]) * FCoefficients[3], FX[3], FY[2]
  FADDP                            // (FY[1] - FY[3]) * FCoefficients[3] + FX[3], FY[2]
  FST     [EBX + 24].Double        // FY[3], FY[2]
// FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]

  FADDP                            // FY[3] + FY[2]
  FMUL    CHalf64                  // (FY[3] + FY[2]) * 0.5
  POP     EBX
{$ENDIF}
end;

{-$UNDEF PUREPASCAL}

function TPolyphaseDownsampler64.ProcessSampleLarge(
  const Input: TDAV2DoubleArray): Double;
{$IFDEF PUREPASCAL}
var
  Index  : Integer;
  LocalX : PDAV2DoubleArray;
  LocalY : PDAV2DoubleArray;
  Coeffs : PDAV2DoubleArray;
begin
 // initialize local variables
 LocalX := PDAV2DoubleArray(FX);
 LocalY := PDAV2DoubleArray(FY);
 Coeffs := PDAV2DoubleArray(FCoefficients);

 // process first filter
 LocalY[0] := (Input[1] - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input[1];
 LocalY[1] := (Input[0] - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input[0];

 // process remaining filters
 for Index := 2 to FNumberOfCoeffs - 1 do
  begin
   LocalY[Index] := (LocalY[Index - 2] - LocalY[Index]) * Coeffs[Index] + LocalX[Index];
   LocalX[Index] := LocalY[Index - 2];
  end;

 // calculate output
 Result := 0.5 * (LocalY[FNumberOfCoeffs - 1] + LocalY[FNumberOfCoeffs - 2]);
{$ELSE}
asm
    PUSH    EBX
    PUSH    ESI
    PUSH    EDI
    MOV     ESI, [EAX.FX]             // ESI = FX
    MOV     EDI, [EAX.FY]             // EDI = FY
    MOV     EBX, [EAX.FCoefficients]  // ECX = FCoefficients

    FLD     [ESI].Double              // FX[0]
    FLD     [Input + 8].Double        // Input[1], FX[0]
    FST     [ESI].Double              // FX[0] := Input[1];
    FSUB    [EDI].Double              // (Input[1] - FY[0])
    FMUL    [EBX].Double              // (Input[1] - FY[0]) * FCoefficients[0]
    FADDP                             // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Double
    // FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]

    FLD     [ESI + 8].Double          // FX[1]
    FLD     [Input].Double            // Input[0], FX[1]
    FST     [ESI + 8].Double          // FX[1] := Input[0];
    FSUB    [EDI + 8].Double          // (Input[0] - FY[1])
    FMUL    [EBX + 8].Double          // (Input[0] - FY[1]) * FCoefficients[1]
    FADDP                             // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [EDI + 8].Double
    // FY[1] := (Input[0] - FY[1]) * FCoefficients[1] + FX[1]

    MOV     ECX,[EAX.FNumberOfCoeffs] // ECX = self.FNumberOfCoeffs
    SUB     ECX, 4                    // "Den Rest mach ich selber"
@Loopy:
    FLD     [ESI + 16].Double         // FX[a]
    FLD     [EDI].Double              // FY[b], FX[a]
    FST     [ESI + 16].Double         // FX[a] := FY[b];
    FSUB    [EDI + 16].Double         // (FY[b] - FY[a])
    FMUL    [EBX + 16].Double         // (FY[b] - FY[a]) * FCoefficients[a]
    FADDP                             // (FY[b] - FY[a]) * FCoefficients[a] + FX[a]
    FSTP    [EDI + 16].Double
    // FY[a] := (FY[b] - FY[a]) * FCoefficients[a] + FX[a]

    ADD     ESI, 8
    ADD     EDI, 8
    ADD     EBX, 8
    LOOP    @Loopy                    // LOOP

    FLD     [ESI + 16].Double         // FX[a]
    FLD     [EDI].Double              // FY[b], FX[a]
    FST     [ESI + 16].Double         // FX[a] := FY[b];
    FSUB    [EDI + 16].Double         // (FY[b] - FY[a])
    FMUL    [EBX + 16].Double         // (FY[b] - FY[a]) * FCoefficients[a]
    FADDP                             // (FY[b] - FY[a]) * FCoefficients[a] + FX[a]
    FST     [EDI + 16].Double
    // FY[a] := (FY[b] - FY[a]) * FCoefficients[a] + FX[a]

    FLD     [ESI + 24].Double         // FX[a], FY[a]
    FLD     [EDI +  8].Double         // FY[b], FX[a], FY[a]
    FST     [ESI + 24].Double         // FX[a] := FY[b];
    FSUB    [EDI + 24].Double         // (FY[b] - FY[a]), FY[a]
    FMUL    [EBX + 24].Double         // (FY[b] - FY[a]) * FCoefficients[a], FY[a]
    FADDP                             // (FY[b] - FY[a]) * FCoefficients[a] + FX[a], FY[a]
    FST     [EDI + 24].Double
    // FY[a] := (FY[b] - FY[a]) * FCoefficients[a] + FX[a]

    FADDP                             // FY[a] + FY[aalt]
    FMUL    CHalf64                   // (FY[a] + FY[aalt]) * 0.5
    POP     EDI
    POP     ESI
    POP     EBX
{$ENDIF}
end;

procedure TPolyphaseDownsampler64.PushStates;
begin
 Move(FX[0], FStateStack[0], FNumberOfCoeffs * SizeOf(Double));
 Move(FY[0], FStateStack[FNumberOfCoeffs], FNumberOfCoeffs * SizeOf(Double));
end;

procedure TPolyphaseDownsampler64.PopStates;
begin
 Move(FStateStack[0], FX[0], FNumberOfCoeffs * SizeOf(Double));
 Move(FStateStack[FNumberOfCoeffs], FY[0], FNumberOfCoeffs * SizeOf(Double));
end;

procedure TPolyphaseDownsampler64.ResetStates;
begin
 FillChar(FX[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FY[0], FNumberOfCoeffs * SizeOf(Double), 0);
end;

procedure TPolyphaseDownsampler64.ClearBuffers;
begin
 FillChar(FX[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FY[0], FNumberOfCoeffs * SizeOf(Double), 0);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ProcessBlock                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Downsamples (x2) a block of samples.                                    //
//    Input and output blocks may overlap, see Assert() for details.          //
//                                                                            //
//  Input parameters:                                                         //
//    - Input: Input array, containing SampleFrames * 2 samples.              //
//    - SampleFrames: Number of samples to output, > 0                        //
//                                                                            //
//  Output parameters:                                                        //
//    - Output: Array for the output samples, capacity: SampleFrames samples. //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseDownsampler64.ProcessBlock(
  const Input, Output: PDAVDoubleFixedArray; SampleFrames: Integer);
var
  SampleIndex: Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1
  do Output[SampleIndex] := FProcessSample64(PDAV2DoubleArray(@Input[SampleIndex * 2])^);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   Name: ProcessSampleSplit                                                 //
//   ------------------------                                                 //
//                                                                            //
//   Description:                                                             //
//     Split (spectrum-wise) in half a pair of samples. The lower part of the //
//     spectrum is a classic downsampling, equivalent to the output of        //
//     process_sample().                                                      //
//     The higher part is the complementary signal: original filter response  //
//     is flipped from left to right, becoming a high-pass filter with the    //
//     same cutoff frequency. This signal is then critically sampled          //
//    (decimation by 2), flipping the spectrum: Fs/4...Fs/2 becomes Fs/4...0. //
//                                                                            //
//  Input parameters:                                                         //
//  - Input: pointer on the pair of input samples                             //
//                                                                            //
//  Output parameters:                                                        //
//  - low: output sample, lower part of the spectrum (downsampling)           //
//  - high: output sample, higher part of the spectrum.                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseDownsampler64.ProcessSampleSplit1(
  out Low, High: Double; Input: TDAV2DoubleArray);
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0];
 FX[0] := Input[1];
 Low := (FY[0] + Input[0]) * 0.5;
 High := FY[0] - Low;
end;

procedure TPolyphaseDownsampler64.ProcessSampleSplit2(
  out Low, High: Double; Input: TDAV2DoubleArray);
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0];
 FX[0] := Input[1];
 PDav2DoubleArray(FY)^[1] := (Input[0] - PDav2DoubleArray(FY)^[1]) *
   PDav2DoubleArray(FCoefficients)^[1] + PDav2DoubleArray(FX)^[1];
 PDav2DoubleArray(FX)^[1] := Input[0];
 Low   := (PDav2DoubleArray(FY)^[1] + PDav2DoubleArray(FY)^[0]) * 0.5;
 High  := PDav2DoubleArray(FY)^[1] - Low;
end;

procedure TPolyphaseDownsampler64.ProcessSampleSplit3(
  out Low, High: Double; Input: TDAV2DoubleArray);
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
 PDav4DoubleArray(FY)^[1] := (Input[0] - PDav4DoubleArray(FY)^[1]) *
   PDav4DoubleArray(FCoefficients)^[1] + PDav4DoubleArray(FX)^[1];
 PDav4DoubleArray(FX)^[1] := Input[0];
 PDav4DoubleArray(FY)^[2] := (PDav4DoubleArray(FY)^[0] - PDav4DoubleArray(FY)^[2]) *
   PDav4DoubleArray(FCoefficients)^[2] + PDav4DoubleArray(FX)^[2];
 PDav4DoubleArray(FX)^[2] := PDav4DoubleArray(FY)^[0];
 Low   := (PDav4DoubleArray(FY)^[2] + PDav4DoubleArray(FY)^[1]) * 0.5;
 High  := PDav4DoubleArray(FY)^[2] - Low;
end;

procedure TPolyphaseDownsampler64.ProcessSampleSplit4(
  out Low, High: Double; Input: TDAV2DoubleArray);
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
 PDav4DoubleArray(FY)^[1] := (Input[0] - PDav4DoubleArray(FY)^[1]) *
   PDav4DoubleArray(FCoefficients)^[1] + PDav4DoubleArray(FX)^[1];
 PDav4DoubleArray(FX)^[1] := Input[0];
 PDav4DoubleArray(FY)^[2] := (PDav4DoubleArray(FY)^[0] - PDav4DoubleArray(FY)^[2]) *
   PDav4DoubleArray(FCoefficients)^[2] + PDav4DoubleArray(FX)^[2];
 PDav4DoubleArray(FX)^[2] := PDav4DoubleArray(FY)^[0];
 PDav4DoubleArray(FY)^[3] := (PDav4DoubleArray(FY)^[1] - PDav4DoubleArray(FY)^[3]) *
   PDav4DoubleArray(FCoefficients)^[3] + PDav4DoubleArray(FX)^[3];
 PDav4DoubleArray(FX)^[3] := PDav4DoubleArray(FY)^[1];
 Low   := (PDav4DoubleArray(FY)^[3] + PDav4DoubleArray(FY)^[2]) * 0.5;
 High  := PDav4DoubleArray(FY)^[3] - Low;
end;

procedure TPolyphaseDownsampler64.ProcessSampleSplitLarge(
  out Low, High: Double; Input: TDAV2DoubleArray);
var
  Index : Integer;
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
 PDav2DoubleArray(FY)^[1] := (Input[0] - PDav2DoubleArray(FY)^[1]) *
   PDav2DoubleArray(FCoefficients)^[1] + PDav2DoubleArray(FX)^[1];
 PDav2DoubleArray(FX)^[1] := Input[0];
 for Index := 2 to FNumberOfCoeffs - 1 do
  begin
   FY[Index] := (FY[Index - 2] - FY[Index]) * FCoefficients[Index] + FX[Index];
   FX[Index] := FY[Index - 2];
  end;

 Low := 0.5 * (FY[FNumberOfCoeffs - 1] + FY[FNumberOfCoeffs - 2]);
 High := FY[FNumberOfCoeffs - 1] - Low;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   Name: ProcessBlockSplit                                                  //
//   -----------------------                                                  //
//                                                                            //
//   Description:                                                             //
//     Split (spectrum-wise) in half a pair of samples. The lower part of the //
//     spectrum is a classic downsampling, equivalent to the output of        //
//     process_sample().                                                      //
//     The higher part is the complementary signal: original filter response  //
//     is flipped from left to right, becoming a high-pass filter with the    //
//     same cutoff frequency. This signal is then critically sampled          //
//    (decimation by 2), flipping the spectrum: Fs/4...Fs/2 becomes Fs/4...0. //
//                                                                            //
//  Input parameters:                                                         //
//  - Input: pointer on the pair of input samples                             //
//                                                                            //
//  Output parameters:                                                        //
//  - low: output sample, lower part of the spectrum (downsampling)           //
//  - high: output sample, higher part of the spectrum.                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseDownsampler64.ProcessBlockSplit(
  const OutputL, OutputH, Input: PDAVDoubleFixedArray; SampleFrames: Integer);
var
  Pos: Integer;
begin
 Assert(SampleFrames > 0);

 Pos := 0;
 repeat
  ProcessSampleSplit(OutputL[Pos], OutputH[Pos], PDAV2DoubleArray(@Input[pos * 2])^);
  Inc(Pos);
 until (Pos >= SampleFrames);
end;

function TPolyphaseDownsampler64.ProcessSample64(
  const Input: TDAV2DoubleArray): Double;
begin
 Result := FProcessSample64(Input);
end;

end.
