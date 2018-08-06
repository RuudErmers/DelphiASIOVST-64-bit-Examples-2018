unit DAV_DspPolyphaseUpsampler;

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
  TProcessSample32 = procedure (const Input: Single; out Output: TDAV2SingleArray) of object;
  TProcessSample64 = procedure (const Input: Double; out Output: TDAV2DoubleArray) of object;

  TCustomPolyphaseUpsampler = class(TCustomPolyphaseFilter)
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

  TPolyphaseUpsampler32 = class(TCustomPolyphaseUpsampler)
  private
    FX, FY           : PDAVSingleFixedArray;
    FStateStack      : PDAVSingleFixedArray;
    FProcessSample32 : TProcessSample32;
    procedure ProcessSample1(const Input: Single; out Output: TDAV2SingleArray);
    procedure ProcessSample2(const Input: Single; out Output: TDAV2SingleArray);
    procedure ProcessSample3(const Input: Single; out Output: TDAV2SingleArray);
    procedure ProcessSample4(const Input: Single; out Output: TDAV2SingleArray);
    procedure ProcessSampleOdd(const Input: Single; out Output: TDAV2SingleArray);
    procedure ProcessSampleEven(const Input: Single; out Output: TDAV2SingleArray);
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
    procedure PushStates; override;
    procedure PopStates; override;
    procedure ResetStates; override;

    procedure ProcessBlock(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer);
    procedure ProcessSample32(Input: Single; out Output: TDAV2SingleArray);
    property ProcessSample: TProcessSample32 read FProcessSample32;
  end;

  TPolyphaseUpsampler64 = class(TCustomPolyphaseUpsampler)
  private
    FX, FY           : PDAVDoubleFixedArray;
    FStateStack      : PDAVDoubleFixedArray;
    FProcessSample64 : TProcessSample64;
    procedure ProcessSample1(const Input: Double; out Output: TDAV2DoubleArray);
    procedure ProcessSample2(const Input: Double; out Output: TDAV2DoubleArray);
    procedure ProcessSample3(const Input: Double; out Output: TDAV2DoubleArray);
    procedure ProcessSample4(const Input: Double; out Output: TDAV2DoubleArray);
    procedure ProcessSampleOdd(const Input: Double; out Output: TDAV2DoubleArray);
    procedure ProcessSampleEven(const Input: Double; out Output: TDAV2DoubleArray);
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
    procedure PushStates; override;
    procedure PopStates; override;
    procedure ResetStates; override;

    procedure ProcessBlock(const Input, Output: PDAVDoubleFixedArray; const SampleFrames: Integer);
    procedure ProcessSample64(Input: Double; out Output: TDAV2DoubleArray);
    property ProcessSample: TProcessSample64 read FProcessSample64;
  end;

implementation


{ TCustomPolyphaseUpsampler }

constructor TCustomPolyphaseUpsampler.Create;
begin
 inherited;
 AllocateBuffer;
 ChooseProcedures;
 ClearBuffers;
end;

constructor TCustomPolyphaseUpsampler.Create(
  const NumberOfCoefficients: Integer; const Transition: Double);
begin
 inherited;
 AllocateBuffer;
 ChooseProcedures;
 ClearBuffers;
end;

procedure TCustomPolyphaseUpsampler.NumberOfCoeffsChanged;
begin
 inherited;
 AllocateBuffer;
 ChooseProcedures;
 ClearBuffers;
end;


{ TPolyphaseUpsampler32 }

constructor TPolyphaseUpsampler32.Create;
begin
 FX          := nil;
 FY          := nil;
 FStateStack := nil;
 inherited;
end;

constructor TPolyphaseUpsampler32.Create(const NumberOfCoefficients: Integer;
  const Transition: Double);
begin
 FX          := nil;
 FY          := nil;
 FStateStack := nil;
 inherited;
end;

destructor TPolyphaseUpsampler32.Destroy;
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

procedure TPolyphaseUpsampler32.AssignTo(Dest: TPersistent);
var
  Index : Integer;
begin
 inherited;
 if Dest is TPolyphaseUpsampler32 then
  with TPolyphaseUpsampler32(Dest) do
   begin
    inherited;
    FProcessSample32 := Self.FProcessSample32;

    Assert(FNumberOfCoeffs = Self.FNumberOfCoeffs);
    Move(Self.FX^, FX^, FNumberOfCoeffs * SizeOf(Single));
    Move(Self.FY^, FY^, FNumberOfCoeffs * SizeOf(Single));
    Move(Self.FStateStack^, FStateStack^, 2 * FNumberOfCoeffs * SizeOf(Single));
   end else
 if Dest is TPolyphaseUpsampler64 then
  with TPolyphaseUpsampler64(Dest) do
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

procedure TPolyphaseUpsampler32.AllocateBuffer;
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

procedure TPolyphaseUpsampler32.ChooseProcedures;
begin
 case FNumberOfCoeffs of
    1: FProcessSample32 := ProcessSample1;
    2: FProcessSample32 := ProcessSample2;
    3: FProcessSample32 := ProcessSample3;
    4: FProcessSample32 := ProcessSample4;
  else
   if FNumberOfCoeffs mod 2 <> 0
    then FProcessSample32 := ProcessSampleOdd
    else FProcessSample32 := ProcessSampleEven;
 end;
end;

procedure TPolyphaseUpsampler32.PushStates;
begin
 Move(FX[0], FStateStack[0], FNumberOfCoeffs * SizeOf(Single));
 Move(FY[0], FStateStack[FNumberOfCoeffs], FNumberOfCoeffs * SizeOf(Single));
end;

procedure TPolyphaseUpsampler32.PopStates;
begin
 Move(FStateStack[0], FX[0], FNumberOfCoeffs * SizeOf(Single));
 Move(FStateStack[FNumberOfCoeffs], FY[0], FNumberOfCoeffs * SizeOf(Single));
end;

procedure TPolyphaseUpsampler32.ClearBuffers;
begin
 FillChar(FX^, FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FY^, FNumberOfCoeffs * SizeOf(Single), 0);
end;

procedure TPolyphaseUpsampler32.ResetStates;
begin
  FillChar(FX^, FNumberOfCoeffs * SizeOf(Single), 0);
  FillChar(FY^, FNumberOfCoeffs * SizeOf(Single), 0);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ProcessBlock                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Downsamples (x2) a block of samples.                                    //
//    Input and output blocks may overlap, see assert() for details.          //
//                                                                            //
//  Input parameters:                                                         //
//    - Input: Input array, containing SampleFrames * 2 samples.              //
//    - SampleFrames: Number of samples to output, > 0                        //
//                                                                            //
//  Output parameters:                                                        //
//    - Output: Array for the output samples, capacity: SampleFrames samples. //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler32.ProcessBlock(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Pos : Integer;
begin
 for Pos := 0 to SampleFrames - 1
  do FProcessSample32(Input[pos], PDAV2SingleArray(@Output[pos * 2])^);
end;

procedure TPolyphaseUpsampler32.ProcessSample1(const Input: Single; out Output: TDAV2SingleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0];
 FX[0] := Input;
 Output[0] := FY[0];
 Output[1] := Input;
{$ELSE}
asm
    PUSH    EDI
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     ECX, [EAX.FX]            // ECX = FX
    MOV     EAX, [EAX.FCoefficients] // EAX = FCoefficients
    FLD     [ECX].Single             // FX[0]
    FLD     Input.Single             // Input, FX[0]
    FST     [Output + 4].Single      // Output[1] := Input;
    FST     [ECX].Single             // FX[0] := Input;
    FSUB    [EDI].Single             // (Input - FY[0])
    FMUL    [EAX].Double             // (Input - FY[0]) * FCoefficients[0]
    FADDP                            // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FST     [EDI].Single             // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [Output].Single          // Output[1] := FY[3];
    POP     EDI
{$ENDIF}
end;

procedure TPolyphaseUpsampler32.ProcessSample2(const Input: Single; out Output: TDAV2SingleArray);
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
 LocalY^[0] := (Input - LocalY^[0]) * Coeffs^[0] + LocalX^[0];
 LocalX^[0] := Input;
 LocalY^[1] := (Input - LocalY^[1]) * Coeffs^[1] + LocalX^[1];
 LocalX^[1] := Input;

 // set output
 Output[0] := LocalY^[0];
 Output[1] := LocalY^[1];
{$ELSE}
asm
    PUSH    EDI
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     ECX, [EAX.FX]            // ESI = FX
    MOV     EAX, [EAX.FCoefficients] // ECX = FCoefficients

    FLD     [ECX].Single             // FX[0]
    FLD     Input.Single             // Input, FX[0]
    FST     [ECX].Single             // FX[0] := Input;
    FSUB    [EDI].Single             // (Input - FY[0])
    FMUL    [EAX].Double             // (Input - FY[0]) * FCoefficients[0]
    FADDP                            // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FST     [EDI].Single             // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [Output].Single          // Output[1] := FY[3];

    FLD     [ECX + 4].Single         // FX[1]
    FLD     Input.Single             // Input, FX[1]
    FST     [ECX + 4].Single         // FX[1] := Input;
    FSUB    [EDI + 4].Single         // (Input - FY[1])
    FMUL    [EAX + 8].Double         // (Input - FY[1]) * FCoefficients[1]
    FADDP                            // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FST     [EDI + 4].Single         // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [Output + 4].Single      // Output[1] := FY[1];

    POP     EDI
{$ENDIF}
end;

procedure TPolyphaseUpsampler32.ProcessSample3(const Input: Single; out Output: TDAV2SingleArray);
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

 // calculate filter
 LocalY[0] := (Input - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input;
 LocalY[1] := (Input - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input;
 LocalY[2] := (LocalY[0] - LocalY[2]) * Coeffs[2] + LocalX[2];
 LocalX[2] := LocalY[0];

 // set output
 Output[1] := LocalY[1];
 Output[0] := LocalY[2];
{$ELSE}
asm
    PUSH    EDI
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     ECX, [EAX.FX]            // ESI = FX
    MOV     EAX, [EAX.FCoefficients] // ECX = FCoefficients

    FLD     [ECX].Single             // FX[0]
    FLD     Input.Single             // Input, FX[0]
    FST     [ECX].Single             // FX[0] := Input;
    FSUB    [EDI].Single             // (Input - FY[0])
    FMUL    [EAX].Double             // (Input - FY[0]) * FCoefficients[0]
    FADDP                            // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Single             // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]

    FLD     [ECX + 4].Single         // FX[1]
    FLD     Input.Single             // Input, FX[1]
    FST     [ECX + 4].Single         // FX[1] := Input;
    FSUB    [EDI + 4].Single         // (Input - FY[1])
    FMUL    [EAX + 8].Double         // (Input - FY[1]) * FCoefficients[1]
    FADDP                            // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FST     [EDI + 4].Single         // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [Output + 4].Single      // Output[1] := FY[1];

    FLD     [ECX +  8].Single        // FX[2]
    FLD     [EDI].Single             // FY[0], FX[2]
    FST     [ECX +  8].Single        // FX[2] := FY[0];
    FSUB    [EDI +  8].Single        // (FY[0] - FY[2])
    FMUL    [EAX + 16].Double        // (FY[0] - FY[2]) * FCoefficients[2]
    FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FST     [EDI +  8].Single        // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output].Single          // Output[0] := FY[2];

    POP     EDI
{$ENDIF}
end;

procedure TPolyphaseUpsampler32.ProcessSample4(const Input: Single; out Output: TDAV2SingleArray);
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

 // calculate filter
 LocalY[0] := (Input - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input;
 LocalY[1] := (Input - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input;
 LocalY[2] := (LocalY[0] - LocalY[2]) * Coeffs[2] + LocalX[2];
 LocalX[2] := LocalY[0];
 LocalY[3] := (LocalY[1] - LocalY[3]) * Coeffs[3] + LocalX[3];
 LocalX[3] := LocalY[1];

 // set output
 Output[0] := LocalY[2];
 Output[1] := LocalY[3];
{$ELSE}
asm
    PUSH    EDI
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     ECX, [EAX.FX]            // ESI = FX
    MOV     EAX, [EAX.FCoefficients] // ECX = FCoefficients

    FLD     [ECX].Single             // FX[0]
    FLD     Input.Single             // Input, FX[0]
    FST     [ECX].Single             // FX[0] := Input; FX[0]
    FSUB    [EDI].Single             // (Input - FY[0]), FX[0]
    FMUL    [EAX].Double             // (Input - FY[0]) * FCoefficients[0], FX[0]
    FADDP                            // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Single             // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]

    FLD     [ECX + 4].Single         // FX[1]
    FLD     Input.Single             // Input, FX[1]
    FST     [ECX + 4].Single         // FX[1] := Input;
    FSUB    [EDI + 4].Single         // (Input - FY[1])
    FMUL    [EAX + 8].Double         // (Input - FY[1]) * FCoefficients[1]
    FADDP                            // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [EDI + 4].Single         // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]

    FLD     [ECX +  8].Single        // FX[2]
    FLD     [EDI].Single             // FY[0], FX[2]
    FST     [ECX +  8].Single        // FX[2] := FY[0];
    FSUB    [EDI +  8].Single        // (FY[0] - FY[2])
    FMUL    [EAX + 16].Double        // (FY[0] - FY[2]) * FCoefficients[2]
    FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FST     [EDI + 8].Single         // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output].Single          // Output[0] := FY[2];

    FLD     [ECX + 12].Single        // FX[2], FY[2]
    FLD     [EDI +  4].Single        // FY[0], FX[2], FY[2]
    FST     [ECX + 12].Single        // FX[2] := FY[0];
    FSUB    [EDI + 12].Single        // (FY[0] - FY[2]), FY[2]
    FMUL    [EAX + 24].Double        // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI + 12].Single        // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output + 4].Single      // Output[1] := FY[3];

    POP     EDI
{$ENDIF}
end;

procedure TPolyphaseUpsampler32.ProcessSampleOdd(const Input: Single; out Output: TDAV2SingleArray);
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

 // calculate first filters
 LocalY^[0] := (Input - LocalY^[0]) * Coeffs^[0] + LocalX^[0];
 LocalX^[0] := Input;
 LocalY^[1] := (Input - LocalY^[1]) * Coeffs^[1] + LocalX^[1];
 LocalX^[1] := Input;

 // calculate remaining filters
 for Index := 2 to FNumberOfCoeffs - 1 do
  begin
   FY[Index] := (FY[Index - 2] - FY[Index]) * FCoefficients[Index] + FX[Index];
   FX[Index] :=  FY[Index - 2];
  end;

 Output[1] := FY[FNumberOfCoeffs - 2];
 Output[0] := FY[FNumberOfCoeffs - 1];
{$ELSE}
asm
    PUSHAD
    MOV     ESI, [EAX.FX]              // ESI = FX
    MOV     EDI, [EAX.FY]              // EDI = FY
    MOV     EBX, [EAX.FCoefficients]   // EBX = FCoefficients

    FLD     [ESI].Single               // FX[0]
    FLD     Input.Single               // Input, FX[0]
    FST     [ESI].Single               // FX[0] := Input;
    FSUB    [EDI].Single               // (Input - FY[0])
    FMUL    [EBX].Double               // (Input - FY[0]) * FCoefficients[0]
    FADDP                              // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Single               // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]

    FLD     [ESI + 4].Single           // FX[1]
    FLD     Input.Single               // Input, FX[1]
    FST     [ESI + 4].Single           // FX[1] := Input;
    FSUB    [EDI + 4].Single           // (Input - FY[1])
    FMUL    [EBX + 8].Double           // (Input - FY[1]) * FCoefficients[1]
    FADDP                              // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [EDI + 4].Single           // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]

    PUSH    ECX                        // store ECX on stack
    MOV     ECX, [EAX.FNumberOfCoeffs] // ECX = self.FNumberOfCoeffs
    SUB     ECX, 4                     // subtract first and last two filters from count

@StartLoop:
    FLD     [ESI +  8].Single          // FX[2], FY[2]
    FLD     [EDI].Single               // FY[0], FX[2], FY[2]
    FST     [ESI +  8].Single          // FX[2] := FY[0];
    FSUB    [EDI +  8].Single          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 16].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FSTP    [EDI +  8].Single          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    ADD     ESI, 4                     // advance FX pointer
    ADD     EDI, 4                     // advance FY pointer
    ADD     EBX, 8                     // advance FCoefficient pointer
    LOOP    @StartLoop

    POP     ECX                        // restore ECX from stack

    FLD     [ESI +  8].Single          // FX[2], FY[2]
    FLD     [EDI].Single               // FY[0], FX[2], FY[2]
    FST     [ESI +  8].Single          // FX[2] := FY[0];
    FSUB    [EDI +  8].Single          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 16].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI +  8].Single          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output + 4].Single        // Output[0] := FY[2];

    FLD     [ESI + 12].Single          // FX[2], FY[2]
    FLD     [EDI +  4].Single          // FY[0], FX[2], FY[2]
    FST     [ESI + 12].Single          // FX[2] := FY[0];
    FSUB    [EDI + 12].Single          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 24].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI + 12].Single          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output].Single            // Output[1] := FY[3];

    POPAD
{$ENDIF}
end;

procedure TPolyphaseUpsampler32.ProcessSampleEven(const Input: Single; out Output: TDAV2SingleArray);
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

 // calculate first filters
 LocalY^[0] := (Input - LocalY^[0]) * Coeffs^[0] + LocalX^[0];
 LocalX^[0] := Input;
 LocalY^[1] := (Input - LocalY^[1]) * Coeffs^[1] + LocalX^[1];
 LocalX^[1] := Input;

 // calculate remaining filters
 for Index := 2 to FNumberOfCoeffs - 1 do
  begin
   FY[Index] := (FY[Index - 2] - FY[Index]) * FCoefficients[Index] + FX[Index];
   FX[Index] :=  FY[Index - 2];
  end;

 Output[0] := FY[FNumberOfCoeffs - 2];
 Output[1] := FY[FNumberOfCoeffs - 1];
{$ELSE}
asm
    PUSHAD
    MOV     ESI, [EAX.FX]              // ESI = FX
    MOV     EDI, [EAX.FY]              // EDI = FY
    MOV     EBX, [EAX.FCoefficients]   // EBX = FCoefficients

    FLD     [ESI].Single               // FX[0]
    FLD     Input.Single               // Input, FX[0]
    FST     [ESI].Single               // FX[0] := Input;
    FSUB    [EDI].Single               // (Input - FY[0])
    FMUL    [EBX].Double               // (Input - FY[0]) * FCoefficients[0]
    FADDP                              // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Single               // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]

    FLD     [ESI + 4].Single           // FX[1]
    FLD     Input.Single               // Input, FX[1]
    FST     [ESI + 4].Single           // FX[1] := Input;
    FSUB    [EDI + 4].Single           // (Input - FY[1])
    FMUL    [EBX + 8].Double           // (Input - FY[1]) * FCoefficients[1]
    FADDP                              // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [EDI + 4].Single           // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]

    PUSH    ECX                        // store ECX on stack
    MOV     ECX, [EAX.FNumberOfCoeffs] // ECX = self.FNumberOfCoeffs
    SUB     ECX, 4                     // subtract first and last two filters from count

@StartLoop:
    FLD     [ESI +  8].Single          // FX[2], FY[2]
    FLD     [EDI].Single               // FY[0], FX[2], FY[2]
    FST     [ESI +  8].Single          // FX[2] := FY[0];
    FSUB    [EDI +  8].Single          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 16].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FSTP    [EDI +  8].Single          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    ADD     ESI, 4                     // advance FX pointer
    ADD     EDI, 4                     // advance FY pointer
    ADD     EBX, 8                     // advance FCoefficient pointer
    LOOP    @StartLoop

    POP     ECX                        // restore ECX from stack

    FLD     [ESI +  8].Single          // FX[2], FY[2]
    FLD     [EDI].Single               // FY[0], FX[2], FY[2]
    FST     [ESI +  8].Single          // FX[2] := FY[0];
    FSUB    [EDI +  8].Single          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 16].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI +  8].Single          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output].Single            // Output[0] := FY[2];

    FLD     [ESI + 12].Single          // FX[2], FY[2]
    FLD     [EDI +  4].Single          // FY[0], FX[2], FY[2]
    FST     [ESI + 12].Single          // FX[2] := FY[0];
    FSUB    [EDI + 12].Single          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 24].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI + 12].Single          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output + 4].Single        // Output[1] := FY[3];

    POPAD
{$ENDIF}
end;

procedure TPolyphaseUpsampler32.ProcessSample32(Input: Single;
  out Output: TDAV2SingleArray);
begin
 FProcessSample32(Input, Output);
end;


{ TPolyphaseUpsampler64 }

constructor TPolyphaseUpsampler64.Create;
begin
 FX          := nil;
 FY          := nil;
 FStateStack := nil;
 inherited;
end;

constructor TPolyphaseUpsampler64.Create(const NumberOfCoefficients: Integer;
  const Transition: Double);
begin
 FX          := nil;
 FY          := nil;
 FStateStack := nil;
 inherited;
end;

destructor TPolyphaseUpsampler64.Destroy;
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

procedure TPolyphaseUpsampler64.AssignTo(Dest: TPersistent);
var
  Index : Integer;
begin
 inherited;

 if Dest is TPolyphaseUpsampler64 then
  with TPolyphaseUpsampler64(Dest) do
   begin
    inherited;
    FProcessSample64 := Self.FProcessSample64;

    Assert(FNumberOfCoeffs = Self.FNumberOfCoeffs);
    Move(Self.FX^, FX^, FNumberOfCoeffs * SizeOf(Double));
    Move(Self.FY^, FY^, FNumberOfCoeffs * SizeOf(Double));
    Move(Self.FStateStack^, FStateStack^, 2 * FNumberOfCoeffs * SizeOf(Double));
   end else
 if Dest is TPolyphaseUpsampler32 then
  with TPolyphaseUpsampler32(Dest) do
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

procedure TPolyphaseUpsampler64.AllocateBuffer;
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

procedure TPolyphaseUpsampler64.ChooseProcedures;
begin
 case FNumberOfCoeffs of
    1: FProcessSample64 := ProcessSample1;
    2: FProcessSample64 := ProcessSample2;
    3: FProcessSample64 := ProcessSample3;
    4: FProcessSample64 := ProcessSample4;
  else
   if FNumberOfCoeffs mod 2 <> 0
    then FProcessSample64 := ProcessSampleOdd
    else FProcessSample64 := ProcessSampleEven;
 end;
end;

procedure TPolyphaseUpsampler64.PushStates;
begin
 Move(FX[0], FStateStack[0], FNumberOfCoeffs * SizeOf(Double));
 Move(FY[0], FStateStack[FNumberOfCoeffs], FNumberOfCoeffs * SizeOf(Double));
end;

procedure TPolyphaseUpsampler64.PopStates;
begin
 Move(FStateStack[0], FX[0], FNumberOfCoeffs * SizeOf(Double));
 Move(FStateStack[FNumberOfCoeffs], FY[0], FNumberOfCoeffs * SizeOf(Double));
end;

procedure TPolyphaseUpsampler64.ClearBuffers;
begin
 FillChar(FX[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FY[0], FNumberOfCoeffs * SizeOf(Double), 0);
end;

procedure TPolyphaseUpsampler64.ResetStates;
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
//    Input and output blocks may overlap, see assert() for details.          //
//                                                                            //
//  Input parameters:                                                         //
//    - Input: Input array, containing SampleFrames * 2 samples.              //
//    - SampleFrames: Number of samples to output, > 0                        //
//                                                                            //
//  Output parameters:                                                        //
//    - Output: Array for the output samples, capacity: SampleFrames samples. //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler64.ProcessBlock(const Input, Output: PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  Pos : Integer;
begin
 for Pos := 0 to SampleFrames - 1
  do FProcessSample64(Input[pos], PDAV2DoubleArray(@Output[pos * 2])^);
end;

procedure TPolyphaseUpsampler64.ProcessSample1(const Input: Double; out Output: TDAV2DoubleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0];
 FX[0] := Input;
 Output[0] := FY[0];
 Output[1] := Input;
{$ELSE}
asm
    PUSHAD
    MOV     ESI, [EAX.FX]            // ESI = FX
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     EBX, [EAX.FCoefficients] // ECX = FCoefficients
    FLD     [ESI].Double             // FX[0]
    FLD     Input.Double             // Input, FX[0]
    FST     [Output + 4].Single      // Output[1] := Input;
    FST     [ESI].Double             // FX[0] := Input;
    FSUB    [EDI].Double             // (Input - FY[0])
    FMUL    [EBX].Double             // (Input - FY[0]) * FCoefficients[0]
    FADDP                            // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FST     [EDI].Double             // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [Output].Double          // Output[1] := FY[3];
    POPAD
{$ENDIF}
end;

procedure TPolyphaseUpsampler64.ProcessSample2(const Input: Double; out Output: TDAV2DoubleArray);
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
 LocalY^[0] := (Input - LocalY^[0]) * Coeffs^[0] + LocalX^[0];
 LocalX^[0] := Input;
 LocalY^[1] := (Input - LocalY^[1]) * Coeffs^[1] + LocalX^[1];
 LocalX^[1] := Input;

 // set output
 Output[0] := LocalY^[0];
 Output[1] := LocalY^[1];
{$ELSE}
asm
    PUSHAD
    MOV     ESI, [EAX.FX]            // ESI = FX
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     EBX, [EAX.FCoefficients] // ECX = FCoefficients

    FLD     [ESI].Double             // FX[0]
    FLD     Input.Double             // Input, FX[0]
    FST     [ESI].Double             // FX[0] := Input;
    FSUB    [EDI].Double             // (Input - FY[0])
    FMUL    [EBX].Double             // (Input - FY[0]) * FCoefficients[0]
    FADDP                            // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FST     [EDI].Double             // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [Output].Double          // Output[1] := FY[3];

    FLD     [ESI + 8].Double         // FX[1]
    FLD     Input.Single             // Input, FX[1]
    FST     [ESI + 8].Double         // FX[1] := Input;
    FSUB    [EDI + 8].Double         // (Input - FY[1])
    FMUL    [EBX + 8].Double         // (Input - FY[1]) * FCoefficients[1]
    FADDP                            // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FST     [EDI + 8].Double         // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [Output + 8].Double      // Output[1] := FY[1];

    POPAD
{$ENDIF}
end;

procedure TPolyphaseUpsampler64.ProcessSample3(const Input: Double; out Output: TDAV2DoubleArray);
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
 LocalY[0] := (Input - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input;
 LocalY[1] := (Input - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input;
 LocalY[2] := (LocalY[0] - LocalY[2]) * Coeffs[2] + LocalX[2];
 LocalX[2] := LocalY[0];

 // set output
 Output[1] := LocalY[1];
 Output[0] := LocalY[2];
{$ELSE}
asm
    PUSHAD
    MOV     ESI, [EAX.FX]            // ESI = FX
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     EBX, [EAX.FCoefficients] // ECX = FCoefficients

    FLD     [ESI].Double             // FX[0]
    FLD     Input.Double             // Input, FX[0]
    FST     [ESI].Double             // FX[0] := Input;
    FSUB    [EDI].Double             // (Input - FY[0])
    FMUL    [EBX].Double             // (Input - FY[0]) * FCoefficients[0]
    FADDP                            // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Double             // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]

    FLD     [ESI + 8].Double         // FX[1]
    FLD     Input.Single             // Input, FX[1]
    FST     [ESI + 8].Double         // FX[1] := Input;
    FSUB    [EDI + 8].Double         // (Input - FY[1])
    FMUL    [EBX + 8].Double         // (Input - FY[1]) * FCoefficients[1]
    FADDP                            // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FST     [EDI + 8].Double         // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [Output + 8].Double      // Output[1] := FY[1];

    FLD     [ESI + 16].Double        // FX[2]
    FLD     [EDI].Double             // FY[0], FX[2]
    FST     [ESI + 16].Double        // FX[2] := FY[0];
    FSUB    [EDI + 16].Double        // (FY[0] - FY[2])
    FMUL    [EBX + 16].Double        // (FY[0] - FY[2]) * FCoefficients[2]
    FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FST     [EDI + 16].Double        // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output].Double          // Output[0] := FY[2];

    POPAD
{$ENDIF}
end;

procedure TPolyphaseUpsampler64.ProcessSample4(const Input: Double; out Output: TDAV2DoubleArray);
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
 LocalY[0] := (Input - LocalY[0]) * Coeffs[0] + LocalX[0];
 LocalX[0] := Input;
 LocalY[1] := (Input - LocalY[1]) * Coeffs[1] + LocalX[1];
 LocalX[1] := Input;
 LocalY[2] := (LocalY[0] - LocalY[2]) * Coeffs[2] + LocalX[2];
 LocalX[2] := LocalY[0];
 LocalY[3] := (LocalY[1] - LocalY[3]) * Coeffs[3] + LocalX[3];
 LocalX[3] := LocalY[1];

 // set output
 Output[0] := LocalY[2];
 Output[1] := LocalY[3];
{$ELSE}
asm
    PUSHAD
    MOV     ESI, [EAX.FX]            // ESI = FX
    MOV     EDI, [EAX.FY]            // EDI = FY
    MOV     EBX, [EAX.FCoefficients] // ECX = FCoefficients

    FLD     [ESI].Double             // FX[0]
    FLD     Input.Double             // Input, FX[0]
    FST     [ESI].Double             // FX[0] := Input;
    FSUB    [EDI].Double             // (Input - FY[0])
    FMUL    [EBX].Double             // (Input - FY[0]) * FCoefficients[0]
    FADDP                            // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Double             // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]

    FLD     [ESI + 8].Double         // FX[1]
    FLD     Input.Double             // Input, FX[1]
    FST     [ESI + 8].Double         // FX[1] := Input;
    FSUB    [EDI + 8].Double         // (Input - FY[1])
    FMUL    [EBX + 8].Double         // (Input - FY[1]) * FCoefficients[1]
    FADDP                            // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [EDI + 8].Double         // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]

    FLD     [ESI + 16].Double        // FX[2]
    FLD     [EDI].Double             // FY[0], FX[2]
    FST     [ESI + 16].Double        // FX[2] := FY[0];
    FSUB    [EDI + 16].Double        // (FY[0] - FY[2])
    FMUL    [EBX + 16].Double        // (FY[0] - FY[2]) * FCoefficients[2]
    FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FST     [EDI + 16].Double        // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output].Double          // Output[0] := FY[2];

    FLD     [ESI + 24].Double        // FX[2], FY[2]
    FLD     [EDI + 8].Double         // FY[0], FX[2], FY[2]
    FST     [ESI + 24].Double        // FX[2] := FY[0];
    FSUB    [EDI + 24].Double        // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 24].Double        // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                            // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI + 24].Double        // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output + 8].Double      // Output[1] := FY[3];

    POPAD
{$ENDIF}
end;

procedure TPolyphaseUpsampler64.ProcessSampleOdd(const Input: Double; out Output: TDAV2DoubleArray);
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

 // calculate first filters
 LocalY^[0] := (Input - LocalY^[0]) * Coeffs^[0] + LocalX^[0];
 LocalX^[0] := Input;
 LocalY^[1] := (Input - LocalY^[1]) * Coeffs^[1] + LocalX^[1];
 LocalX^[1] := Input;

 // calculate remaining filters
 for Index := 2 to FNumberOfCoeffs - 1 do
  begin
   FY[Index] := (FY[Index - 2] - FY[Index]) * FCoefficients[Index] + FX[Index];
   FX[Index] :=  FY[Index - 2];
  end;

 Output[1] := FY[FNumberOfCoeffs - 2];
 Output[0] := FY[FNumberOfCoeffs - 1];
{$ELSE}
asm
    PUSHAD
    MOV     ESI, [EAX.FX]              // ESI = FX
    MOV     EDI, [EAX.FY]              // EDI = FY
    MOV     EBX, [EAX.FCoefficients]   // EBX = FCoefficients

    FLD     [ESI].Double               // FX[0]
    FLD     Input.Double               // Input, FX[0]
    FST     [ESI].Double               // FX[0] := Input;
    FSUB    [EDI].Double               // (Input - FY[0])
    FMUL    [EBX].Double               // (Input - FY[0]) * FCoefficients[0]
    FADDP                              // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Double               // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]

    FLD     [ESI + 8].Double           // FX[1]
    FLD     Input.Double               // Input, FX[1]
    FST     [ESI + 8].Double           // FX[1] := Input;
    FSUB    [EDI + 8].Double           // (Input - FY[1])
    FMUL    [EBX + 8].Double           // (Input - FY[1]) * FCoefficients[1]
    FADDP                              // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [EDI + 8].Double           // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]

    PUSH    ECX                        // store ECX on stack
    MOV     ECX, [EAX.FNumberOfCoeffs] // ECX = self.FNumberOfCoeffs
    SUB     ECX, 4                     // subtract first and last two filters from count

@StartLoop:
    FLD     [ESI + 16].Double          // FX[2], FY[2]
    FLD     [EDI].Double               // FY[0], FX[2], FY[2]
    FST     [ESI + 16].Double          // FX[2] := FY[0];
    FSUB    [EDI + 16].Double          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 16].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FSTP    [EDI + 16].Double          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    ADD     ESI, 8                     // advance FX pointer
    ADD     EDI, 8                     // advance FY pointer
    ADD     EBX, 8                     // advance FCoefficient pointer
    LOOP    @StartLoop

    POP     ECX                        // restore ECX from stack

    FLD     [ESI + 16].Double          // FX[2], FY[2]
    FLD     [EDI].Double               // FY[0], FX[2], FY[2]
    FST     [ESI + 16].Double          // FX[2] := FY[0];
    FSUB    [EDI + 16].Double          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 16].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI + 16].Double          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output + 8].Double        // Output[0] := FY[2];

    FLD     [ESI + 24].Double          // FX[2], FY[2]
    FLD     [EDI +  8].Double          // FY[0], FX[2], FY[2]
    FST     [ESI + 24].Double          // FX[2] := FY[0];
    FSUB    [EDI + 24].Double          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 24].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI + 24].Double          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output].Double            // Output[1] := FY[3];

    POPAD
{$ENDIF}
end;

procedure TPolyphaseUpsampler64.ProcessSampleEven(const Input: Double; out Output: TDAV2DoubleArray);
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

 // calculate first filters
 LocalY^[0] := (Input - LocalY^[0]) * Coeffs^[0] + LocalX^[0];
 LocalX^[0] := Input;
 LocalY^[1] := (Input - LocalY^[1]) * Coeffs^[1] + LocalX^[1];
 LocalX^[1] := Input;

 // calculate remaining filters
 for Index := 2 to FNumberOfCoeffs - 1 do
  begin
   FY[Index] := (FY[Index - 2] - FY[Index]) * FCoefficients[Index] + FX[Index];
   FX[Index] :=  FY[Index - 2];
  end;

 Output[0] := FY[FNumberOfCoeffs - 2];
 Output[1] := FY[FNumberOfCoeffs - 1];
{$ELSE}
asm
    PUSHAD
    MOV     ESI, [EAX.FX]              // ESI = FX
    MOV     EDI, [EAX.FY]              // EDI = FY
    MOV     EBX, [EAX.FCoefficients]   // EBX = FCoefficients

    FLD     [ESI].Double               // FX[0]
    FLD     Input.Double               // Input, FX[0]
    FST     [ESI].Double               // FX[0] := Input;
    FSUB    [EDI].Double               // (Input - FY[0])
    FMUL    [EBX].Double               // (Input - FY[0]) * FCoefficients[0]
    FADDP                              // (Input - FY[0]) * FCoefficients[0] + FX[0]
    FSTP    [EDI].Double               // FY[0] := (Input - FY[0]) * FCoefficients[0] + FX[0]

    FLD     [ESI + 8].Double           // FX[1]
    FLD     Input.Double               // Input, FX[1]
    FST     [ESI + 8].Double           // FX[1] := Input;
    FSUB    [EDI + 8].Double           // (Input - FY[1])
    FMUL    [EBX + 8].Double           // (Input - FY[1]) * FCoefficients[1]
    FADDP                              // (Input - FY[1]) * FCoefficients[1] + FX[1]
    FSTP    [EDI + 8].Double           // FY[1] := (Input - FY[1]) * FCoefficients[1] + FX[1]

    PUSH    ECX                        // store ECX on stack
    MOV     ECX, [EAX.FNumberOfCoeffs] // ECX = self.FNumberOfCoeffs
    SUB     ECX, 4                     // subtract first and last two filters from count

@StartLoop:
    FLD     [ESI + 16].Double          // FX[2], FY[2]
    FLD     [EDI].Double               // FY[0], FX[2], FY[2]
    FST     [ESI + 16].Double          // FX[2] := FY[0];
    FSUB    [EDI + 16].Double          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 16].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FSTP    [EDI + 16].Double          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    ADD     ESI, 8                     // advance FX pointer
    ADD     EDI, 8                     // advance FY pointer
    ADD     EBX, 8                     // advance FCoefficient pointer
    LOOP    @StartLoop

    POP     ECX                        // restore ECX from stack

    FLD     [ESI + 16].Double          // FX[2], FY[2]
    FLD     [EDI].Double               // FY[0], FX[2], FY[2]
    FST     [ESI + 16].Double          // FX[2] := FY[0];
    FSUB    [EDI + 16].Double          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 16].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI + 16].Double          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output].Double            // Output[0] := FY[2];

    FLD     [ESI + 24].Double          // FX[2], FY[2]
    FLD     [EDI +  8].Double          // FY[0], FX[2], FY[2]
    FST     [ESI + 24].Double          // FX[2] := FY[0];
    FSUB    [EDI + 24].Double          // (FY[0] - FY[2]), FY[2]
    FMUL    [EBX + 24].Double          // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
    FADDP                              // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
    FST     [EDI + 24].Double          // FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
    FSTP    [Output + 8].Double        // Output[1] := FY[3];

    POPAD
{$ENDIF}
end;

procedure TPolyphaseUpsampler64.ProcessSample64(Input: Double;
  out Output: TDAV2DoubleArray);
begin
 FProcessSample64(Input, Output);
end;

end.
