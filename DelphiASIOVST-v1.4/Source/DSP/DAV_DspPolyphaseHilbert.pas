unit DAV_DspPolyphaseHilbert;

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

{$IFDEF CPUx86_64}
  {$DEFINE PUREPASCAL}
{$ENDIF}

uses
  DAV_Types, DAV_DspPolyphaseFilter;

type
  TPhaseMem32 = array [0..1] of
    record
      X : PDAVSingleFixedArray;
      Y : PDAVSingleFixedArray;
    end;
  TPhaseMem64 = array [0..1] of
    record
      X : PDAVDoubleFixedArray;
      Y : PDAVDoubleFixedArray;
    end;
  TProcessHilbertSample32 = procedure(const Input: Single; out OutputA, OutputB: Single) of object;
  TProcessEnvelopeSample32 = function(const Input: Single): Single of object;
  TProcessHilbertSample64 = procedure(const Input: Double; out OutputA, OutputB: Double) of object;
  TProcessEnvelopeSample64 = function(const Input: Double): Double of object;

  TCustomPhaseHalfPi = class(TCustomPolyphaseFilter);

  TPhaseHalfPi32 = class(TCustomPhaseHalfPi)
  private
    FPHilbertSample32 : TProcessHilbertSample32;
    FPEnvSample32     : TProcessEnvelopeSample32;
    FPrev             : Single;
    FPhase            : Integer;
    FMem              : TPhaseMem32;
  protected
    procedure ChooseProcedures; override;
    procedure NumberOfCoeffsChanged; override;
    procedure ProcessSampleLarge(const Input: Single;  out OutputA, OutputB: Single); overload;
    procedure ProcessSample1(const Input: Single; out OutputA, OutputB: Single); overload;
    procedure ProcessSample2(const Input: Single; out OutputA, OutputB: Single); overload;
    procedure ProcessSample3(const Input: Single; out OutputA, OutputB: Single); overload;
    procedure ProcessSample4(const Input: Single; out OutputA, OutputB: Single); overload;
    function ProcessSampleLarge(const Input: Single): Single; overload;
    function ProcessSample1(const Input: Single): Single; overload;
    function ProcessSample2(const Input: Single): Single; overload;
    function ProcessSample3(const Input: Single): Single; overload;
    function ProcessSample4(const Input: Single): Single; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, OutputA, OutputB: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure ClearBuffers;

    property ProcessHilbertSample: TProcessHilbertSample32 read FPHilbertSample32;
    property ProcessEnvelopeSample: TProcessEnvelopeSample32 read FPEnvSample32;
  end;

  TPhaseHalfPi64 = class(TCustomPhaseHalfPi)
  private
    FPHilbertSample64 : TProcessHilbertSample64;
    FPEnvSample64     : TProcessEnvelopeSample64;
    FPrev             : Double;
    FPhase            : Integer;      // 0 or 1
    FMem              : TPhaseMem64;
  protected
    procedure ChooseProcedures; override;
    procedure NumberOfCoeffsChanged; override;
    procedure ProcessSampleLarge(const Input: Double;  out OutputA, OutputB: Double); overload;
    procedure ProcessSample1(const Input: Double; out OutputA, OutputB: Double); overload;
    procedure ProcessSample2(const Input: Double; out OutputA, OutputB: Double); overload;
    procedure ProcessSample3(const Input: Double; out OutputA, OutputB: Double); overload;
    procedure ProcessSample4(const Input: Double; out OutputA, OutputB: Double); overload;
    function ProcessSampleLarge(const Input: Double): Double; overload;
    function ProcessSample1(const Input: Double): Double; overload;
    function ProcessSample2(const Input: Double): Double; overload;
    function ProcessSample3(const Input: Double): Double; overload;
    function ProcessSample4(const Input: Double): Double; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, OutputA, OutputB: PDAVDoubleFixedArray; SampleFrames: Integer);
    procedure ClearBuffers;

    property ProcessHilbertSample: TProcessHilbertSample64 read FPHilbertSample64;
    property ProcessEnvelopeSample: TProcessEnvelopeSample64 read FPEnvSample64;
  end;

implementation

uses
  DAV_Common;

{$IFDEF HandleDenormals}
var
  CDenorm32 : Single;
  CDenorm64 : Double;
{$ENDIF}

constructor TPhaseHalfPi32.Create;
begin
  inherited;
  FMem[0].X := nil;
  FMem[0].Y := nil;
  FMem[1].X := nil;
  FMem[1].Y := nil;
  NumberOfCoeffsChanged;
end;

destructor TPhaseHalfPi32.Destroy;
begin
  Dispose(FMem[0].X);
  Dispose(FMem[0].Y);
  Dispose(FMem[1].X);
  Dispose(FMem[1].Y);
  inherited;
end;

procedure TPhaseHalfPi32.ChooseProcedures;
begin
  case FNumberOfCoeffs of
    1 :
     begin
      FPHilbertSample32 := ProcessSample1;
      FPEnvSample32 := ProcessSample1;
     end;
    2 :
     begin
      FPHilbertSample32 := ProcessSample2;
      FPEnvSample32 := ProcessSample2;
     end;
    3 :
     begin
      FPHilbertSample32 := ProcessSample3;
      FPEnvSample32 := ProcessSample3;
     end;
    4 :
     begin
      FPHilbertSample32 := ProcessSample4;
      FPEnvSample32 := ProcessSample4;
     end;
  else
   begin
    FPHilbertSample32 := ProcessSampleLarge;
    FPEnvSample32 := ProcessSampleLarge;
   end;
   end;
end;

procedure TPhaseHalfPi32.NumberOfCoeffsChanged;
begin
 inherited;
 ReallocMem(FMem[0].X, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FMem[0].Y, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FMem[1].X, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FMem[1].Y, FNumberOfCoeffs * SizeOf(Single));
 ChooseProcedures;
 ClearBuffers;
end;


procedure TPhaseHalfPi32.ProcessBlock(
  const Input, OutputA, OutputB: PDAVSingleFixedArray; SampleFrames: Integer);
var
  Pos: Integer;
begin
  Assert(SampleFrames > 0);
  Pos := 0;
  repeat
    ProcessHilbertSample(Input[pos], OutputA[Pos], OutputB[Pos]);
    Inc(Pos);
  until (pos >= SampleFrames);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ClearBuffers                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Clears filter memory, as if it processed silence since an infinite      //
//    amount of time.                                                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPhaseHalfPi32.ClearBuffers;
begin
 FillChar(FMem[0].X[0], FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FMem[0].Y[0], FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FMem[0].X[0], FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FMem[0].Y[0], FNumberOfCoeffs * SizeOf(Single), 0);
end;


procedure TPhaseHalfPi32.ProcessSample1(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                        // push EBX on stack
    PUSH    EDI                        // push EDI on stack
    PUSH    ESI                        // push ESI on stack
    MOV     EBX, [EAX.FPhase]          // EBX = FPhase
    SHL     EBX, 3                     // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]     // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4] // ESI = Y[0]
    SHR     EBX, 3                     // EBX = FPhase
    XOR     EBX, $1                    // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX          // FPhase = EBX

    FLD     Input.Single               // Input
    FLD     [EDI].Single               // X[0], Input
    FLD     Input.Single               // Input, X[0],Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32
    {$ENDIF}
    FST     [EDI].Single               // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single               // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX = FCoefficients
    FMUL    [EBX].Double               // (Input + Y[0]) * FCoefficients[0], X[0]
    FSUBRP                             // (Input + Y[0]) * FCoefficients[0] - X[0]
    FST     [ESI].Single               // FMem[FPhase].Y[0] := "
    FSTP    OutputA.Single             // OutputA := FMem[FPhase].Y[0];
    FLD     [EAX.FPrev].Single         // FPrev, Input
    FSTP    OutputB.Single             // OutputB := FPrev;
    FSTP    [EAX.FPrev].Single         // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  OutputA := FMem[FPhase].Y[0];
  OutputB := FPrev;
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi32.ProcessSample2(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                        // push EBX on stack
    PUSH    EDI                        // push EDI on stack
    PUSH    ESI                        // push ESI on stack
    MOV     EBX, [EAX.FPhase]          // EBX = FPhase
    SHL     EBX, 3                     // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]     // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4] // ESI = Y[0]
    SHR     EBX, 3                     // EBX = FPhase
    XOR     EBX, $1                    // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX          // FPhase = EBX
    {$IFDEF HandleDenormals}
    FLD     CDenorm32                  // Pure Speed
    {$ELSE}
    FLDZ
    {$ENDIF}

    FLD     Input.Single               // Input
    FLD     [EDI].Single               // X[0], Input
    FLD     Input.Single               // Input, X[0],Input
    {$IFDEF HandleDenormals}
    FADD    ST(0),ST(3)                // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Single               // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single               // Input + Y[0], X[0], Input
    MOV     EBX,[EAX.FCoefficients]    // EBX = FCoefficients
    FMUL    [EBX].Double               // (Input + Y[0]) * FCoefficients[0], X[0], Input
    FSUBRP                             // (Input + Y[0]) * FCoefficients[0] - X[0], Input
    FST     [ESI].Single               // FMem[FPhase].Y[0] := "
    FSTP    OutputA.Single             // OutputA := FMem[FPhase].Y[0];

    FLD     [EDI + 4].Single           // X[1], Input
    FLD     [EAX.FPrev].Single         // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)               // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 4].Single           // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 4].Single           // FPrev + Y[1], X[1], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX = FCoefficients
    FMUL    [EBX + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                             // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FST     [ESI + 4].Single           // FMem[FPhase].Y[1] := "
    FSTP    OutputB.Single             // OutputB := FMem[FPhase].Y[1];
    FSTP    [EAX.FPrev].Single         // FPrev := Input;

    FSTP    ST(0)
    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  OutputA := FMem[FPhase].Y[0];
  OutputB := FMem[FPhase].Y[1];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi32.ProcessSample3(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                        // push EBX on stack
    PUSH    EDI                        // push EDI on stack
    PUSH    ESI                        // push ESI on stack
    MOV     EBX, [EAX.FPhase]          // EBX = FPhase
    SHL     EBX, 3                     // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]     // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4] // ESI = Y[0]
    SHR     EBX, 3                     // EBX = FPhase
    XOR     EBX, $1                    // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX          // FPhase = EBX
    {$IFDEF HandleDenormals}
    FLD     CDenorm32                  // Pure Speed
    {$ELSE}
    FLDZ
    {$ENDIF}

    FLD     Input.Single               // Input
    FLD     [EDI].Single               // X[0], Input
    FLD     Input.Single               // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)               // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Single               // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single               // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX = FCoefficients
    FMUL    [EBX].Double               // (Input + Y[0]) * FCoefficients[0], X[0], Input
    FSUBRP                             // (Input + Y[0]) * FCoefficients[0] - X[0], Input
    FSTP    [ESI].Single               // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 4].Single           // X[1], Input
    FLD     [EAX.FPrev].Single         // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)               // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 4].Single           // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 4].Single           // FPrev + Y[1], X[1], Input
    FMUL    [EBX + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                             // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FST     [ESI + 4].Single           // FMem[FPhase].Y[1] :=  "
    FSTP    OutputA.Single             // OutputB := FMem[FPhase].Y[1];

    FLD     [EDI +  8].Single          // X[2], Input
    FLD     [ESI     ].Single          // Y[0], X[2], Input
    FST     [EDI +  8].Single          // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI +  8].Single          // Y[2] + Y[0], X[2], Input
    FMUL    [EBX + 16].Double          // (Y[0] + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                             // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
    FST     [ESI +  8].Single          // FMem[FPhase].Y[2] :=  "
    FSTP    OutputB.Single             // OutputB := FMem[FPhase].Y[2];

    FSTP    [EAX.FPrev].Single         // FPrev := Input;
    FSTP    ST(0)

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
var
  MemY : PDAV4SingleArray;
  MemX : PDAV4SingleArray;
begin
  MemY  := @FMem[FPhase].Y[0];
  MemX  := @FMem[FPhase].X[0];
  MemY[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    MemY[0]) * PDAV4DoubleArray(FCoefficients)[0] - MemX[0]; MemX[0] := Input;
  MemY[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    MemY[1]) * PDAV4DoubleArray(FCoefficients)[1] - MemX[1]; MemX[1] := FPrev;
  MemY[2] := (MemY[0] + MemY[2]) * PDAV4DoubleArray(FCoefficients)[2] - MemX[2];           MemX[2] := MemY[0];
  OutputA := MemY[1];
  OutputB := MemY[2];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi32.ProcessSample4(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX
    {$IFDEF HandleDenormals}
    FLD     CDenorm32                   // store on stack for pure speed
    {$ENDIF}

    FLD     Input.Single                // Input
    FLD     [EDI].Single                // X[0], Input
    FLD     Input.Single                // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)                // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Single                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single                // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]    // EDX = FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0], Input
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0], Input
    FSTP    [ESI].Single                // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 4].Single            // X[1], Input
    FLD     [EAX.FPrev].Single          // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)                // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 4].Single            // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 4].Single            // FPrev + Y[1], X[1], Input
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FSTP    [ESI + 4].Single            // FMem[FPhase].Y[1] :=  "

    FLD     [EDI +  8].Single           // X[2], Input
    FLD     [ESI].Single                // Y[0], X[2], Input
    FST     [EDI +  8].Single           // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI +  8].Single           // Y[2] + Y[0], X[2], Input
    FMUL    [EBX + 16].Double           // (Y[0] + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                              // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
    FST     [ESI +  8].Single           // FMem[FPhase].Y[2] :=  "
    FSTP    OutputA.Single              // OutputB := FMem[FPhase].Y[2];

    FLD     [EDI + 12].Single           // X[3], Input
    FLD     [ESI +  4].Single           // X[1], X[3], Input
    FST     [EDI + 12].Single           // FMem[FPhase].X[3] := X[1];
    FADD    [ESI + 12].Single           // FPrev + Y[3], X[3], Input
    FMUL    [EBX + 24].Double           // (FPrev + Y[3]) * FCoefficients[3], X[3]
    FSUBRP                              // (FPrev + Y[3]) * FCoefficients[3] - X[3]
    FST     [ESI + 12].Single           // FMem[FPhase].Y[3] :=  "
    FSTP    OutputB.Single              // OutputB := FMem[FPhase].Y[3];

    FSTP    [EAX.FPrev].Single          // FPrev := Input;
    {$IFDEF HandleDenormals}
    FSTP    ST(0)                       // pop denormal
    {$ENDIF}
    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
var
  MemY : PDAV4SingleArray;
  MemX : PDAV4SingleArray;
begin
  MemY  := @FMem[FPhase].Y[0];
  MemX  := @FMem[FPhase].X[0];
  MemY[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    MemY[0]) * PDAV4DoubleArray(FCoefficients)^[0] - MemX[0]; MemX[0] := Input;
  MemY[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    MemY[1]) * PDAV4DoubleArray(FCoefficients)^[1] - MemX[1]; MemX[1] := FPrev;
  MemY[2] := (MemY[0] + MemY[2]) * PDAV4DoubleArray(FCoefficients)^[2] - MemX[2]; MemX[2] := MemY[0];
  MemY[3] := (MemY[1] + MemY[3]) * PDAV4DoubleArray(FCoefficients)^[3] - MemX[3]; MemX[3] := MemY[1];
  OutputA := MemY[2];
  OutputB := MemY[3];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

procedure TPhaseHalfPi32.ProcessSampleLarge(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX

    {$IFDEF HandleDenormals}
    FLD     CDenorm32                   // Pure Speed
    {$ELSE}
    FLDZ
    {$ENDIF}

    FLD     Input.Single                // Input
    FLD     [EDI].Single                // X[0], Input
    FLD     Input.Single                // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)                // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Single                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single                // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]    // EBX = FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0],Input
    FSTP    [ESI].Single                // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 4].Single            // X[1], Input
    FLD     [EAX.FPrev].Single          // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)                // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 4].Single            // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 4].Single            // FPrev + Y[1], X[1], Input
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FSTP    [ESI + 4].Single            // FMem[FPhase].Y[1] :=  "

    PUSH    ECX                         // push ECX on stack
    MOV     ECX, [EAX.FNumberOfCoeffs]  // ECX = Self.FNumberOfCoeffs
    SUB     ECX, 4                      // "Den Rest mach ich selber"
@Loopy:
    FLD     [EDI +  8].Single           // X[2], Input
    FLD     [ESI].Single                // Y[0], X[2], Input
    FST     [EDI +  8].Single           // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI +  8].Single           // FPrev + Y[2], X[2], Input
    FMUL    [EBX + 16].Double           // (FPrev + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                              // (FPrev + Y[2]) * FCoefficients[2] - X[2]
    FSTP    [ESI +  8].Single           // FMem[FPhase].Y[2] :=  "
    ADD     ESI, 4
    ADD     EDI, 4
    ADD     EBX, 8
    LOOP    @Loopy
    POP     ECX                         // restore ECX

    FLD     [EDI + 8].Single            // X[10], Input
    FLD     [ESI].Single                // X[8], X[10], Input
    FST     [EDI + 8].Single            // FMem[FPhase].X[10] := X[8];
    FADD    [ESI + 8].Single            // FPrev + Y[10], X[8], Input
    FMUL    [EBX + 16].Double           // (FPrev + Y[10]) * FCoefficients[10], X[10]
    FSUBRP                              // (FPrev + Y[10]) * FCoefficients[10] - X[10]
    FST     [ESI + 8].Single            // FMem[FPhase].Y[10] :=  "
    FSTP    [ECX].Single                // OutputB := FMem[FPhase].Y[10];

    FLD     [EDI + 12].Single           // X[11], Input
    FLD     [ESI +  4].Single           // X[9], X[11], Input
    FST     [EDI + 12].Single           // FMem[FPhase].X[11] := X[9];
    FADD    [ESI + 12].Single           // FPrev + Y[11], X[9], Input
    FMUL    [EBX + 24].Double           // (FPrev + Y[11]) * FCoefficients[11], X[11]
    FSUBRP                              // (FPrev + Y[11]) * FCoefficients[11] - X[11]
    FST     [ESI + 12].Single           // FMem[FPhase].Y[11] :=  "
    FSTP    [EDX].Single                // OutputB := FMem[FPhase].Y[11];

    FSTP    [EAX.FPrev].Single          // FPrev := Input;
    FSTP    ST(0)
    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) * FCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  OutputA := FMem[FPhase].Y[FNumberOfCoeffs - 2];
  OutputB := FMem[FPhase].Y[FNumberOfCoeffs - 1];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample1(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX

    FLD     Input.Single                // Input
    FLD     [EDI].Single                // X[0], Input
    FLD     Input.Single                // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32
    {$ENDIF}
    FST     [EDI].Single                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single                // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]    // EDX = FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0]
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0]
    FST     [ESI].Single                // FMem[FPhase].Y[0] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[0]), Input
    FLD     [EAX.FPrev].Single          // FPrev, Sqr(FMem[FPhase].Y[0]), Input
    FMUL    ST(0), ST(0)                // Sqr(FPrev), Sqr(FMem[FPhase].Y[0]), Input
    FADDP                               // Sqr(FPrev) + Sqr(FMem[FPhase].Y[0]), Input
    FSQRT                               // Sqrt(Sqr(FPrev) + Sqr(FMem[FPhase].Y[0])), Input
    FXCH                                // Input, Sqrt(Sqr(FPrev) + Sqr(FMem[FPhase].Y[0]))
    FSTP    [EAX.FPrev].Single          // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  Result := Sqrt(Sqr(FMem[FPhase].Y[0]) + Sqr(FPrev));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample2(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX

    FLD     Input.Single                // Input
    FLD     [EDI].Single                // X[0], Input
    FLD     Input.Single                // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Single                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single                // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]    // EBX = FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0],Input
    FST     [ESI].Single                // FMem[FPhase].Y[0] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[0]);

    FLD     [EDI + 4].Single            // X[1], Input
    FLD     [EAX.FPrev].Single          // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 4].Single            // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 4].Single            // FPrev + Y[1], X[1], Input
    MOV     EBX, [EAX.FCoefficients]    // EDX = FCoefficients
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FST     [ESI + 4].Single            // FMem[FPhase].Y[1] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[1]);
    FADDP                               // Sqr(FMem[FPhase].Y[0]) + Sqr(FMem[FPhase].Y[1]
    FSQRT                               // Result := Sqrt(Sqr(FMem[FPhase].Y[0]) + Sqr(FMem[FPhase].Y[1]));
    FXCH                                // Input, Result
    FSTP    [EAX.FPrev].Single          // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  Result := Sqrt(Sqr(FMem[FPhase].Y[0]) + Sqr(FMem[FPhase].Y[1]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample3(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX

    FLD     Input.Single                // Input
    FLD     [EDI].Single                // X[0],Input
    FLD     Input.Single                // Input,X[0],Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Single                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single                // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]    // EDX = FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0],Input
    FSTP    [ESI].Single                // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 4].Single            // X[1], Input
    FLD     [EAX.FPrev].Single          // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 4].Single            // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 4].Single            // FPrev + Y[1], X[1], Input
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FST     [ESI + 4].Single            // FMem[FPhase].Y[1] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[1]);

    FLD     [EDI +  8].Single           // X[2], Input
    FLD     [ESI].Single                // Y[0], X[2], Input
    FST     [EDI +  8].Single           // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI +  8].Single           // Y[2] + Y[0], X[2], Input
    FMUL    [EBX + 16].Double           // (Y[0] + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                              // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
    FST     [ESI +  8].Single           // FMem[FPhase].Y[2] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[2]);
    FADDP                               // Sqr(FMem[FPhase].Y[1]) + Sqr(FMem[FPhase].Y[2]
    FSQRT                               // Result := Sqrt(Sqr(FMem[FPhase].Y[1]) + Sqr(FMem[FPhase].Y[2]));
    FXCH                                // Input, Result
    FSTP    [EAX.FPrev].Single          // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) *
    FCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  Result := Sqrt(Sqr(FMem[FPhase].Y[1]) + Sqr(FMem[FPhase].Y[2]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample4(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX

    FLD     Input.Single                // Input
    FLD     [EDI].Single                // X[0], Input
    FLD     Input.Single                // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Single                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single                // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]    // EDX = FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0],Input
    FSTP    [ESI].Single                // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 4].Single            // X[1], Input
    FLD     [EAX.FPrev].Single          // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 4].Single            // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 4].Single            // FPrev + Y[1], X[1], Input
    MOV     EBX, [EAX.FCoefficients]    // EDX=FCoefficients
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FSTP    [ESI + 4].Single            // FMem[FPhase].Y[1] :=  "

    FLD     [EDI + 8].Single            // X[2], Input
    FLD     [ESI].Single                // Y[0], X[2], Input
    FST     [EDI + 8].Single            // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI + 8].Single            // Y[2] + Y[0], X[2], Input
    FMUL    [EBX + 16].Double           // (Y[0] + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                              // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
    FST     [ESI + 8].Single            // FMem[FPhase].Y[2] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[3]);

    FLD     [EDI + 12].Single           // X[3], Input
    FLD     [ESI + 4].Single            // X[1], X[3], Input
    FST     [EDI + 12].Single           // FMem[FPhase].X[3] := X[1];
    FADD    [ESI + 12].Single           // FPrev + Y[3], X[3], Input
    FMUL    [EBX + 24].Double           // (FPrev + Y[3]) * FCoefficients[3], X[3]
    FSUBRP                              // (FPrev + Y[3]) * FCoefficients[3] - X[3]
    FST     [ESI + 12].Single           // FMem[FPhase].Y[3] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[3]);
    FADDP                               // Sqr(FMem[FPhase].Y[3]) + Sqr(FMem[FPhase].Y[4])
    FSQRT                               // Result := Sqrt(Sqr(FMem[FPhase].Y[3]) + Sqr(FMem[FPhase].Y[4]));
    FXCH                                // Input, Result
    FSTP    [EAX.FPrev].Single          // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) *
    FCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  FMem[FPhase].Y[3] := (FMem[FPhase].Y[1] + FMem[FPhase].Y[3]) *
    FCoefficients[3] - FMem[FPhase].X[3];
  FMem[FPhase].X[3] := FMem[FPhase].Y[1];
  Result := Sqrt(Sqr(FMem[FPhase].Y[2]) + Sqr(FMem[FPhase].Y[3]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

function TPhaseHalfPi32.ProcessSampleLarge(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX

    FLD     Input.Single                // Input
    FLD     [EDI].Single                // X[0], Input
    FLD     Input.Single                // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Single                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Single                // Input + Y[0], X[0], Input
    MOV     EBX,[EAX.FCoefficients]     // EBX = FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0],Input
    FSTP    [ESI].Single                // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 4].Single            // X[1], Input
    FLD     [EAX.FPrev].Single          // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm32                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 4].Single            // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 4].Single            // FPrev + Y[1], X[1], Input
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FSTP    [ESI + 4].Single            // FMem[FPhase].Y[1] :=  "

    PUSH    ECX                         // push ECX on stack
    MOV     ECX, [EAX.FNumberOfCoeffs]  // ECX = Self.FNumberOfCoeffs
    SUB     ECX, 4                      // "Den Rest mach ich selber"
@Loopy:
    FLD     [EDI +  8].Single           // X[2], Input
    FLD     [ESI].Single                // Y[0], X[2], Input
    FST     [EDI +  8].Single           // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI +  8].Single           // FPrev + Y[2], X[2], Input
    FMUL    [EBX + 16].Double           // (FPrev + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                              // (FPrev + Y[2]) * FCoefficients[2] - X[2]
    FSTP    [ESI +  8].Single           // FMem[FPhase].Y[2] :=  "
    ADD     ESI, 4
    ADD     EDI, 4
    ADD     EBX, 8
    LOOP    @Loopy
    POP     ECX                         // restore ECX

    FLD     [EDI + 8].Single            // X[10], Input
    FLD     [ESI].Single                // X[8], X[10], Input
    FST     [EDI + 8].Single            // FMem[FPhase].X[10] := X[8];
    FADD    [ESI + 8].Single            // FPrev + Y[10], X[8], Input
    FMUL    [EBX + 16].Double           // (FPrev + Y[10]) * FCoefficients[10], X[10]
    FSUBRP                              // (FPrev + Y[10]) * FCoefficients[10] - X[10]
    FST     [ESI + 8].Single            // FMem[FPhase].Y[10] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[10]);

    FLD     [EDI + 12].Single           // X[11], Input
    FLD     [ESI + 4].Single            // X[9], X[11], Input
    FST     [EDI + 12].Single           // FMem[FPhase].X[11] := X[9];
    FADD    [ESI + 12].Single           // FPrev + Y[11], X[9], Input
    FMUL    [EBX + 24].Double           // (FPrev + Y[11]) * FCoefficients[11], X[11]
    FSUBRP                              // (FPrev + Y[11]) * FCoefficients[11] - X[11]
    FST     [ESI + 12].Single           // FMem[FPhase].Y[11] :=  "

    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[11])
    FADDP                               // Sqr(FMem[FPhase].Y[10]) + Sqr(FMem[FPhase].Y[11]);
    FSQRT                               // Result := Sqrt(Sqr(FMem[FPhase].Y[10]) + Sqr(FMem[FPhase].Y[11]));
    FXCH                                // Input, Result
    FSTP    [EAX.FPrev].Single          // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) *
      FCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  Result := Sqrt(Sqr(FMem[FPhase].Y[FNumberOfCoeffs - 2]) + Sqr(FMem[FPhase].Y[FNumberOfCoeffs - 1]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}


{ TPhaseHalfPi64 }

constructor TPhaseHalfPi64.Create;
begin
  inherited;
  FMem[0].X := nil;
  FMem[0].Y := nil;
  FMem[1].X := nil;
  FMem[1].Y := nil;
  NumberOfCoeffsChanged;
end;

destructor TPhaseHalfPi64.Destroy;
begin
  Dispose(FMem[0].X);
  Dispose(FMem[0].Y);
  Dispose(FMem[1].X);
  Dispose(FMem[1].Y);
  inherited;
end;

procedure TPhaseHalfPi64.ChooseProcedures;
begin
  case FNumberOfCoeffs of
    1 :
     begin
      FPHilbertSample64 := ProcessSample1;
      FPEnvSample64 := ProcessSample1;
     end;
    2 :
     begin
      FPHilbertSample64 := ProcessSample2;
      FPEnvSample64 := ProcessSample2;
     end;
    3 :
     begin
      FPHilbertSample64 := ProcessSample3;
      FPEnvSample64 := ProcessSample3;
     end;
    4 :
     begin
      FPHilbertSample64 := ProcessSample4;
      FPEnvSample64 := ProcessSample4;
     end;
  else
   begin
    FPHilbertSample64 := ProcessSampleLarge;
    FPEnvSample64 := ProcessSampleLarge;
   end;
   end;
end;

procedure TPhaseHalfPi64.NumberOfCoeffsChanged;
begin
 inherited;
 ReallocMem(FMem[0].X, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FMem[0].Y, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FMem[1].X, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FMem[1].Y, FNumberOfCoeffs * SizeOf(Double));
 ChooseProcedures;
 ClearBuffers;
end;


procedure TPhaseHalfPi64.ProcessBlock(
  const Input, OutputA, OutputB: PDAVDoubleFixedArray; SampleFrames: Integer);
var
  Pos: Integer;
begin
  Assert(SampleFrames > 0);
  Pos := 0;
  repeat
    ProcessHilbertSample(Input[pos], OutputA[Pos], OutputB[Pos]);
    Inc(Pos);
  until (pos >= SampleFrames);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ClearBuffers                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Clears filter memory, as if it processed silence since an infinite      //
//    amount of time.                                                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPhaseHalfPi64.ClearBuffers;
begin
 FillChar(FMem[0].X[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FMem[0].Y[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FMem[0].X[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FMem[0].Y[0], FNumberOfCoeffs * SizeOf(Double), 0);
end;


procedure TPhaseHalfPi64.ProcessSample1(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                        // push EBX on stack
    PUSH    EDI                        // push EDI on stack
    PUSH    ESI                        // push ESI on stack
    MOV     EBX, [EAX.FPhase]          // EBX = FPhase
    SHL     EBX, 3                     // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]     // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4] // ESI = Y[0]
    SHR     EBX, 3                     // EBX = FPhase
    XOR     EBX, $1                    // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX          // FPhase = EBX

    FLD     Input.Double               // Input
    FLD     [EDI].Double               // X[0],Input
    FLD     Input.Double               // Input,X[0],Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                  // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Double               // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Double               // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX = FCoefficients
    FMUL    [EBX].Double               // (Input + Y[0]) * FCoefficients[0], X[0]
    FSUBRP                             // (Input + Y[0]) * FCoefficients[0] - X[0]
    FST     [ESI].Double               // FMem[FPhase].Y[0] := "
    FSTP    OutputA.Double             // OutputA := FMem[FPhase].Y[0];
    FLD     [EAX.FPrev].Double         // FPrev, Input
    FSTP    OutputB.Double             // OutputB := FPrev;
    FSTP    [EAX.FPrev].Double         // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  OutputA := FMem[FPhase].Y[0];
  OutputB := FPrev;
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi64.ProcessSample2(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                        // push EBX on stack
    PUSH    EDI                        // push EDI on stack
    PUSH    ESI                        // push ESI on stack
    MOV     EBX, [EAX.FPhase]          // EBX = FPhase
    SHL     EBX, 3                     // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]     // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4] // ESI = Y[0]
    SHR     EBX, 3                     // EBX = FPhase
    XOR     EBX, $1                    // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX          // FPhase = EBX
    {$IFDEF HandleDenormals}
    FLD     CDenorm64                  // Pure Speed
    {$ENDIF}

    FLD     Input.Double               // Input
    FLD     [EDI].Double               // X[0], Input
    FLD     Input.Double               // Input, X[0],Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)               // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Double               // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Double               // Input + Y[0], X[0], Input
    MOV     EBX,[EAX.FCoefficients]    // EBX = FCoefficients
    FMUL    [EBX].Double               // (Input + Y[0]) * FCoefficients[0], X[0], Input
    FSUBRP                             // (Input + Y[0]) * FCoefficients[0] - X[0], Input
    FST     [ESI].Double               // FMem[FPhase].Y[0] := "
    FSTP    OutputA.Double             // OutputA := FMem[FPhase].Y[0];

    FLD     [EDI + 8].Double           // X[1], Input
    FLD     [EAX.FPrev].Double         // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)               // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 8].Double           // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 8].Double           // FPrev + Y[1], X[1], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX = FCoefficients
    FMUL    [EBX + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                             // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FST     [ESI + 8].Double           // FMem[FPhase].Y[1] := "
    FSTP    OutputB.Double             // OutputB := FMem[FPhase].Y[1];
    FSTP    [EAX.FPrev].Double         // FPrev := Input;

    {$IFDEF HandleDenormals}
    FSTP     ST(0)
    {$ENDIF}

    POP      ESI
    POP      EDI
    POP      EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  OutputA := FMem[FPhase].Y[0];
  OutputB := FMem[FPhase].Y[1];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi64.ProcessSample3(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase],EBX            // FPhase = EBX
    {$IFDEF HandleDenormals}
    FLD     CDenorm64                   // Pure Speed
    {$ELSE}
    FLDZ
    {$ENDIF}

    FLD     Input.Double               // Input
    FLD     [EDI].Double               // X[0], Input
    FLD     Input.Double               // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)               // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Double               // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Double               // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX = FCoefficients
    FMUL    [EBX].Double               // (Input + Y[0]) * FCoefficients[0], X[0], Input
    FSUBRP                             // (Input + Y[0]) * FCoefficients[0] - X[0], Input
    FSTP    [ESI].Double               // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 8].Double           // X[1], Input
    FLD     [EAX.FPrev].Double         // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)               // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 8].Double           // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 8].Double           // FPrev + Y[1], X[1], Input
    FMUL    [EBX + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                             // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FST     [ESI + 8].Double           // FMem[FPhase].Y[1] :=  "
    FSTP    OutputA.Double             // OutputB := FMem[FPhase].Y[1];

    FLD     [EDI + 16].Double          // X[2], Input
    FLD     [ESI].Double               // Y[0], X[2], Input
    FST     [EDI + 16].Double          // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI + 16].Double          // Y[2] + Y[0], X[2], Input
    FMUL    [EBX + 16].Double          // (Y[0] + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                             // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
    FST     [ESI + 16].Double          // FMem[FPhase].Y[2] :=  "
    FSTP    OutputB.Double             // OutputB := FMem[FPhase].Y[2];

    FSTP    [EAX.FPrev].Double         // FPrev := Input;
    FSTP    ST(0)

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * FCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  OutputA := FMem[FPhase].Y[1];
  OutputB := FMem[FPhase].Y[2];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi64.ProcessSample4(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX
    {$IFDEF HandleDenormals}
    FLD     CDenorm64                   // Pure Speed
    {$ELSE}
    FLDZ
    {$ENDIF}

    FLD     Input.Double                // Input
    FLD     [EDI].Double                // X[0], Input
    FLD     Input.Double                // Input, X[0], Input
    FST     [EDI].Double                // FMem[FPhase].X[0] := Input;
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)                // dEnOrMaL + Input, X[0], Input
    {$ENDIF}
    FADD    [ESI].Double                // dEnOrMaL + Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]    // EDX = FCoefficients
    FMUL    [EBX].Double                // (dEnOrMaL + Input + Y[0]) * FCoefficients[0], X[0], Input
    FSUBRP                              // (dEnOrMaL + Input + Y[0]) * FCoefficients[0] - X[0], Input
    FSTP    [ESI].Double                // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 8].Double            // X[1], Input
    FLD     [EAX.FPrev].Double          // FPrev, X[1], Input
    FST     [EDI + 8].Double            // FMem[FPhase].X[1] := FPrev;
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)                // dEnOrMaL
    {$ENDIF}
    FADD    [ESI + 8].Double            // FPrev + Y[1], X[1], Input
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FSTP    [ESI + 8].Double            // FMem[FPhase].Y[1] :=  "

    FLD     [EDI + 16].Double           // X[2], Input
    FLD     [ESI].Double                // Y[0], X[2], Input
    FST     [EDI + 16].Double           // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI + 16].Double           // Y[2] + Y[0], X[2], Input
    FMUL    [EBX + 16].Double           // (Y[0] + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                              // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
    FST     [ESI + 16].Double           // FMem[FPhase].Y[2] :=  "
    FSTP    OutputA.Double              // OutputB := FMem[FPhase].Y[2];

    FLD     [EDI + 24].Double           // X[3], Input
    FLD     [ESI +  8].Double           // X[1], X[3], Input
    FST     [EDI + 24].Double           // FMem[FPhase].X[3] := X[1];
    FADD    [ESI + 24].Double           // FPrev + Y[3], X[3], Input
    FMUL    [EBX + 24].Double           // (FPrev + Y[3]) * FCoefficients[3], X[3]
    FSUBP                               // (FPrev + Y[3]) * FCoefficients[3] - X[3]
    FST     [ESI + 24].Double           // FMem[FPhase].Y[3] :=  "
    FSTP    OutputB.Double              // OutputB := FMem[FPhase].Y[3];

    FSTP    [EAX.FPrev].Double          // FPrev := Input;
    FSTP    ST(0)
    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * FCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  FMem[FPhase].Y[3] := (FMem[FPhase].Y[1] + FMem[FPhase].Y[3]) * FCoefficients[3] - FMem[FPhase].X[3];
  FMem[FPhase].X[3] := FMem[FPhase].Y[1];
  OutputA := FMem[FPhase].Y[2];
  OutputB := FMem[FPhase].Y[3];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

procedure TPhaseHalfPi64.ProcessSampleLarge(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX

    {$IFDEF HandleDenormals}
    FLD     CDenorm64                   // Pure Speed
    {$ELSE}
    FLDZ
    {$ENDIF}

    FLD     Input.Double                // Input
    FLD     [EDI].Double                // X[0], Input
    FLD     Input.Double                // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)                // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Double                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Double                // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]    // EBX = FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0],Input
    FSTP    [ESI].Double                // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 8].Double            // X[1], Input
    FLD     [EAX.FPrev].Double          // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    ST(0), ST(3)                // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 8].Double            // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 8].Double            // FPrev + Y[1], X[1], Input
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FSTP    [ESI + 8].Double            // FMem[FPhase].Y[1] :=  "

    PUSH    ECX                         // push ECX on stack
    MOV     ECX, [EAX.FNumberOfCoeffs]  // ECX = Self.FNumberOfCoeffs
    SUB     ECX, 4                      // "Den Rest mach ich selber"

@Loopy:
    FLD     [EDI + 16].Double           // X[2], Input
    FLD     [ESI].Double                // Y[0], X[2], Input
    FST     [EDI + 16].Double           // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI + 16].Double           // FPrev + Y[2], X[2], Input
    FMUL    [EBX + 16].Double           // (FPrev + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                              // (FPrev + Y[2]) * FCoefficients[2] - X[2]
    FSTP    [ESI + 16].Double           // FMem[FPhase].Y[2] :=  "
    ADD     ESI, 8
    ADD     EDI, 8
    ADD     EBX, 8
    LOOP    @Loopy
    POP     ECX                         // restore ECX

    FLD     [EDI + 16].Double           // X[10], Input
    FLD     [ESI].Double                // X[8], X[10], Input
    FST     [EDI + 16].Double           // FMem[FPhase].X[10] := X[8];
    FADD    [ESI + 16].Double           // FPrev + Y[10], X[8], Input
    FMUL    [EBX + 16].Double           // (FPrev + Y[10]) * FCoefficients[10], X[10]
    FSUBRP                              // (FPrev + Y[10]) * FCoefficients[10] - X[10]
    FST     [ESI + 16].Double           // FMem[FPhase].Y[10] :=  "
    FSTP    [ECX].Double                // OutputB := FMem[FPhase].Y[10];

    FLD     [EDI + 24].Double           // X[11], Input
    FLD     [ESI +  8].Double           // X[9], X[11], Input
    FST     [EDI + 24].Double           // FMem[FPhase].X[11] := X[9];
    FADD    [ESI + 24].Double           // FPrev + Y[11], X[9], Input
    FMUL    [EBX + 24].Double           // (FPrev + Y[11]) * FCoefficients[11], X[11]
    FSUBRP                              // (FPrev + Y[11]) * FCoefficients[11] - X[11]
    FST     [ESI + 24].Double           // FMem[FPhase].Y[11] :=  "
    FSTP    [EDX].Double                // OutputB := FMem[FPhase].Y[11];

    FSTP    [EAX.FPrev].Double          // FPrev := Input;
    FSTP    ST(0)
    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) * FCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  OutputA := FMem[FPhase].Y[FNumberOfCoeffs - 2];
  OutputB := FMem[FPhase].Y[FNumberOfCoeffs - 1];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample1(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                        // push EBX on stack
    PUSH    EDI                        // push EDI on stack
    PUSH    ESI                        // push ESI on stack
    MOV     EBX, [EAX.FPhase]          // EBX = FPhase
    SHL     EBX, 3                     // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]     // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4] // ESI = Y[0]
    SHR     EBX, 3                     // EBX = FPhase
    XOR     EBX, $1                    // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX          // FPhase = EBX

    FLD     Input.Double               // Input
    FLD     [EDI].Double               // X[0], Input
    FLD     Input.Double               // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                  // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Double               // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Double               // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX = FCoefficients
    FMUL    [EBX].Double               // (Input + Y[0]) * FCoefficients[0], X[0]
    FSUBRP                             // (Input + Y[0]) * FCoefficients[0] - X[0]
    FST     [ESI].Double               // FMem[FPhase].Y[0] :=  "
    FMUL    ST(0), ST(0)               // Sqr(FMem[FPhase].Y[0]), Input
    FLD     [EAX.FPrev].Double         // FPrev, Sqr(FMem[FPhase].Y[0]), Input
    FMUL    ST(0), ST(0)               // Sqr(FPrev), Sqr(FMem[FPhase].Y[0]), Input
    FADDP                              // Sqr(FPrev) + Sqr(FMem[FPhase].Y[0]), Input
    FSQRT                              // Sqrt(Sqr(FPrev) + Sqr(FMem[FPhase].Y[0])), Input
    FXCH                               // Input, Sqrt(Sqr(FPrev) + Sqr(FMem[FPhase].Y[0]))
    FSTP    [EAX.FPrev].Double         // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  Result := Sqrt(Sqr(FMem[FPhase].Y[0]) + Sqr(FPrev));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample2(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase],EBX            // FPhase = EBX

    FLD     Input.Double                // Input
    FLD     [EDI].Double                // X[0],Input
    FLD     Input.Double                // Input,X[0],Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Double                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Double                // Input + Y[0], X[0], Input
    MOV     EBX,[EAX.FCoefficients]     // EBX=FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0],Input
    FST     [ESI].Double                // FMem[FPhase].Y[0] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[0]);

    FLD     [EDI + 8].Double            // X[1], Input
    FLD     [EAX.FPrev].Double          // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 8].Double            // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 8].Double            // FPrev + Y[1], X[1], Input
    MOV     EBX,[EAX.FCoefficients]     // EDX = FCoefficients
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FST     [ESI + 8].Double            // FMem[FPhase].Y[1] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[1]);
    FADDP                               // Sqr(FMem[FPhase].Y[0]) + Sqr(FMem[FPhase].Y[1]
    FSQRT                               // Result := Sqrt(Sqr(FMem[FPhase].Y[0]) + Sqr(FMem[FPhase].Y[1]));
    FXCH                                // Input, Result
    FSTP    [EAX.FPrev].Double          // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  Result := Sqrt(Sqr(FMem[FPhase].Y[0]) + Sqr(FMem[FPhase].Y[1]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample3(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                        // push EBX on stack
    PUSH    EDI                        // push EDI on stack
    PUSH    ESI                        // push ESI on stack
    MOV     EBX, [EAX.FPhase]          // EBX = FPhase
    SHL     EBX, 3                     // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]     // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4] // ESI = Y[0]
    SHR     EBX, 3                     // EBX = FPhase
    XOR     EBX, $1                    // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX          // FPhase = EBX

    FLD     Input.Double               // Input
    FLD     [EDI].Double               // X[0],Input
    FLD     Input.Double               // Input,X[0],Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                  // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Double               // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Double               // Input + Y[0], X[0], Input
    MOV     EBX,[EAX.FCoefficients]    // EDX=FCoefficients
    FMUL    [EBX].Double               // (Input + Y[0]) * FCoefficients[0], X[0],Input
    FSUBRP                             // (Input + Y[0]) * FCoefficients[0] - X[0],Input
    FSTP    [ESI].Double               // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 8].Double           // X[1], Input
    FLD     [EAX.FPrev].Double         // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                  // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 8].Double           // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 8].Double           // FPrev + Y[1], X[1], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX=FCoefficients
    FMUL    [EBX + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                             // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FST     [ESI + 8].Double           // FMem[FPhase].Y[1] :=  "
    FMUL    ST(0), ST(0)               // Sqr(FMem[FPhase].Y[1]);

    FLD     [EDI + 16].Double          // X[2], Input
    FLD     [ESI].Double               // Y[0], X[2], Input
    FST     [EDI + 16].Double          // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI + 16].Double          // Y[2] + Y[0], X[2], Input
    FMUL    [EBX + 16].Double          // (Y[0] + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                             // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
    FST     [ESI + 16].Double          // FMem[FPhase].Y[2] :=  "
    FMUL    ST(0), ST(0)               // Sqr(FMem[FPhase].Y[2]);
    FADDP                              // Sqr(FMem[FPhase].Y[1]) + Sqr(FMem[FPhase].Y[2]
    FSQRT                              // Result := Sqrt(Sqr(FMem[FPhase].Y[1]) + Sqr(FMem[FPhase].Y[2]));
    FXCH                               // Input, Result
    FSTP    [EAX.FPrev].Double         // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0]; FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1]; FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * FCoefficients[2] - FMem[FPhase].X[2]; FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  Result := Sqrt(Sqr(FMem[FPhase].Y[1]) + Sqr(FMem[FPhase].Y[2]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample4(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                        // push EBX on stack
    PUSH    EDI                        // push EDI on stack
    PUSH    ESI                        // push ESI on stack
    MOV     EBX, [EAX.FPhase]          // EBX = FPhase
    SHL     EBX, 3                     // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]     // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4] // ESI = Y[0]
    SHR     EBX, 3                     // EBX = FPhase
    XOR     EBX, $1                    // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX          // FPhase = EBX

    FLD     Input.Double               // Input
    FLD     [EDI].Double               // X[0],Input
    FLD     Input.Double               // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                  // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Double               // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Double               // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX=FCoefficients
    FMUL    [EBX].Double               // (Input + Y[0]) * FCoefficients[0], X[0],Input
    FSUBRP                             // (Input + Y[0]) * FCoefficients[0] - X[0],Input
    FSTP    [ESI].Double               // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 8].Double           // X[1], Input
    FLD     [EAX.FPrev].Double         // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                  // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 8].Double           // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 8].Double           // FPrev + Y[1], X[1], Input
    MOV     EBX, [EAX.FCoefficients]   // EDX=FCoefficients
    FMUL    [EBX + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                             // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FSTP    [ESI + 8].Double           // FMem[FPhase].Y[1] :=  "

    FLD     [EDI + 16].Double          // X[2], Input
    FLD     [ESI].Double               // Y[0], X[2], Input
    FST     [EDI + 16].Double          // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI + 16].Double          // Y[2] + Y[0], X[2], Input
    FMUL    [EBX + 16].Double          // (Y[0] + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                             // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
    FST     [ESI + 16].Double          // FMem[FPhase].Y[2] :=  "
    FMUL    ST(0), ST(0)               // Sqr(FMem[FPhase].Y[3]);

    FLD     [EDI + 24].Double          // X[3], Input
    FLD     [ESI + 8].Double           // X[1], X[3], Input
    FST     [EDI + 24].Double          // FMem[FPhase].X[3] := X[1];
    FADD    [ESI + 24].Double          // FPrev + Y[3], X[3], Input
    FMUL    [EBX + 24].Double          // (FPrev + Y[3]) * FCoefficients[3], X[3]
    FSUBRP                             // (FPrev + Y[3]) * FCoefficients[3] - X[3]
    FST     [ESI + 24].Double          // FMem[FPhase].Y[3] :=  "
    FMUL    ST(0), ST(0)               // Sqr(FMem[FPhase].Y[3]);
    FADDP                              // Sqr(FMem[FPhase].Y[3]) + Sqr(FMem[FPhase].Y[4])
    FSQRT                              // Result := Sqrt(Sqr(FMem[FPhase].Y[3]) + Sqr(FMem[FPhase].Y[4]));
    FXCH                               // Input, Result
    FSTP    [EAX.FPrev].Double         // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0]; FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1]; FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * FCoefficients[2] - FMem[FPhase].X[2]; FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  FMem[FPhase].Y[3] := (FMem[FPhase].Y[1] + FMem[FPhase].Y[3]) * FCoefficients[3] - FMem[FPhase].X[3]; FMem[FPhase].X[3] := FMem[FPhase].Y[1];
  Result := Sqrt(Sqr(FMem[FPhase].Y[2]) + Sqr(FMem[FPhase].Y[3]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

function TPhaseHalfPi64.ProcessSampleLarge(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
    PUSH    EBX                         // push EBX on stack
    PUSH    EDI                         // push EDI on stack
    PUSH    ESI                         // push ESI on stack
    MOV     EBX, [EAX.FPhase]           // EBX = FPhase
    SHL     EBX, 3                      // EBX = 8 * FPhase
    MOV     EDI, [EAX + FMem[EBX]]      // EDI = X[0]
    MOV     ESI, [EAX + FMem[EBX] + 4]  // ESI = Y[0]
    SHR     EBX, 3                      // EBX = FPhase
    XOR     EBX, $1                     // Toggle FPhase!!
    MOV     [EAX.FPhase], EBX           // FPhase = EBX

    FLD     Input.Double                // Input
    FLD     [EDI].Double                // X[0], Input
    FLD     Input.Double                // Input, X[0], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI].Double                // FMem[FPhase].X[0] := Input;
    FADD    [ESI].Double                // Input + Y[0], X[0], Input
    MOV     EBX, [EAX.FCoefficients]    // EBX = FCoefficients
    FMUL    [EBX].Double                // (Input + Y[0]) * FCoefficients[0], X[0], Input
    FSUBRP                              // (Input + Y[0]) * FCoefficients[0] - X[0], Input
    FSTP    [ESI].Double                // FMem[FPhase].Y[0] :=  "

    FLD     [EDI + 8].Double            // X[1], Input
    FLD     [EAX.FPrev].Double          // FPrev, X[1], Input
    {$IFDEF HandleDenormals}
    FADD    CDenorm64                   // dEnOrMaL
    {$ENDIF}
    FST     [EDI + 8].Double            // FMem[FPhase].X[1] := FPrev;
    FADD    [ESI + 8].Double            // FPrev + Y[1], X[1], Input
    FMUL    [EBX + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
    FSUBRP                              // (FPrev + Y[1]) * FCoefficients[1] - X[1]
    FSTP    [ESI + 8].Double            // FMem[FPhase].Y[1] :=  "

    PUSH    ECX                         // push ECX on stack
    MOV     ECX, [EAX.FNumberOfCoeffs]  // ECX = Self.FNumberOfCoeffs
    SUB     ECX, 4                      // "Den Rest mach ich selber"

@Loopy:
    FLD     [EDI + 16].Double           // X[2], Input
    FLD     [ESI].Double                // Y[0], X[2], Input
    FST     [EDI + 16].Double           // FMem[FPhase].X[2] := Y[0];
    FADD    [ESI + 16].Double           // FPrev + Y[2], X[2], Input
    FMUL    [EBX + 16].Double           // (FPrev + Y[2]) * FCoefficients[2], X[2]
    FSUBRP                              // (FPrev + Y[2]) * FCoefficients[2] - X[2]
    FSTP    [ESI + 16].Double           // FMem[FPhase].Y[2] :=  "
    ADD     ESI, 8
    ADD     EDI, 8
    ADD     EBX, 8
    LOOP    @Loopy
    POP     ECX                         // restore ECX

    FLD     [EDI + 16].Double           // X[10], Input
    FLD     [ESI].Double                // X[8], X[10], Input
    FST     [EDI + 16].Double           // FMem[FPhase].X[10] := X[8];
    FADD    [ESI + 16].Double           // FPrev + Y[10], X[8], Input
    FMUL    [EBX + 16].Double           // (FPrev + Y[10]) * FCoefficients[10], X[10]
    FSUBRP                              // (FPrev + Y[10]) * FCoefficients[10] - X[10]
    FST     [ESI + 16].Double           // FMem[FPhase].Y[10] :=  "
    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[10]);

    FLD     [EDI + 24].Double           // X[11], Input
    FLD     [ESI + 8].Double            // X[9], X[11], Input
    FST     [EDI + 24].Double           // FMem[FPhase].X[11] := X[9];
    FADD    [ESI + 24].Double           // FPrev + Y[11], X[9], Input
    FMUL    [EBX + 24].Double           // (FPrev + Y[11]) * FCoefficients[11], X[11]
    FSUBRP                              // (FPrev + Y[11]) * FCoefficients[11] - X[11]
    FST     [ESI + 24].Double           // FMem[FPhase].Y[11] :=  "

    FMUL    ST(0), ST(0)                // Sqr(FMem[FPhase].Y[11])
    FADDP                               // Sqr(FMem[FPhase].Y[10]) + Sqr(FMem[FPhase].Y[11]);
    FSQRT                               // Result := Sqrt(Sqr(FMem[FPhase].Y[10]) + Sqr(FMem[FPhase].Y[11]));
    FXCH                                // Input, Result
    FSTP    [EAX.FPrev].Double          // FPrev := Input;

    POP     ESI
    POP     EDI
    POP     EBX
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0]; FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1]; FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) * FCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  Result := Sqrt(Sqr(FMem[FPhase].Y[FNumberOfCoeffs - 2]) + Sqr(FMem[FPhase].Y[FNumberOfCoeffs - 1]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

{$IFDEF HandleDenormals}
initialization
  CDenorm32 := DAV_Common.CDenorm32;
  CDenorm64 := DAV_Common.CDenorm64;
{$ENDIF}

end.
