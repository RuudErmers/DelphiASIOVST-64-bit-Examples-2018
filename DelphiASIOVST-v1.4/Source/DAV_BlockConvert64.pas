{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_BlockConvert64;

//  Assembler code optimizations are based on Agner Fog's excellent
//  documentations. In particular the following document was used:
//  http://www.agner.org/optimize/optimizing_assembly.pdf

interface

{$I DAV_Compiler.inc}

{-$DEFINE PUREPASCAL}

uses
  DAV_Bindings;

{ Prototype }

type
  TBlockConvertToFloat64   = procedure(Destination: PDouble; Source: Pointer; Count: LongInt);
  TBlockConvertFromFloat64 = procedure(Destination: Pointer; Source: PDouble; Count: LongInt);

{ Function Pointers }

var
  BlockConvertInt16LSBToFloat64     : TBlockConvertToFloat64;
  BlockConvertInt24LSBToFloat64     : TBlockConvertToFloat64;
  BlockConvertInt32LSBToFloat64     : TBlockConvertToFloat64;
  BlockConvertInt32LSB16ToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32LSB18ToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32LSB20ToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32LSB24ToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt16MSBToFloat64     : TBlockConvertToFloat64;
  BlockConvertInt24MSBToFloat64     : TBlockConvertToFloat64;
  BlockConvertInt32MSBToFloat64     : TBlockConvertToFloat64;
  BlockConvertInt32MSB16ToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32MSB18ToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32MSB20ToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32MSB24ToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt16LSBFromFloat64   : TBlockConvertFromFloat64;
  BlockConvertInt24LSBFromFloat64   : TBlockConvertFromFloat64;
  BlockConvertInt32LSBFromFloat64   : TBlockConvertFromFloat64;
  BlockConvertInt32LSB16FromFloat64 : TBlockConvertFromFloat64;
  BlockConvertInt32LSB18FromFloat64 : TBlockConvertFromFloat64;
  BlockConvertInt32LSB20FromFloat64 : TBlockConvertFromFloat64;
  BlockConvertInt32LSB24FromFloat64 : TBlockConvertFromFloat64;
  BlockConvertInt16MSBFromFloat64   : TBlockConvertFromFloat64;
  BlockConvertInt24MSBFromFloat64   : TBlockConvertFromFloat64;
  BlockConvertInt32MSBFromFloat64   : TBlockConvertFromFloat64;
  BlockConvertInt32MSB16FromFloat64 : TBlockConvertFromFloat64;
  BlockConvertInt32MSB18FromFloat64 : TBlockConvertFromFloat64;
  BlockConvertInt32MSB20FromFloat64 : TBlockConvertFromFloat64;
  BlockConvertInt32MSB24FromFloat64 : TBlockConvertFromFloat64;


{ Binding Function Pointers }

var
  BindingBlockConvertInt16LSBToFloat64     : TFunctionBinding;
  BindingBlockConvertInt24LSBToFloat64     : TFunctionBinding;
  BindingBlockConvertInt32LSBToFloat64     : TFunctionBinding;
  BindingBlockConvertInt32LSB16ToFloat64   : TFunctionBinding;
  BindingBlockConvertInt32LSB18ToFloat64   : TFunctionBinding;
  BindingBlockConvertInt32LSB20ToFloat64   : TFunctionBinding;
  BindingBlockConvertInt32LSB24ToFloat64   : TFunctionBinding;
  BindingBlockConvertInt16MSBToFloat64     : TFunctionBinding;
  BindingBlockConvertInt24MSBToFloat64     : TFunctionBinding;
  BindingBlockConvertInt32MSBToFloat64     : TFunctionBinding;
  BindingBlockConvertInt32MSB16ToFloat64   : TFunctionBinding;
  BindingBlockConvertInt32MSB18ToFloat64   : TFunctionBinding;
  BindingBlockConvertInt32MSB20ToFloat64   : TFunctionBinding;
  BindingBlockConvertInt32MSB24ToFloat64   : TFunctionBinding;
  BindingBlockConvertInt16LSBFromFloat64   : TFunctionBinding;
  BindingBlockConvertInt24LSBFromFloat64   : TFunctionBinding;
  BindingBlockConvertInt32LSBFromFloat64   : TFunctionBinding;
  BindingBlockConvertInt32LSB16FromFloat64 : TFunctionBinding;
  BindingBlockConvertInt32LSB18FromFloat64 : TFunctionBinding;
  BindingBlockConvertInt32LSB20FromFloat64 : TFunctionBinding;
  BindingBlockConvertInt32LSB24FromFloat64 : TFunctionBinding;
  BindingBlockConvertInt16MSBFromFloat64   : TFunctionBinding;
  BindingBlockConvertInt24MSBFromFloat64   : TFunctionBinding;
  BindingBlockConvertInt32MSBFromFloat64   : TFunctionBinding;
  BindingBlockConvertInt32MSB16FromFloat64 : TFunctionBinding;
  BindingBlockConvertInt32MSB18FromFloat64 : TFunctionBinding;
  BindingBlockConvertInt32MSB20FromFloat64 : TFunctionBinding;
  BindingBlockConvertInt32MSB24FromFloat64 : TFunctionBinding;


{ Binding List }

var
  BindingBlockConvertToFloat64   : TFunctionBindingList;
  BindingBlockConvertFromFloat64 : TFunctionBindingList;

implementation

uses
  DAV_Common;

const
  CFloatToShort   : Single = $7F;
  CShortToFloat   : Single = 1 / $7F;
  CFloatToSmall   : Single = $7FFF;
  CSmallToFloat   : Single = 1 / $7FFF;
  CFloatToInt18   : Double = $1FFFF;
  CInt18ToFloat   : Double = 1 / $1FFFF;
  CFloatToInt20   : Double = $7FFFF;
  CInt20ToFloat   : Double = 1 / $7FFFF;
  CFloatToInt24   : Double = $7FFFFF;
  CInt24ToFloat   : Double = 1 / $7FFFFF;
  CFloatToInt32   : Double = $7FFFFFFF;
  CInt32ToFloat   : Double = 1 / $7FFFFFFF;

  // Note:
  // Single = 1 / $7FFF     <=>  Integer = $38000100
  // Single = 1 / $7FFFFFFF <=>  Integer = $30000000

  // EMMS (single precision)
  C2Int32ToSingle : array [0..1] of Integer = ($30000000, $30000000);
  C2Int16ToSingle : array [0..1] of Integer = ($38000100, $38000100);

  // SSE & SSE2 (single precision)
  C4Int32ToSingle : array [0..3] of Integer = ($30000000, $30000000,
    $30000000, $30000000);
  C4SmallToSingle : array [0..3] of Integer = ($38000100, $38000100,
    $38000100, $38000100);
  C4Int18ToSingle : array [0..3] of Single = (1 / $1FFFF, 1 / $1FFFF,
    1 / $1FFFF, 1 / $1FFFF);
  C4Int20ToSingle : array [0..3] of Single = (1 / $7FFFF, 1 / $7FFFF,
    1 / $7FFFF, 1 / $7FFFF);
  C4Int24ToSingle : array [0..3] of Single = (1 / $7FFFFF, 1 / $7FFFFF,
    1 / $7FFFFF, 1 / $7FFFFF);

procedure BlockConvertInt16LSBToFloat64Native(Destination: PDouble;

  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceWord: PWord absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := SourceWord^ * CSmallToFloat;
    Inc(SourceWord);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 2
  NEG     ECX
  JNL     @Done

  FLD     CSmallToFloat

@Start:
  FILD    [EDX + ECX * 2].Word
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt24LSBToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  SourceByte: PByte absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := (SourceInt^ and $FFFFFF00) * CInt32ToFloat;
    Inc(SourceByte, 3);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     CInt32ToFloat
  PUSH    EBX

@Start:
  MOV     EBX, [EDX].DWord
  SHL     EBX, 8
  AND     EBX, $FFFFFF00

  MOV     [ESP - 4], EBX
  FILD    [ESP - 4].Single
  FMUL    ST(0), ST(1)

  FSTP    [EAX + ECX * 8].Double
  ADD     EDX, 3
  ADD     ECX, 1
  JS      @Start

  POP     EBX
  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSBToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := SourceInt^ * CInt32ToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  FLD     CInt32ToFloat

@Start:
  FILD    [EDX + ECX * 4].Single
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB16ToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := SourceInt^ * CSmallToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  FLD     CSmallToFloat

@Start:
  FILD    [EDX + ECX * 4].Single
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB18ToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := SourceInt^ * CInt18ToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  FLD      CInt18ToFloat

@Start:
  FILD    [EDX + ECX * 4].Single
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB20ToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := SourceInt^ * CInt20ToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  FLD     CInt20ToFloat

@Start:
  FILD    [EDX + ECX * 4].Single
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB24ToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := SourceInt^ * CInt24ToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  FLD     CInt24ToFloat

@Start:
  FILD    [EDX + ECX * 4].Single
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt16MSBToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceWord: PWord absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := Swap(SourceWord^) * CSmallToFloat;
    Inc(SourceWord);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 2
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CSmallToFloat

@Start:
  MOV     BX, [EDX + 2 * ECX]
  XCHG    BH, BL
  MOV     [ESP - 4], BX
  FILD    [ESP - 4].Word
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt24MSBToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
type
  TByte3Array = Array [0 .. 2] of Byte;
  PByte3Array = ^TByte3Array;
var
  SourceBytes: PByte3Array absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := ((SourceBytes^[0] shl 32) + (SourceBytes^[1] shl 24) +
      (SourceBytes^[2] shl 16)) * CInt32ToFloat;
    Inc(SourceBytes, 3);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     CInt24ToFloat
  PUSH    EBX

@Start:
  XOR     EBX, EBX

  MOV     BL, [EDX + 2]
  MOV     BH, [EDX + 1]
  ROR     EBX, 8
  MOV     BH, [EDX]
  ROL     EBX, 8

  MOV     [ESP - 4], EBX
  FILD    [ESP - 4].Single
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     EDX, 3
  ADD     ECX, 1
  JS      @Start

  POP     EBX
  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSBToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := Swap32(SourceInt^) * CInt32ToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CInt32ToFloat

@Start:
  MOV     EBX, [EDX + ECX * 4]
  BSWAP   EBX
  MOV     [ESP - 4], BX
  FILD    [ESP - 4].Word
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB16ToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := Swap32(SourceInt^) * CSmallToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CSmallToFloat

@Start:
  MOV     EBX, [EDX + ECX * 4]
  BSWAP   EBX
  MOV     [ESP - 4], BX
  FILD    [ESP - 4].Word
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB18ToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := Swap32(SourceInt^) * CInt18ToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CInt18ToFloat

@Start:
  MOV     EBX, [EDX + ECX * 4]
  BSWAP   EBX
  MOV     [ESP - 4], BX
  FILD    [ESP - 4].Word
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB20ToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := Swap32(SourceInt^) * CInt20ToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CInt20ToFloat

@Start:
  MOV     EBX, [EDX + ECX * 4]
  BSWAP   EBX
  MOV     [ESP - 4], BX
  FILD    [ESP - 4].Word
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB24ToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt: PInteger absolute Source;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Destination^ := Swap32(SourceInt^) * CInt24ToFloat;
    Inc(SourceInt);
    Inc(Destination);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CInt24ToFloat

@Start:
  MOV     EBX, [EDX + ECX * 4]
  BSWAP   EBX
  MOV     [ESP - 4], BX
  FILD    [ESP - 4].Word
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}
// ... from float

procedure BlockConvertInt16LSBFromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestWord: PWord absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestWord^ := Round(Source^ * CFloatToSmall);
    Inc(Source);
    Inc(DestWord);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 2
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     CFloatToSmall

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [EAX + ECX * 2].Word
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt24LSBFromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestByte: PByte absolute Destination;
  DestTemp: Integer;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestTemp := Round(Source^ * CFloatToInt32);
    DestByte^ := DestTemp and $FF;
    Inc(DestByte);
    DestByte^ := (DestTemp shr 8) and $FF;
    Inc(DestByte);
    DestByte^ := (DestTemp shr 16) and $FF;
    Inc(DestByte);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     CFloatToInt24
  PUSH    EBX

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [ESP - 4].Single

  MOV     EBX, [ESP - 4]
  AND     EBX, $FFFFFF
  MOV     [EAX].DWord, EBX

  ADD     EAX, 3
  ADD     ECX, 1
  JS      @Start

  POP     EBX
  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSBFromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToInt32);
    Inc(Source);
    Inc(DestInt);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     CFloatToInt32

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [EAX + ECX * 4].DWord
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB16FromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToSmall);
    Inc(Source);
    Inc(DestInt);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     CFloatToSmall

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [EAX + ECX * 4].DWord
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB18FromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToInt18);
    Inc(Source);
    Inc(DestInt);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD      CFloatToInt18

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [EAX + ECX * 4].DWord
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB20FromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToInt20);
    Inc(Source);
    Inc(DestInt);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     CFloatToInt20

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [EAX + ECX * 4].DWord
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB24FromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToInt24);
    Inc(Source);
    Inc(DestInt);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     CFloatToInt24

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [EAX + ECX * 4].DWord
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt16MSBFromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestWord: PWord absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestWord^ := Round(Source^ * CFloatToSmall);
    Flip16(DestWord^);
    Inc(DestWord);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 2
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  XOR     EBX, EBX
  FLD     CFloatToSmall

@Start:
  FLD     [EDX + 8 * ECX].Double
  FMUL    ST(0), ST(1)
  FISTP   [ESP - 4].Word
  MOV     BX, [ESP - 4]
  XCHG    BH, BL
  MOV     [EAX + ECX * 2], BX
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt24MSBFromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestByte: PByte absolute Destination;
  DestTemp: Integer;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestTemp := Round(Source^ * CFloatToInt24);
    DestByte^ := (DestTemp shr 16) and $FF;
    Inc(DestByte);
    DestByte^ := (DestTemp shr 8) and $FF;
    Inc(DestByte);
    DestByte^ := DestTemp and $FF;
    Inc(DestByte);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     CFloatToInt24
  PUSH    EBX

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [ESP - 4].Single
  MOV     EBX, [ESP - 4]
  MOV     [EAX], BL
  MOV     [EAX + 1], BH
  ROR     EBX, 8
  MOV     [EAX + 2], BH

  ADD     EAX, 3
  ADD     ECX, 1
  JS      @Start


  POP     EBX
  FSTP    ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSBFromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToInt32);
    Flip32(DestInt^);
    Inc(DestInt);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CFloatToInt32

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [ESP - 4].DWord
  MOV     EBX, [ESP - 4]
  BSWAP   EBX
  MOV     [EAX + ECX * 4], EBX
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB16FromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToSmall);
    Flip32(DestInt^);
    Inc(DestInt);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CFloatToSmall

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [ESP - 4].DWord
  MOV     EBX, [ESP - 4]
  BSWAP   EBX
  MOV     [EAX + ECX * 4], EBX
  ADD     ECX, 1
  JS      @Start


  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB18FromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToInt18);
    Flip32(DestInt^);
    Inc(DestInt);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CFloatToInt18

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [ESP - 4].DWord
  MOV     EBX, [ESP - 4]
  BSWAP   EBX
  MOV     [EAX + ECX * 4], EBX
  ADD     ECX, 1
  JS      @Start


  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB20FromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToInt20);
    Flip32(DestInt^);
    Inc(DestInt);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CFloatToInt20

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [ESP - 4].DWord
  MOV     EBX, [ESP - 4]
  BSWAP   EBX
  MOV     [EAX + ECX * 4], EBX
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB24FromFloat64Native(Destination: Pointer;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  DestInt: PInteger absolute Destination;
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    DestInt^ := Round(Source^ * CFloatToInt24);
    Flip32(DestInt^);
    Inc(DestInt);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  PUSH    EBX
  FLD     CFloatToInt24

@Start:
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FISTP   [ESP - 4].DWord
  MOV     EBX, [ESP - 4]
  BSWAP   EBX
  MOV     [EAX + ECX * 4], EBX
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
  POP     EBX

@Done:
end;
{$ENDIF}
{$IFNDEF PUREPASCAL}
{ SSE optimizations }

{ 3D-Now optimizations }

{$ENDIF}

procedure BindFunctions;
begin
  // create function binding list for 64-bit float conversions
  BindingBlockConvertToFloat64 := TFunctionBindingList.Create;

  BindingBlockConvertInt16LSBToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt16LSBToFloat64,
    @BlockConvertInt16LSBToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding(BindingBlockConvertInt16LSBToFloat64);
  with BindingBlockConvertInt16LSBToFloat64 do
  begin
    Add(@BlockConvertInt16LSBToFloat64Native);
  end;

  BindingBlockConvertInt24LSBToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt24LSBToFloat64,
    @BlockConvertInt24LSBToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding(BindingBlockConvertInt24LSBToFloat64);
  with BindingBlockConvertInt24LSBToFloat64 do
  begin
    Add(@BlockConvertInt24LSBToFloat64Native);
  end;

  BindingBlockConvertInt32LSBToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSBToFloat64,
    @BlockConvertInt32LSBToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding(BindingBlockConvertInt32LSBToFloat64);
  with BindingBlockConvertInt32LSBToFloat64 do
  begin
    Add(@BlockConvertInt32LSBToFloat64Native);
  end;

  BindingBlockConvertInt32LSB16ToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSB16ToFloat64,
    @BlockConvertInt32LSB16ToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding
    (BindingBlockConvertInt32LSB16ToFloat64);
  with BindingBlockConvertInt32LSB16ToFloat64 do
  begin
    Add(@BlockConvertInt32LSB16ToFloat64Native);
  end;

  BindingBlockConvertInt32LSB18ToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSB18ToFloat64,
    @BlockConvertInt32LSB18ToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding
    (BindingBlockConvertInt32LSB18ToFloat64);
  with BindingBlockConvertInt32LSB18ToFloat64 do
  begin
    Add(@BlockConvertInt32LSB18ToFloat64Native);
  end;

  BindingBlockConvertInt32LSB20ToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSB20ToFloat64,
    @BlockConvertInt32LSB20ToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding
    (BindingBlockConvertInt32LSB20ToFloat64);
  with BindingBlockConvertInt32LSB20ToFloat64 do
  begin
    Add(@BlockConvertInt32LSB20ToFloat64Native);
  end;

  BindingBlockConvertInt32LSB24ToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSB24ToFloat64,
    @BlockConvertInt32LSB24ToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding
    (BindingBlockConvertInt32LSB24ToFloat64);
  with BindingBlockConvertInt32LSB24ToFloat64 do
  begin
    Add(@BlockConvertInt32LSB24ToFloat64Native);
  end;

  BindingBlockConvertInt16MSBToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt16MSBToFloat64,
    @BlockConvertInt16MSBToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding(BindingBlockConvertInt16MSBToFloat64);
  with BindingBlockConvertInt16MSBToFloat64 do
  begin
    Add(@BlockConvertInt16MSBToFloat64Native);
  end;

  BindingBlockConvertInt24MSBToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt24MSBToFloat64,
    @BlockConvertInt24MSBToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding(BindingBlockConvertInt24MSBToFloat64);
  with BindingBlockConvertInt24MSBToFloat64 do
  begin
    Add(@BlockConvertInt24MSBToFloat64Native);
  end;

  BindingBlockConvertInt32MSBToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSBToFloat64,
    @BlockConvertInt32MSBToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding(BindingBlockConvertInt32MSBToFloat64);
  with BindingBlockConvertInt32MSBToFloat64 do
  begin
    Add(@BlockConvertInt32MSBToFloat64Native);
  end;

  BindingBlockConvertInt32MSB16ToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSB16ToFloat64,
    @BlockConvertInt32MSB16ToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding
    (BindingBlockConvertInt32MSB16ToFloat64);
  with BindingBlockConvertInt32MSB16ToFloat64 do
  begin
    Add(@BlockConvertInt32MSB16ToFloat64Native);
  end;

  BindingBlockConvertInt32MSB18ToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSB18ToFloat64,
    @BlockConvertInt32MSB18ToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding
    (BindingBlockConvertInt32MSB18ToFloat64);
  with BindingBlockConvertInt32MSB18ToFloat64 do
  begin
    Add(@BlockConvertInt32MSB18ToFloat64Native);
  end;

  BindingBlockConvertInt32MSB20ToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSB20ToFloat64,
    @BlockConvertInt32MSB20ToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding
    (BindingBlockConvertInt32MSB20ToFloat64);
  with BindingBlockConvertInt32MSB20ToFloat64 do
  begin
    Add(@BlockConvertInt32MSB20ToFloat64Native);
  end;

  BindingBlockConvertInt32MSB24ToFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSB24ToFloat64,
    @BlockConvertInt32MSB24ToFloat64Native);
  BindingBlockConvertToFloat64.AddBinding
    (BindingBlockConvertInt32MSB24ToFloat64);
  with BindingBlockConvertInt32MSB24ToFloat64 do
  begin
    Add(@BlockConvertInt32MSB24ToFloat64Native);
  end;

  // create function binding list for 64-bit float conversions
  BindingBlockConvertFromFloat64 := TFunctionBindingList.Create;

  BindingBlockConvertInt16LSBFromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt16LSBFromFloat64,
    @BlockConvertInt16LSBFromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt16LSBFromFloat64);
  with BindingBlockConvertInt16LSBFromFloat64 do
  begin
    Add(@BlockConvertInt16LSBFromFloat64Native);
  end;

  BindingBlockConvertInt24LSBFromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt24LSBFromFloat64,
    @BlockConvertInt24LSBFromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt24LSBFromFloat64);
  with BindingBlockConvertInt24LSBFromFloat64 do
  begin
    Add(@BlockConvertInt24LSBFromFloat64Native);
  end;

  BindingBlockConvertInt32LSBFromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSBFromFloat64,
    @BlockConvertInt32LSBFromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32LSBFromFloat64);
  with BindingBlockConvertInt32LSBFromFloat64 do
  begin
    Add(@BlockConvertInt32LSBFromFloat64Native);
  end;

  BindingBlockConvertInt32LSB16FromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSB16FromFloat64,
    @BlockConvertInt32LSB16FromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32LSB16FromFloat64);
  with BindingBlockConvertInt32LSB16FromFloat64 do
  begin
    Add(@BlockConvertInt32LSB16FromFloat64Native);
  end;

  BindingBlockConvertInt32LSB18FromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSB18FromFloat64,
    @BlockConvertInt32LSB18FromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32LSB18FromFloat64);
  with BindingBlockConvertInt32LSB18FromFloat64 do
  begin
    Add(@BlockConvertInt32LSB18FromFloat64Native);
  end;

  BindingBlockConvertInt32LSB20FromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSB20FromFloat64,
    @BlockConvertInt32LSB20FromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32LSB20FromFloat64);
  with BindingBlockConvertInt32LSB20FromFloat64 do
  begin
    Add(@BlockConvertInt32LSB20FromFloat64Native);
  end;

  BindingBlockConvertInt32LSB24FromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32LSB24FromFloat64,
    @BlockConvertInt32LSB24FromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32LSB24FromFloat64);
  with BindingBlockConvertInt32LSB24FromFloat64 do
  begin
    Add(@BlockConvertInt32LSB24FromFloat64Native);
  end;

  BindingBlockConvertInt16MSBFromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt16MSBFromFloat64,
    @BlockConvertInt16MSBFromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt16MSBFromFloat64);
  with BindingBlockConvertInt16MSBFromFloat64 do
  begin
    Add(@BlockConvertInt16MSBFromFloat64Native);
  end;

  BindingBlockConvertInt24MSBFromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt24MSBFromFloat64,
    @BlockConvertInt24MSBFromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt24MSBFromFloat64);
  with BindingBlockConvertInt24MSBFromFloat64 do
  begin
    Add(@BlockConvertInt24MSBFromFloat64Native);
  end;

  BindingBlockConvertInt32MSBFromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSBFromFloat64,
    @BlockConvertInt32MSBFromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32MSBFromFloat64);
  with BindingBlockConvertInt32MSBFromFloat64 do
  begin
    Add(@BlockConvertInt32MSBFromFloat64Native);
  end;

  BindingBlockConvertInt32MSB16FromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSB16FromFloat64,
    @BlockConvertInt32MSB16FromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32MSB16FromFloat64);
  with BindingBlockConvertInt32MSB16FromFloat64 do
  begin
    Add(@BlockConvertInt32MSB16FromFloat64Native);
  end;

  BindingBlockConvertInt32MSB18FromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSB18FromFloat64,
    @BlockConvertInt32MSB18FromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32MSB18FromFloat64);
  with BindingBlockConvertInt32MSB18FromFloat64 do
  begin
    Add(@BlockConvertInt32MSB18FromFloat64Native);
  end;

  BindingBlockConvertInt32MSB20FromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSB20FromFloat64,
    @BlockConvertInt32MSB20FromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32MSB20FromFloat64);
  with BindingBlockConvertInt32MSB20FromFloat64 do
  begin
    Add(@BlockConvertInt32MSB20FromFloat64Native);
  end;

  BindingBlockConvertInt32MSB24FromFloat64 :=
    TFunctionBinding.Create(@@BlockConvertInt32MSB24FromFloat64,
    @BlockConvertInt32MSB24FromFloat64Native);
  BindingBlockConvertFromFloat64.AddBinding
    (BindingBlockConvertInt32MSB24FromFloat64);
  with BindingBlockConvertInt32MSB24FromFloat64 do
  begin
    Add(@BlockConvertInt32MSB24FromFloat64Native);
  end;
end;

procedure UnbindFunctions;
begin
  BindingBlockConvertToFloat64.Free;
  BindingBlockConvertToFloat64 := nil;
  BindingBlockConvertFromFloat64.Free;
  BindingBlockConvertFromFloat64 := nil;
end;

initialization

BindFunctions;

finalization

UnbindFunctions;

end.
