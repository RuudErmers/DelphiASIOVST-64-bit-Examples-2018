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

unit DAV_BlockArithmetrics;

interface

{$I DAV_Compiler.inc}
{$IFDEF CPUx86_64}
{$DEFINE PUREPASCAL}
{$ENDIF}

var
  BlockAdditionInplace32: procedure(Destination, Source: PSingle; Count: Integer);
  BlockAdditionInplace64: procedure(Destination, Source: PDouble; Count: Integer);

  BlockSubtractInplace32: procedure(Destination, Source: PSingle; Count: Integer);
  BlockSubtractInplace64: procedure(Destination, Source: PDouble; Count: Integer);

  BlockReverseSubtractInplace32: procedure(Destination, Source: PSingle; Count: Integer);
  BlockReverseSubtractInplace64: procedure(Destination, Source: PDouble; Count: Integer);

  BlockOffsetInplace32: procedure(Destination: PSingle; Value: Single; Count: Integer);
  BlockOffsetInplace64: procedure(Destination: PDouble; Value: Double; Count: Integer);

  BlockMultiplyInplace32: procedure(Destination, Source: PSingle; Count: Integer);
  BlockMultiplyInplace64: procedure(Destination, Source: PDouble; Count: Integer);

  BlockDivideInplace32: procedure(Destination, Source: PSingle; Count: Integer);
  BlockDivideInplace64: procedure(Destination, Source: PDouble; Count: Integer);

  BlockReverseDivideInplace32: procedure(Destination, Source: PSingle; Count: Integer);
  BlockReverseDivideInplace64: procedure(Destination, Source: PDouble; Count: Integer);

  BlockScaleInplace32: procedure(Destination: PSingle; Value: Single; Count: Integer);
  BlockScaleInplace64: procedure(Destination: PDouble; Value: Double; Count: Integer);

  BlockScale32: procedure(Destination, Source: PSingle; Count: Integer; Value: Single);
  BlockScale64: procedure(Destination, Source: PDouble; Count: Integer; Value: Double);

  BlockAdditionScale32: procedure(Destination, Source: PSingle; Count: Integer; Scale: Single);
  BlockAdditionScale64: procedure(Destination, Source: PDouble; Count: Integer; Scale: Double);

implementation

uses
  DAV_Bindings;

procedure BlockAdditionInplace32Native(Destination, Source: PSingle;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ + Source^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EAX + ECX * 4].Single
  FADD    [EDX + ECX * 4].Single
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockAdditionInplace64Native(Destination, Source: PDouble;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ + Source^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EAX + ECX * 8].Double
  FADD    [EDX + ECX * 8].Double
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockSubtractInplace32Native(Destination, Source: PSingle;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ - Source^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EAX + ECX * 4].Single
  FSUB    [EDX + ECX * 4].Single
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockSubtractInplace64Native(Destination, Source: PDouble;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ - Source^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EAX + ECX * 8].Double
  FSUB    [EDX + ECX * 8].Double
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockReverseSubtractInplace32Native(Destination, Source: PSingle;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^ - Destination^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EDX + ECX * 4].Single
  FSUB    [EAX + ECX * 4].Single
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockReverseSubtractInplace64Native(Destination, Source: PDouble;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^ - Destination^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EDX + ECX * 8].Double
  FSUB    [EAX + ECX * 8].Double
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockOffsetInplace32Native(Destination: PSingle; Value: Single;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ + Value;
    Inc(Destination);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  NEG     ECX
  JNL     @Done

  FLD     Value.Single

@Start:
  FLD     [EAX + ECX * 4].Single
  FADD    ST(0), ST(1)
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
@Done:
  {$ENDIF}
end;

procedure BlockOffsetInplace64Native(Destination: PDouble; Value: Double;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ + Value;
    Inc(Destination);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     Value.Single

@Start:
  FLD     [EAX + ECX * 8].Double
  FADD    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
@Done:
  {$ENDIF}
end;

procedure BlockMultiplyInplace32Native(Destination, Source: PSingle;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ * Source^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EAX + ECX * 4].Single
  FMUL    [EDX + ECX * 4].Single
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockMultiplyInplace64Native(Destination, Source: PDouble;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ * Source^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EAX + ECX * 8].Single
  FMUL    [EDX + ECX * 8].Single
  FSTP    [EAX + ECX * 8].Single
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockDivideInplace32Native(Destination, Source: PSingle;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ / Source^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EAX + ECX * 4].Single
  FDIV    [EDX + ECX * 4].Single
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockDivideInplace64Native(Destination, Source: PDouble;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ / Source^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EAX + ECX * 8].Double
  FDIV    [EDX + ECX * 8].Double
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockReverseDivideInplace32Native(Destination, Source: PSingle;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^ / Destination^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EDX + ECX * 4].Single
  FDIV    [EAX + ECX * 4].Single
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockReverseDivideInplace64Native(Destination, Source: PDouble;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^ / Destination^;
    Inc(Destination);
    Inc(Source);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EDX + ECX * 8].Double
  FDIV    [EAX + ECX * 8].Double
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

@Done:
  {$ENDIF}
end;

procedure BlockScaleInplace32Native(Destination: PSingle; Value: Single;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ * Value;
    Inc(Destination);
  end;
{$ELSE}
asm
  MOV     ECX, Count
  LEA     EAX, EAX + ECX * 4
  NEG     ECX
  JNL     @Done

  FLD     Value.Single

@Start:
  FLD     [EAX + ECX * 4].Single
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
@Done:
  {$ENDIF}
end;

procedure BlockScaleInplace64Native(Destination: PDouble; Value: Double;
  Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Destination^ * Value;
    Inc(Destination);
  end;
{$ELSE}
asm
  MOV     ECX, Count
  LEA     EAX, EAX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     Value.Double

@Start:
  FLD     [EAX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
@Done:
  {$ENDIF}
end;

procedure BlockScale32Native(Destination, Source: PSingle; Count: Integer;
  Value: Single);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^ * Value;
    Inc(Source);
    Inc(Destination);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

  FLD     Value.Single

@Start:
  FLD     [EDX + ECX * 4].Single
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
@Done:
  {$ENDIF}
end;

procedure BlockScale64Native(Destination, Source: PDouble; Count: Integer;
  Value: Double);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^ * Value;
    Inc(Source);
    Inc(Destination);
  end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

  FLD     Value.Double

@Start:
  FLD     [EAX + ECX * 8].Double
  FLD     [EDX + ECX * 8].Double
  FMUL    ST(0), ST(1)
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
@Done:
  {$ENDIF}
end;

procedure BlockAdditionScale32Native(Destination, Source: PSingle;
  Count: Integer; Scale: Single);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^ * Scale + Destination^;
    Inc(Destination);
  end;
{$ELSE}
asm
  LEA     EAX, [EAX + ECX * 4]
  LEA     EDX, [EDX + ECX * 4]
  NEG     ECX
  JNL     @Done

  FLD     Scale.Single

@Start:
  FLD     [EDX + ECX * 4].Single
  FMUL    ST(0), ST(1)
  FADD    [EAX + ECX * 4].Single
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
@Done:
  {$ENDIF}
end;

procedure BlockAdditionScale64Native(Destination, Source: PDouble;
  Count: Integer; Scale: Double);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^ * Scale + Destination^;
    Inc(Destination);
  end;
{$ELSE}
asm
  LEA     EAX, [EAX + ECX * 8]
  LEA     EDX, [EDX + ECX * 8]
  NEG     ECX
  JNL     @Done

  FLD     Scale.Double

@Start:
  FLD     [EDX + ECX * 8].Single
  FMUL    ST(0), ST(1)
  FADD    [EAX + ECX * 8].Single
  FSTP    [EAX + ECX * 8].Single
  ADD     ECX, 1
  JS      @Start

  FSTP    ST(0)
@Done:
  {$ENDIF}
end;

procedure BindFunctions;
begin
  // Block Inplace Addition Binding (32 bit)
  with TFunctionBinding.Create(@@BlockAdditionInplace32,
    @BlockAdditionInplace32Native) do
  begin
    Add(@BlockAdditionInplace32Native);
  end;

  // Block Inplace Addition Binding (64 bit)
  with TFunctionBinding.Create(@@BlockAdditionInplace64,
    @BlockAdditionInplace64Native) do
  begin
    Add(@BlockAdditionInplace64Native);
  end;

  // Block Inplace Subtraction Binding (32 bit)
  with TFunctionBinding.Create(@@BlockSubtractInplace32,
    @BlockSubtractInplace32Native) do
  begin
    Add(@BlockSubtractInplace32Native);
  end;

  // Block Inplace Subtraction Binding (64 bit)
  with TFunctionBinding.Create(@@BlockSubtractInplace64,
    @BlockSubtractInplace64Native) do
  begin
    Add(@BlockSubtractInplace64Native);
  end;

  // Block Inplace Reverse Subtraction Binding (32 bit)
  with TFunctionBinding.Create(@@BlockReverseSubtractInplace32,
    @BlockReverseSubtractInplace32Native) do
  begin
    Add(@BlockReverseSubtractInplace32Native);
  end;

  // Block Inplace Reverse Subtraction Binding (64 bit)
  with TFunctionBinding.Create(@@BlockReverseSubtractInplace64,
    @BlockReverseSubtractInplace64Native) do
  begin
    Add(@BlockReverseSubtractInplace64Native);
  end;

  // Block Inplace Offset Binding (32 bit)
  with TFunctionBinding.Create(@@BlockOffsetInplace32,
    @BlockOffsetInplace32Native) do
  begin
    Add(@BlockOffsetInplace32Native);
  end;

  // Block Inplace Offset Binding (64 bit)
  with TFunctionBinding.Create(@@BlockOffsetInplace64,
    @BlockOffsetInplace64Native) do
  begin
    Add(@BlockOffsetInplace64Native);
  end;

  // Block Inplace Multiply Binding (32 bit)
  with TFunctionBinding.Create(@@BlockMultiplyInplace32,
    @BlockMultiplyInplace32Native) do
  begin
    Add(@BlockMultiplyInplace32Native);
  end;

  // Block Inplace Multiply Binding (64 bit)
  with TFunctionBinding.Create(@@BlockMultiplyInplace64,
    @BlockMultiplyInplace64Native) do
  begin
    Add(@BlockMultiplyInplace64Native);
  end;

  // Block Inplace Divide Binding (32 bit)
  with TFunctionBinding.Create(@@BlockDivideInplace32,
    @BlockDivideInplace32Native) do
  begin
    Add(@BlockDivideInplace32Native);
  end;

  // Block Inplace Divide Binding (64 bit)
  with TFunctionBinding.Create(@@BlockDivideInplace64,
    @BlockDivideInplace64Native) do
  begin
    Add(@BlockDivideInplace64Native);
  end;

  // Block Inplace Reverse Divide Binding (32 bit)
  with TFunctionBinding.Create(@@BlockReverseDivideInplace32,
    @BlockReverseDivideInplace32Native) do
  begin
    Add(@BlockReverseDivideInplace32Native);
  end;

  // Block Inplace Reverse Divide Binding (64 bit)
  with TFunctionBinding.Create(@@BlockReverseDivideInplace64,
    @BlockReverseDivideInplace64Native) do
  begin
    Add(@BlockReverseDivideInplace64Native);
  end;

  // Block Inplace Scale Binding (32 bit)
  with TFunctionBinding.Create(@@BlockScaleInplace32,
    @BlockScaleInplace32Native) do
  begin
    Add(@BlockScaleInplace32Native);
  end;

  // Block Inplace Scale Binding (64 bit)
  with TFunctionBinding.Create(@@BlockScaleInplace64,
    @BlockScaleInplace64Native) do
  begin
    Add(@BlockScaleInplace64Native);
  end;

  // Block Scale Binding (32 bit)
  with TFunctionBinding.Create(@@BlockScale32, @BlockScale32Native) do
  begin
    Add(@BlockScale32Native);
  end;

  // Block Scale Binding (64 bit)
  with TFunctionBinding.Create(@@BlockScale64, @BlockScale64Native) do
  begin
    Add(@BlockScale64Native);
  end;

  // Block Scaled Addition Binding (32 bit)
  with TFunctionBinding.Create(@@BlockAdditionScale32,
    @BlockAdditionScale32Native) do
  begin
    Add(@BlockAdditionScale32Native);
  end;

  // Block Scaled Addition Binding (64 bit)
  with TFunctionBinding.Create(@@BlockAdditionScale64,
    @BlockAdditionScale64Native) do
  begin
    Add(@BlockAdditionScale64Native);
  end;

end;

initialization

BindFunctions;

end.
