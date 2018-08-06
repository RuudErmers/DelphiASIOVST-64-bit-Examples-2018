unit DAV_GuiBlend;

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
//  Several optimizations has been copied from the Graphics32 project which   //
//  is under the same license as this project.                                //
//  Please check the file GR32_Blend.pas at http://graphics32.org for         //
//  further copyright information!                                            //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{-$DEFINE AlternativeSSE2}

uses
  SysUtils, DAV_GuiCommon, DAV_Bindings;


{ Function Prototypes }

type
  TBlendMergePixel        = function(Foreground, Background: TPixel32): TPixel32;
  TBlendMergePixelInplace = procedure(Foreground: TPixel32; var Background: TPixel32);
  TBlendMergePixelLine    = procedure(Foreground: TPixel32; Destination: PPixel32; Count: Cardinal);
  TBlendMergeLine         = procedure(Source, Destination: PPixel32; Count: Cardinal);
  TCombinePixel           = function(Foreground, Background: TPixel32; Weight: Cardinal): TPixel32;
  TCombinePixelInplace    = procedure(Foreground: TPixel32; var Background: TPixel32; Weight: Integer);
  TCombinePixelLine       = procedure(Foreground: TPixel32; Destination: PPixel32; Count: Integer; Weight: Cardinal);
  TCombineLine            = procedure(Source, Destination: PPixel32; Count: Integer; Weight: Cardinal);


{ Function Pointers }

var
  BlendPixel          : TBlendMergePixel;
  BlendPixelInplace   : TBlendMergePixelInplace;
  BlendPixelLine      : TBlendMergePixelLine;
  BlendLine           : TBlendMergeLine;
  CombinePixel        : TCombinePixel;
  CombinePixelInplace : TCombinePixelInplace;
  CombinePixelLine    : TCombinePixelLine;
  CombineLine         : TCombineLine;
  EMMS                : procedure;
  MergePixel          : TBlendMergePixel;
  MergePixelInplace   : TBlendMergePixelInplace;
  MergePixelLine      : TBlendMergePixelLine;
  MergeLine           : TBlendMergeLine;


{ Binding Function Pointers }

var
  GBindingBlendPixel          : TFunctionBinding;
  GBindingBlendPixelInplace   : TFunctionBinding;
  GBindingBlendPixelLine      : TFunctionBinding;
  GBindingBlendLine           : TFunctionBinding;
  GBindingCombinePixel        : TFunctionBinding;
  GBindingCombinePixelInplace : TFunctionBinding;
  GBindingCombinePixelLine    : TFunctionBinding;
  GBindingCombineLine         : TFunctionBinding;
  GBindingEMMS                : TFunctionBinding;
  GBindingMergePixel          : TFunctionBinding;
  GBindingMergePixelInplace   : TFunctionBinding;
  GBindingMergePixelLine      : TFunctionBinding;
  GBindingMergeLine           : TFunctionBinding;


{ Binding List }

var
  GBindingBlend : TFunctionBindingList;

implementation

uses
  DAV_MemoryUtils;

{$IFNDEF PUREPASCAL}
var
  BiasPointer: Pointer;
  AlphaPointer: Pointer;
  {$IFDEF AlternativeSSE2}
  ScaleBiasPointer: Pointer;
  {$ENDIF}
{$ENDIF}

const
  CBias = $00800080; // with this value the error is distributed equally

function BlendPixelNative(Foreground, Background: TPixel32): TPixel32;
{$IFDEF PUREPASCAL}
var
  Alpha : Byte;
begin
 if Foreground.A =   0 then Result := Background else
 if Foreground.A = $FF then Result := Foreground else
  begin
   Alpha := (ForeGround.ARGB shr 24);
   ForeGround.ARGB := ((((Alpha * (ForeGround.ARGB and $00FF00FF)) + CBias)
     and $FF00FF00) shr 8) or ((Alpha * ((ForeGround.ARGB and $FF00FF00)
     shr 8) + CBias) and $FF00FF00);

   Alpha := Alpha xor $FF;

   Background.ARGB := ((((Alpha * (Background.ARGB and $00FF00FF)) + CBias)
     and $FF00FF00) shr 8) or ((Alpha * ((Background.ARGB and $FF00FF00)
     shr 8) + CBias) and $FF00FF00);

   Result.ARGB := (Background.ARGB + Foreground.ARGB) or $FF000000;
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
  CMP     ECX, $FF000000
  JNC     @Done

  TEST    ECX, $FF000000
  JZ      @CopyPixel

  MOV     EAX, ECX
  SHR     RCX, 24

  MOV     R8D, EAX
  AND     EAX, $00FF00FF
  AND     R8D, $FF00FF00
  IMUL    EAX, ECX
  SHR     R8D, 8
  IMUL    R8D, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     R8D, CBias
  AND     R8D, $FF00FF00
  OR      EAX, R8D

  XOR     ECX, $000000FF
  MOV     R8D, EDX
  AND     EDX, $00FF00FF
  AND     R8D, $FF00FF00
  IMUL    EDX, ECX
  SHR     R8D, 8
  IMUL    R8D, ECX
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     R8D, CBias
  AND     R8D, $FF00FF00
  OR      R8D, EDX

  ADD     EAX, R8D
  OR      EAX, $FF000000

  RET

@CopyPixel:
  MOV     EAX, EDX
  OR      EAX, $FF000000

@Done:
  RET
{$ENDIF}
{$IFDEF CPU32}
  CMP     EAX, $FF000000
  JNC     @Done

  TEST    EAX, $FF000000
  JZ      @CopyPixel

  MOV     ECX, EAX
  SHR     ECX, 24

  PUSH    EBX

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  XOR     ECX, $000000FF
  MOV     EBX, EDX
  AND     EDX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EDX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, EDX

  ADD     EAX, EBX
  OR      EAX, $FF000000

  POP     EBX
  RET

@CopyPixel:
  MOV     EAX, EDX
  OR      EAX, $FF000000

@Done:
  RET
{$ENDIF}
{$ENDIF}
end;

procedure BlendPixelInplaceNative(Foreground: TPixel32; var Background: TPixel32);
{$IFDEF PUREPASCAL}
var
  Alpha : Byte;
begin
 if Foreground.A = 0 then Exit;

 if Foreground.A = $FF then
  begin
   Background := Foreground;
   Exit;
  end;

 Alpha := (ForeGround.ARGB shr 24);
 ForeGround.ARGB := ((((Alpha * (ForeGround.ARGB and $00FF00FF)) + CBias)
   and $FF00FF00) shr 8) or ((Alpha * ((ForeGround.ARGB and $FF00FF00)
   shr 8) + CBias) and $FF00FF00);

 Alpha := Alpha xor $FF;

 Background.ARGB := ((((Alpha * (Background.ARGB and $00FF00FF)) + CBias)
   and $FF00FF00) shr 8) or ((Alpha * ((Background.ARGB and $FF00FF00)
   shr 8) + CBias) and $FF00FF00);

 Background.ARGB := (Background.ARGB + Foreground.ARGB) or $FF000000;
{$ELSE}
asm
{$IFDEF CPUx86_64}
  TEST    ECX, $FF000000
  JZ      @Done

  MOV     EAX, ECX
  SHR     ECX, 24

  CMP     ECX, $FF
  JZ      @CopyPixel

  MOV     R8D, EAX
  AND     EAX, $00FF00FF
  AND     R8D, $FF00FF00
  IMUL    EAX, ECX
  SHR     R8D, 8
  IMUL    R8D, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     R8D, CBias
  AND     R8D, $FF00FF00
  OR      EAX, R8D

  MOV     R9D, [RDX]
  XOR     ECX, $000000FF
  MOV     R8D, R9D
  AND     R9D, $00FF00FF
  AND     R8D, $FF00FF00
  IMUL    R9D, ECX
  SHR     R8D, 8
  IMUL    R8D, ECX
  ADD     R9D, CBias
  AND     R9D, $FF00FF00
  SHR     R9D, 8
  ADD     R8D, CBias
  AND     R8D, $FF00FF00
  OR      R8D, R9D

  ADD     EAX, R8D
  OR      EAX, $FF000000
  MOV     [RDX], EAX

  RET

@CopyPixel:
  OR      EAX, $FF000000
  MOV     [RDX], EAX

@Done:
  RET
{$ENDIF}
{$IFDEF CPU32}
  TEST    EAX, $FF000000
  JZ      @Done

  MOV     ECX, EAX
  SHR     ECX, 24

  CMP     ECX, $FF
  JZ      @CopyPixel

  PUSH    EBX
  PUSH    ESI

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  MOV     ESI, [EDX]
  XOR     ECX, $000000FF
  MOV     EBX, ESI
  AND     ESI, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    ESI, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     ESI, CBias
  AND     ESI, $FF00FF00
  SHR     ESI, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, ESI

  ADD     EAX, EBX
  OR      EAX, $FF000000
  MOV     [EDX], EAX

  POP     ESI
  POP     EBX
  RET

@CopyPixel:
  OR      EAX, $FF000000
  MOV     [EDX], EAX

@Done:
  RET
{$ENDIF}
{$ENDIF}
end;

procedure BlendPixelLineNative(Foreground: TPixel32; Destination: PPixel32; Count: Integer);
{$IFDEF PUREPASCAL}
begin
 while Count > 0 do
  begin
   BlendPixelInplace(Foreground, Destination^);
   Inc(Destination);
   Dec(Count);
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
  TEST    R8D, R8D
  JZ      @Done

  TEST    ECX, $FF000000
  JZ      @Done

  PUSH    RDI
  MOV     RDI, RDX

  MOV     R9D, ECX
  SHR     R9D, 24

  CMP     R9D, $FF
  JZ      @CopyPixel

  MOV     EAX, ECX
  AND     ECX, $00FF00FF
  AND     EAX, $FF00FF00
  IMUL    ECX, R9D
  SHR     EAX, 8
  IMUL    EAX, R9D
  ADD     ECX, CBias
  AND     ECX, $FF00FF00
  SHR     ECX, 8
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  OR      ECX, EAX
  XOR     R9D, $000000FF

@LoopStart:

  MOV     EDX, [RDI]
  MOV     EAX, EDX
  AND     EDX, $00FF00FF
  AND     EAX, $FF00FF00
  IMUL    EDX, R9D
  SHR     EAX, 8
  IMUL    EAX, R9D
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  OR      EAX, EDX

  ADD     EAX, ECX

  OR      EAX, $FF000000
  MOV     [RDI], EAX

@NextPixel:
  ADD     RDI, 4

  DEC     R8D
  JNZ     @LoopStart

  POP     RDI

@Done:
  RET

@CopyPixel:
  MOV     [RDI], ECX
  ADD     RDI, 4

  DEC     R8D
  JNZ     @CopyPixel

  POP     RDI
{$ENDIF}

{$IFDEF CPU32}
  TEST    ECX, ECX
  JZ      @Done

  TEST    EAX, $FF000000
  JZ      @Done

  PUSH    EBX
  PUSH    ESI
  PUSH    EDI

  MOV     EDI, EDX

  MOV     ESI, EAX
  SHR     ESI, 24

  CMP     ESI, $FF
  JZ      @CopyPixel

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ESI
  SHR     EBX, 8
  IMUL    EBX, ESI
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX
  XOR     ESI, $000000FF

@LoopStart:

  MOV     EDX, [EDI]
  MOV     EBX, EDX
  AND     EDX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EDX, ESI
  SHR     EBX, 8
  IMUL    EBX, ESI
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, EDX

  ADD     EBX, EAX

  OR      EBX, $FF000000
  MOV     [EDI], EBX

@NextPixel:
  ADD     EDI, 4

  DEC     ECX
  JNZ     @LoopStart

  POP     EDI
  POP     ESI
  POP     EBX

@Done:
  RET

@CopyPixel:
  MOV     [EDI], EAX
  ADD     EDI, 4

  DEC     ECX
  JNZ     @CopyPixel

  POP     EDI
  POP     ESI
  POP     EBX
{$ENDIF}
{$ENDIF}
end;

procedure BlendLineNative(Source, Destination: PPixel32; Count: Integer);
{$IFDEF PUREPASCAL}
begin
 while Count > 0 do
  begin
   BlendPixelInplace(Source^, Destination^);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
  TEST    R8D, R8D
  JZ      @Done

  PUSH    RDI

  MOV     R9, RCX
  MOV     RDI, RDX

@LoopStart:
  MOV     ECX, [RSI]
  TEST    ECX, $FF000000
  JZ      @NextPixel

  PUSH    R8

  MOV     R8D, ECX
  SHR     R8D, 24

  CMP     R8D, $FF
  JZ      @CopyPixel

  MOV     EAX, ECX
  AND     ECX, $00FF00FF
  AND     EAX, $FF00FF00
  IMUL    ECX, R8D
  SHR     EAX, 8
  IMUL    EAX, R8D
  ADD     ECX, CBias
  AND     ECX, $FF00FF00
  SHR     ECX, 8
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  OR      ECX, EAX

  MOV     EDX, [RDI]
  XOR     R8D, $000000FF
  MOV     EAX, EDX
  AND     EDX, $00FF00FF
  AND     EAX, $FF00FF00
  IMUL    EDX, R8D
  SHR     EAX, 8
  IMUL    EAX, R8D
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  OR      EAX, EDX

  ADD     ECX, EAX
@CopyPixel:
  OR      ECX, $FF000000
  MOV     [RDI], ECX
  POP     R8

@NextPixel:
  ADD     R9, 4
  ADD     RDI, 4

  DEC     R8D
  JNZ     @LoopStart

  POP     RDI

@Done:
  RET
{$ENDIF}

{$IFDEF CPU32}
  TEST    ECX, ECX
  JZ      @Done

  PUSH    EBX
  PUSH    ESI
  PUSH    EDI

  MOV     ESI, EAX
  MOV     EDI, EDX

@LoopStart:
  MOV     EAX, [ESI]
  TEST    EAX, $FF000000
  JZ      @NextPixel

  PUSH    ECX

  MOV     ECX, EAX
  SHR     ECX, 24

  CMP     ECX, $FF
  JZ      @CopyPixel

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  MOV     EDX, [EDI]
  XOR     ECX, $000000FF
  MOV     EBX, EDX
  AND     EDX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EDX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, EDX

  ADD     EAX, EBX
@CopyPixel:
  OR      EAX, $FF000000
  MOV     [EDI], EAX
  POP     ECX

@NextPixel:
  ADD     ESI, 4
  ADD     EDI, 4

  DEC     ECX
  JNZ     @LoopStart

  POP     EDI
  POP     ESI
  POP     EBX

@Done:
  RET
{$ENDIF}
{$ENDIF}
end;

function CombinePixelNative(ForeGround, Background: TPixel32; Weight: Cardinal): TPixel32;
{$IFDEF PUREPASCAL}
begin
 if Weight = 0 then Result := Background else
 if Weight >= $FF then Result := ForeGround else
  begin
   ForeGround.ARGB := (((Weight * ((ForeGround.ARGB and $FF00FF00) shr 8)) +
     CBias) and $FF00FF00) + ((((Weight * (ForeGround.ARGB and $00FF00FF)) +
     CBias) and $FF00FF00) shr 8);

   Weight := Weight xor $000000FF;
   Background.ARGB := (((Weight * ((Background.ARGB and $FF00FF00) shr 8)) +
     CBias) and $FF00FF00) + ((((Weight * (Background.ARGB and $00FF00FF)) +
     CBias) and $FF00FF00) shr 8);

   Result.ARGB := Background.ARGB + Foreground.ARGB;
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
  TEST    R8,R8
  JZ      @Copy

  CMP     R8D, $FF
  JE      @Done

  MOV     EAX, ECX
  AND     EAX, $00FF00FF
  AND     ECX, $FF00FF00
  IMUL    EAX, R8D
  SHR     ECX, 8
  IMUL    ECX, R8D
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     ECX, CBias
  AND     ECX, $FF00FF00
  OR      EAX, ECX

  XOR     R8D, $000000FF
  MOV     ECX, EDX
  AND     EDX, $00FF00FF
  AND     ECX, $FF00FF00
  IMUL    EDX, R8D
  SHR     ECX, 8
  IMUL    ECX, R8D
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     ECX, CBias
  AND     ECX, $FF00FF00
  OR      ECX, EDX

  ADD     EAX, ECX
  RET

@Copy:
  MOV     EAX, EDX
@Done:
  RET
{$ENDIF}

{$IFDEF CPU32}
  JCXZ    @Copy

  CMP     ECX, $FF
  JE      @Done

  PUSH    EBX

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  XOR     ECX, $000000FF
  MOV     EBX, EDX
  AND     EDX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EDX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, EDX

  ADD     EAX, EBX

  POP     EBX
  RET

@Copy:
  MOV     EAX, EDX
@Done:
  RET
{$ENDIF}
{$ENDIF}
end;

procedure CombinePixelInplaceNative(ForeGround: TPixel32; var Background: TPixel32; Weight: Cardinal);
{$IFDEF PUREPASCAL}
begin
 if Weight = 0 then Exit else
 if Weight >= $FF then Background := ForeGround else
  begin
   ForeGround.ARGB := (((Weight * ((ForeGround.ARGB and $FF00FF00) shr 8)) +
     CBias) and $FF00FF00) + ((((Weight * (ForeGround.ARGB and $00FF00FF)) +
     CBias) and $FF00FF00) shr 8);

   Weight := Weight xor $000000FF;
   Background.ARGB := (((Weight * ((Background.ARGB and $FF00FF00) shr 8)) +
     CBias) and $FF00FF00) + ((((Weight * (Background.ARGB and $00FF00FF)) +
     CBias) and $FF00FF00) shr 8);

   Background.ARGB := Background.ARGB + Foreground.ARGB;
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
  TEST    R8,R8
  JZ      @Done

  CMP     R8D, $FF
  JZ      @Copy

  MOV     EAX, ECX
  AND     EAX, $00FF00FF
  AND     ECX, $FF00FF00
  IMUL    EAX, R8D
  SHR     ECX, 8
  IMUL    ECX, R8D
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     ECX, CBias
  AND     ECX, $FF00FF00
  OR      EAX, ECX

  MOV     R9D, [RDX]
  XOR     R8D, $000000FF
  MOV     ECX, R9D
  AND     R9D, $00FF00FF
  AND     ECX, $FF00FF00
  IMUL    R9D, R8D
  SHR     ECX, 8
  IMUL    ECX, R8D
  ADD     R9D, CBias
  AND     R9D, $FF00FF00
  SHR     R9D, 8
  ADD     ECX, CBias
  AND     ECX, $FF00FF00
  OR      ECX, R9D

  ADD     EAX, ECX

  MOV     [RDX], EAX
@Done:
  RET

@Copy:
  MOV     [RDX], ECX
  RET
{$ENDIF}

{$IFDEF CPU32}
  JCXZ    @Done

  CMP     ECX, $FF
  JZ      @Copy

  PUSH    EBX
  PUSH    ESI

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  MOV     ESI, [EDX]
  XOR     ECX, $000000FF
  MOV     EBX, ESI
  AND     ESI, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    ESI, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     ESI, CBias
  AND     ESI, $FF00FF00
  SHR     ESI, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, ESI

  ADD     EAX, EBX

  MOV     [EDX], EAX

  POP     ESI
  POP     EBX
@Done:
  RET

@Copy:
  MOV     [EDX], EAX
  RET
{$ENDIF}
{$ENDIF}
end;

procedure CombinePixelLineNative(Foreground: TPixel32; Destination: PPixel32;
  Count: Integer; Weight: Cardinal);
begin
 while Count > 0 do
  begin
   CombinePixelInplace(Foreground, Destination^, Weight);
   Inc(Destination);
   Dec(Count);
  end;
end;

procedure CombineLineNative(Source, Destination: PPixel32; Count: Integer;
  Weight: Cardinal);
{$IFDEF PUREPASCAL}
begin
 while Count > 0 do
  begin
   CombinePixelInplace(Source^, Destination^, Weight);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
  // Probably contain a bug!!!
  TEST      R8D, R8D
  JZ        @Done

  TEST      R9D, R9D
  JZ        @LoopEnd

  CMP       R9D, $FF
  JZ        @DoneMove       // R9D = 255  =>  copy src to dst

  PUSH      RDI
  PUSH      RSI

@LoopStart:
  PUSH      RCX
  MOV       EDI, [RCX]
  AND       ECX, $00FF00FF
  AND       EDI, $FF00FF00
  IMUL      ECX, R9D
  SHR       EDI, 8
  IMUL      EDI, R9D
  ADD       ECX, CBias
  AND       ECX, $FF00FF00
  SHR       ECX, 8
  ADD       EDI, CBias
  AND       EDI, $FF00FF00
  OR        ECX, EDI

  MOV       ESI, [RDX]
  XOR       R9D, $000000FF
  MOV       EDI, ESI
  AND       ESI, $00FF00FF
  AND       EDI, $FF00FF00
  IMUL      ESI, R9D
  SHR       EDI, 8
  IMUL      EDI, R9D
  ADD       ESI, CBias
  AND       ESI, $FF00FF00
  SHR       ESI, 8
  ADD       EDI, CBias
  AND       EDI, $FF00FF00
  OR        EDI, ESI

  ADD       ECX, EDI

  MOV       [RDX], ECX

  POP       RCX
  ADD       RCX, 4
  ADD       RDX, 4

  DEC       R8D
  JNZ       @LoopStart

@LoopEnd:
  POP       RSI
  POP       RDI

  POP       RBP
@Done:
  RET       $0004

@DoneMove:
  SHL       R8D, 2
  CALL      Move
{$ENDIF}

{$IFDEF CPU32}
  // Probably contain a bug!!!
  TEST      ECX, ECX
  JZ        @Done

  PUSH      EBX
  MOV       EBX, Weight

  TEST      EBX, EBX
  JZ        @LoopEnd        // weight is zero

  CMP       EBX, $FF
  JZ        @DoneMove       // weight = 255  =>  copy src to dst

  PUSH      EDI
  PUSH      ESI

@LoopStart:
  PUSH      EAX
  MOV       EDI, [EAX]
  AND       EAX, $00FF00FF
  AND       EDI, $FF00FF00
  IMUL      EAX, EBX
  SHR       EDI, 8
  IMUL      EDI, EBX
  ADD       EAX, CBias
  AND       EAX, $FF00FF00
  SHR       EAX, 8
  ADD       EDI, CBias
  AND       EDI, $FF00FF00
  OR        EAX, EDI

  MOV       ESI, [EDX]
  XOR       EBX, $000000FF
  MOV       EDI, ESI
  AND       ESI, $00FF00FF
  AND       EDI, $FF00FF00
  IMUL      ESI, EBX
  SHR       EDI, 8
  IMUL      EDI, EBX
  ADD       ESI, CBias
  AND       ESI, $FF00FF00
  SHR       ESI, 8
  ADD       EDI, CBias
  AND       EDI, $FF00FF00
  OR        EDI, ESI

  ADD       EAX, EDI

  MOV       [EDX], EAX

  POP       EAX
  ADD       EAX, 4
  ADD       EDX, 4

  DEC       ECX
  JNZ       @LoopStart

@LoopEnd:
  POP       ESI
  POP       EDI

  POP       EBX
  POP       EBP
@Done:
  RET       $0004

@DoneMove:
  SHL       ECX, 2
  CALL      Move
  POP       EBX
{$ENDIF}
{$ENDIF}
end;

function MergePixelNative(Foreground, Background: TPixel32): TPixel32;
var
  Temp  : Integer;
  Scale : Integer;
begin
 if Foreground.A = $FF then Result := Foreground else
 if Foreground.A = $0  then Result := Background else
 if Background.A = $0  then Result := Foreground else
 if Background.A = $FF
  then Result := BlendPixel(Foreground, Background)
  else
   begin
    Temp := Sqr($FF) - (Foreground.A xor $FF) * (Background.A xor $FF);
    Result.A := (Temp + $80) shr 8;
    Scale := (Sqr($FF) * Foreground.A) div Temp;
    Result.R := Background.R + (Scale * ($FF + Foreground.R - Background.R) + $7F) shr 8 + 1 - Scale;
    Result.G := Background.G + (Scale * ($FF + Foreground.G - Background.G) + $7F) shr 8 + 1 - Scale;
    Result.B := Background.B + (Scale * ($FF + Foreground.B - Background.B) + $7F) shr 8 + 1 - Scale;
   end;
end;

procedure MergePixelInplaceNative(Foreground: TPixel32; var Background: TPixel32);
begin
 Background := MergePixelNative(Foreground, Background);
end;

procedure MergeLineNative(Source, Destination: PPixel32; Count: Cardinal);
begin
 while Count > 0 do
  begin
   Destination^ := MergePixel(Source^, Destination^);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
end;

procedure EMMSNative;
begin
 // dummy
end;



{ MMX Functions }


{$IFNDEF PUREPASCAL}

procedure EMMSMMX;
asm
  EMMS
end;

function BlendPixelMMX(Foreground, Background: TPixel32): TPixel32;
asm
{$IFDEF CPUx86_64}
  MOVD      MM0, ECX
  PXOR      MM3, MM3
  MOVD      MM2, EDX
  PUNPCKLBW MM0, MM3
  MOV       RAX, BiasPointer
  PUNPCKLBW MM2, MM3
  MOVQ      MM1, MM0
  PUNPCKHWD MM1, MM1
  PSUBW     MM0, MM2
  PUNPCKHDQ MM1, MM1
  PSLLW     MM2, 8
  PMULLW    MM0, MM1
  PADDW     MM2, [RAX]
  PADDW     MM2, MM0
  PSRLW     MM2, 8
  PACKUSWB  MM2, MM3
  MOVD      EAX, MM2
{$ENDIF}

{$IFDEF CPU32}
  MOVD      MM0, EAX
  PXOR      MM3, MM3
  MOVD      MM2, EDX
  PUNPCKLBW MM0, MM3
  MOV       ECX, BiasPointer
  PUNPCKLBW MM2, MM3
  MOVQ      MM1, MM0
  PUNPCKHWD MM1, MM1
  PSUBW     MM0, MM2
  PUNPCKHDQ MM1, MM1
  PSLLW     MM2, 8
  PMULLW    MM0, MM1
  PADDW     MM2, [ECX]
  PADDW     MM2, MM0
  PSRLW     MM2, 8
  PACKUSWB  MM2, MM3
  MOVD      EAX, MM2
{$ENDIF}
end;

procedure BlendPixelInplaceMMX(Foreground: TPixel32; var Background: TPixel32);
asm
{$IFDEF CPUx86_64}
  TEST      ECX, $FF000000
  JZ        @Done
  CMP       ECX, $FF000000
  JNC       @Copy

  PXOR      MM3, MM3
  MOVD      MM0, ECX
  MOVD      MM2, [RDX]
  PUNPCKLBW MM0, MM3
  MOV       RAX, BiasPointer
  PUNPCKLBW MM2, MM3
  MOVQ      MM1, MM0
  PUNPCKHWD MM1, MM1
  PSUBW     MM0, MM2
  PUNPCKHDQ MM1, MM1
  PSLLW     MM2, 8
  PMULLW    MM0, MM1
  PADDW     MM2, [RAX]
  PADDW     MM2, MM0
  PSRLW     MM2, 8
  PACKUSWB  MM2, MM3
  MOVD      [RDX], MM2

@Done:
  RET

@Copy:
  MOV       [RDX], ECX
{$ENDIF}

{$IFDEF CPU32}
  TEST      EAX, $FF000000
  JZ        @Done
  CMP       EAX, $FF000000
  JNC       @Copy

  PXOR      MM3, MM3
  MOVD      MM0, EAX
  MOVD      MM2, [EDX]
  PUNPCKLBW MM0, MM3
  MOV       ECX, BiasPointer
  PUNPCKLBW MM2, MM3
  MOVQ      MM1, MM0
  PUNPCKHWD MM1, MM1
  PSUBW     MM0, MM2
  PUNPCKHDQ MM1, MM1
  PSLLW     MM2, 8
  PMULLW    MM0, MM1
  PADDW     MM2, [ECX]
  PADDW     MM2, MM0
  PSRLW     MM2, 8
  PACKUSWB  MM2, MM3
  MOVD      [EDX], MM2

@Done:
  RET

@Copy:
  MOV       [EDX], EAX
{$ENDIF}
end;

procedure BlendLineMMX(Source, Destination: PPixel32; Count: Integer);
asm
{$IFDEF CPUx86_64}
  TEST      R8D, R8D
  JZ        @Done

@LoopStart:
  MOV       EAX, [RCX]
  TEST      EAX, $FF000000
  JZ        @NextPixel
  CMP       EAX, $FF000000
  JNC       @CopyPixel

  MOVD      MM0, EAX
  PXOR      MM3, MM3
  MOVD      MM2, [RDX]
  PUNPCKLBW MM0, MM3
  MOV       RAX, BiasPointer
  PUNPCKLBW MM2, MM3
  MOVQ      MM1, MM0
  PUNPCKHWD MM1, MM1
  PSUBW     MM0, MM2
  PUNPCKHDQ MM1, MM1
  PSLLW     MM2, 8
  PMULLW    MM0, MM1
  PADDW     MM2, [RAX]
  PADDW     MM2, MM0
  PSRLW     MM2, 8
  PACKUSWB  MM2, MM3
  MOVD      EAX, MM2

@CopyPixel:
  MOV       [RDX], EAX

@NextPixel:
  ADD       RCX, 4
  ADD       RDX, 4

  DEC       R8D
  JNZ       @LoopStart

@Done:
  RET
{$ENDIF}

{$IFDEF CPU32}
  TEST      ECX, ECX
  JZ        @Done

  PUSH      EDI
  MOV       EDI, EAX

@LoopStart:
  MOV       EAX, [EDI]
  TEST      EAX, $FF000000
  JZ        @NextPixel
  CMP       EAX, $FF000000
  JNC       @CopyPixel

  MOVD      MM0, EAX
  PXOR      MM3, MM3
  MOVD      MM2, [EDX]
  PUNPCKLBW MM0, MM3
  MOV       EAX, BiasPointer
  PUNPCKLBW MM2, MM3
  MOVQ      MM1, MM0
  PUNPCKHWD MM1, MM1
  PSUBW     MM0, MM2
  PUNPCKHDQ MM1, MM1
  PSLLW     MM2, 8
  PMULLW    MM0, MM1
  PADDW     MM2, [EAX]
  PADDW     MM2, MM0
  PSRLW     MM2, 8
  PACKUSWB  MM2, MM3
  MOVD      EAX, MM2

@CopyPixel:
  MOV       [EDX], EAX

@NextPixel:
  ADD       EDI, 4
  ADD       EDX, 4

  DEC       ECX
  JNZ       @LoopStart

  POP       EDI

@Done:
  RET
{$ENDIF}
end;

function CombinePixelMMX(ForeGround, Background: TPixel32; Weight: TPixel32): TPixel32;
asm
{$IFDEF CPUx86_64}
  MOVD      MM1, ECX
  PXOR      MM0, MM0
  SHL       R8, 4

  MOVD      MM2, EDX
  PUNPCKLBW MM1, MM0
  PUNPCKLBW MM2, MM0

  ADD       R8, AlphaPointer

  PSUBW     MM1, MM2
  PMULLW    MM1, [R8]
  PSLLW     MM2, 8

  MOV       RAX, BiasPointer

  PADDW     MM2, [RAX]
  PADDW     MM1, MM2
  PSRLW     MM1, 8
  PACKUSWB  MM1, MM0
  MOVD      EAX, MM1
{$ENDIF}

{$IFDEF CPU32}
  MOVD      MM1, EAX
  PXOR      MM0, MM0
  SHL       ECX, 4

  MOVD      MM2, EDX
  PUNPCKLBW MM1, MM0
  PUNPCKLBW MM2, MM0

  ADD       ECX, AlphaPointer

  PSUBW     MM1, MM2
  PMULLW    MM1, [ECX]
  PSLLW     MM2, 8

  MOV       ECX, BiasPointer

  PADDW     MM2, [ECX]
  PADDW     MM1, MM2
  PSRLW     MM1, 8
  PACKUSWB  MM1, MM0
  MOVD      EAX, MM1
{$ENDIF}
end;

procedure CombinePixelInplaceMMX(F: TPixel32; var B: TPixel32; W: TPixel32);
asm
{$IFDEF CPUx86_64}
  TEST      R8, R8
  JZ        @Done

  CMP       R8D, $FF
  JZ        @Copy

  MOVD      MM1, ECX
  PXOR      MM0, MM0

  SHL       R8, 4

  MOVD      MM2, [RDX]
  PUNPCKLBW MM1, MM0
  PUNPCKLBW MM2, MM0

  ADD       R8, AlphaPointer

  PSUBW     MM1, MM2
  PMULLW    MM1, [R8]
  PSLLW     MM2, 8

  MOV       RAX, BiasPointer

  PADDW     MM2, [RAX]
  PADDW     MM1, MM2
  PSRLW     MM1, 8
  PACKUSWB  MM1, MM0
  MOVD      [RDX], MM1

@Done:
  RET

@Copy:
  MOV       [RDX], ECX
{$ENDIF}

{$IFDEF CPU32}
  JCXZ      @Done

  CMP       ECX, $FF
  JZ        @Copy

  MOVD      MM1, EAX
  PXOR      MM0, MM0

  SHL       ECX, 4

  MOVD      MM2, [EDX]
  PUNPCKLBW MM1, MM0
  PUNPCKLBW MM2, MM0

  ADD       ECX, AlphaPointer

  PSUBW     MM1, MM2
  PMULLW    MM1, [ECX]
  PSLLW     MM2, 8

  MOV       ECX, BiasPointer

  PADDW     MM2, [ECX]
  PADDW     MM1, MM2
  PSRLW     MM1, 8
  PACKUSWB  MM1, MM0
  MOVD      [EDX], MM1

@Done:
  RET

@Copy:
  MOV       [EDX], EAX
{$ENDIF}
end;

procedure CombineLineMMX(Source, Destination: PPixel32; Count: Integer;
  Weight: Cardinal);
asm
{$IFDEF CPUx86_64}
  TEST      R8D, R8D
  JZ        @Done

  TEST      R9D, R9D
  JZ        @LoopEnd        // R9D is zero

  CMP       R9B, $FF
  JZ        @DoneMove       // R9D = 255  =>  copy src to dst

  SHL       R9, 4
  ADD       R9, AlphaPointer
  MOVQ      MM3, QWORD PTR [R9]
  MOV       RAX, BiasPointer
  MOVQ      MM4, QWORD PTR [RAX]

@LoopStart:
  MOVD      MM1, [RCX]
  PXOR      MM0, MM0
  MOVD      MM2, [RDX]
  PUNPCKLBW MM1, MM0
  PUNPCKLBW MM2, MM0

  PSUBW     MM1, MM2
  PMULLW    MM1, MM3
  PSLLW     MM2, 8

  PADDW     MM2, MM4
  PADDW     MM1, MM2
  PSRLW     MM1, 8
  PACKUSWB  MM1, MM0
  MOVD      [RDX], MM1

  ADD       RCX, 4
  ADD       RDX, 4

  DEC       R8D
  JNZ       @LoopStart
@LoopEnd:
  POP       RBP
@Done:
  RET       $0004

@DoneMove:
  SHL       R8D, 2
  CALL      Move
{$ENDIF}

{$IFDEF CPU32}
  TEST      ECX, ECX
  JZ        @Done

  PUSH      EBX
  MOV       EBX, Weight

  TEST      EBX, EBX
  JZ        @LoopEnd        // weight is zero

  CMP       EBX, $FF
  JZ        @DoneMove       // weight = 255  =>  copy src to dst

  SHL       EBX, 4
  ADD       EBX, AlphaPointer
  MOVQ      MM3, [EBX]
  MOV       EBX, BiasPointer
  MOVQ      MM4, [EBX]

@LoopStart:
  MOVD      MM1, [EAX]
  PXOR      MM0, MM0
  MOVD      MM2, [EDX]
  PUNPCKLBW MM1, MM0
  PUNPCKLBW MM2, MM0

  PSUBW     MM1, MM2
  PMULLW    MM1, MM3
  PSLLW     MM2, 8

  PADDW     MM2, MM4
  PADDW     MM1, MM2
  PSRLW     MM1, 8
  PACKUSWB  MM1, MM0
  MOVD      [EDX], MM1

  ADD       EAX, 4
  ADD       EDX, 4

  DEC       ECX
  JNZ       @LoopStart
@LoopEnd:
  POP       EBX
  POP       EBP
@Done:
  RET       $0004

@DoneMove:
  SHL       ECX, 2
  CALL      Move
  POP       EBX
{$ENDIF}
end;

(*
function MergePixelMMX(Foreground, Background: TPixel32): TPixel32;
asm
  TEST      EAX, $FF000000  // foreground completely transparent =>
  JZ        @CopyPixel      // result = background
  CMP       EAX, $FF000000  // foreground completely opaque =>
  JNC       @Done           // result = foreground
  TEST      EDX, $FF000000  // background completely transparent =>
  JZ        @Done           // result = foreground

  PXOR      MM7, MM7        // MM7  <-  00 00 00 00 00 00 00 00
  MOVD      MM0, EAX        // MM0  <-  00 00 00 00 Fa Fr Fg Fb
  MOVD      MM1, EDX        // MM1  <-  00 00 00 00 Ba Br Bg Bb
  SHR       EAX, $18        // EAX  <-  00 00 00 Fa
  SHR       EDX, $18        // EDX  <-  00 00 00 Ba
  MOV       ECX, EDX        // ECX  <-  00 00 00 Ba
//  IMUL

  // Fa + Ba - Fa * Ba
  // Fa * (1 - Ba) + Ba

{
  ROR       EDX, 24         // EDX  <-  Br Bg Bb Ba
  MOVZX     ECX, DL         // ECX  <-  00 00 00 Ba
  PUNPCKLBW MM0, MM7        // MM0  <-  00 Fa 00 Fr 00 Fg 00 Fb
  SUB       EAX, $FF        // EAX  <-  (Fa - 1)
  XOR       ECX, $FF        // ECX  <-  (1 - Ba)
  IMUL      ECX, EAX        // ECX  <-  (Fa - 1) * (1 - Ba)  =  Ra - 1
  IMUL      ECX, $8081      // ECX  <-  Xa 00 00 00
  ADD       ECX, $8081*$FF*$FF
  SHR       ECX, 15         // ECX  <-  Ra
  MOV       DL, CH          // EDX  <-  Br Bg Bb Ra
  ROR       EDX, 8          // EDX  <-  Ra Br Bg Bb
  MOVD      MM1, EDX        // MM1  <-  Ra Br Bg Bb
  PUNPCKLBW MM1, MM7        // MM1  <-  00 Ra 00 Br 00 Bg 00 Bb
  SHL       EAX, 20         // EAX  <-  Fa 00 00
  PSUBW     MM0, MM1        // MM0  <-  ** Da ** Dr ** Dg ** Db
  ADD       EAX, $0FF01000
  PSLLW     MM0, 4
  XOR       EDX, EDX        // EDX  <-  00
  DIV       EAX, ECX        // EAX  <-  Fa / Ra  =  Wa
  MOVD      MM4, EAX        // MM3  <-  Wa
//  PSHUFLW   MM4, MM4, $C0   // MM3  <-  00 00 ** Wa ** Wa ** Wa
  PMULHW    MM0, MM4        // MM0  <-  00 00 ** Pr ** Pg ** Pb
  PADDW     MM0, MM1        // MM0  <-  00 Ra 00 Rr 00 Rg 00 Rb
  PACKUSWB  MM0, MM7        // MM0  <-  Ra Rr Rg Rb
  MOVD      EAX, MM0
}

  RET

@CopyPixel:
  MOV       EAX, EDX

@Done:
end;
*)


{ SSE2 }

function BlendPixelSSE2(Foreground, Background: TPixel32): TPixel32;
asm
{$IFDEF CPUx86_64}

{$IFDEF AlternativeSSE2}
  MOVD      XMM0, ECX         // XMM0 contains foreground
  PXOR      XMM3, XMM3        // XMM3 is zero
  PUNPCKLBW XMM0, XMM3        // stretch foreground
  MOVD      XMM1, EDX         // XMM1 contains background
  PUNPCKLWD XMM0, XMM3        // stretch foreground (even further)
  MOV       RAX,  ScaleBiasPointer
  PSHUFD    XMM2, XMM0, $FF   // XMM2 contains foreground alpha
  PUNPCKLBW XMM1, XMM3        // stretch background
  PMULLD    XMM2, [RAX]       // scale alpha
  PUNPCKLWD XMM1, XMM3        // stretch background (even further)
  PSUBD     XMM0, XMM1        // XMM0 = XMM0 - XMM1 (= foreground - background)
  PMULLD    XMM0, XMM2        // XMM0 = XMM0 * XMM2 (= alpha - (  "  )        )
  PSLLD     XMM1, 24          // shift left XMM1 (background)
  PADDD     XMM0, [RAX + $10] // add bias
  PADDD     XMM0, XMM1        // add background to weighted difference
  PSRLD     XMM0, 24          // shift right XMM0
  PACKUSWB  XMM0, XMM3        // pack data
  PACKUSWB  XMM0, XMM3        // pack data
  MOVD      EAX, XMM0         // return result

{$ELSE}

  MOVD      XMM0, ECX
  PXOR      XMM3, XMM3
  MOVD      XMM2, EDX
  PUNPCKLBW XMM0, XMM3
  MOV       RAX,  BiasPointer
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM0
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [RAX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      EAX,  XMM2
{$ENDIF}

{$ENDIF}

{$IFDEF CPU32}

{$IFDEF AlternativeSSE2}
  MOVD      XMM0, EAX         // XMM0 contains foreground
  PXOR      XMM3, XMM3        // XMM3 is zero
  PUNPCKLBW XMM0, XMM3        // stretch foreground
  MOVD      XMM1, EDX         // XMM1 contains background
  PUNPCKLWD XMM0, XMM3        // stretch foreground (even further)
  MOV       ECX,  ScaleBiasPointer
  PSHUFD    XMM2, XMM0, $FF   // XMM2 contains foreground alpha
  PUNPCKLBW XMM1, XMM3        // stretch background
  PMULLD    XMM2, [ECX]       // scale alpha
  PUNPCKLWD XMM1, XMM3        // stretch background (even further)
  PSUBD     XMM0, XMM1        // XMM0 = XMM0 - XMM1 (= foreground - background)
  PMULLD    XMM0, XMM2        // XMM0 = XMM0 * XMM2 (= alpha - (  "  )        )
  PSLLD     XMM1, 24          // shift left XMM1 (background)
  PADDD     XMM0, [ECX + $10] // add bias
  PADDD     XMM0, XMM1        // add background to weighted difference
  PSRLD     XMM0, 24          // shift right XMM0
  PACKUSWB  XMM0, XMM3        // pack data
  PACKUSWB  XMM0, XMM3        // pack data
  MOVD      EAX, XMM0         // return result

{$ELSE}

  MOVD      XMM0, EAX
  PXOR      XMM3, XMM3
  MOVD      XMM2, EDX
  PUNPCKLBW XMM0, XMM3
  MOV       ECX,  BiasPointer
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM0
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [ECX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      EAX, XMM2
{$ENDIF}

{$ENDIF}
end;

procedure BlendPixelInplaceSSE2(Foreground: TPixel32; var Background: TPixel32);
asm
{$IFDEF CPUx86_64}

  TEST      ECX, $FF000000
  JZ        @Done
  CMP       ECX, $FF000000
  JNC       @Copy

{$IFDEF AlternativeSSE2}
  MOVD      XMM0, ECX         // XMM0 contains foreground
  PXOR      XMM3, XMM3        // XMM3 is zero
  PUNPCKLBW XMM0, XMM3        // stretch foreground
  MOVD      XMM1, [RDX]       // XMM1 contains background
  PUNPCKLWD XMM0, XMM3        // stretch foreground (even further)
  MOV       RAX,  ScaleBiasPointer
  PSHUFD    XMM2, XMM0, $FF   // XMM2 contains foreground alpha
  PUNPCKLBW XMM1, XMM3        // stretch background
  PMULLD    XMM2, [RAX]       // scale alpha
  PUNPCKLWD XMM1, XMM3        // stretch background (even further)
  PSUBD     XMM0, XMM1        // XMM0 = XMM0 - XMM1 (= foreground - background)
  PMULLD    XMM0, XMM2        // XMM0 = XMM0 * XMM2 (= alpha - (  "  )        )
  PSLLD     XMM1, 24          // shift left XMM1 (background)
  PADDD     XMM0, [RAX + $10] // add bias
  PADDD     XMM0, XMM1        // add background to weighted difference
  PSRLD     XMM0, 24          // shift right XMM0
  PACKUSWB  XMM0, XMM3        // pack data
  PACKUSWB  XMM0, XMM3        // pack data
  MOVD      [RDX], XMM0       // return result

{$ELSE}

  PXOR      XMM3, XMM3
  MOVD      XMM0, ECX
  MOVD      XMM2, [RDX]
  PUNPCKLBW XMM0, XMM3
  MOV       RAX,  BiasPointer
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM0
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [RAX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      [RDX], XMM2
{$ENDIF}

@Done:
  RET

@Copy:
  MOV       [RDX], ECX

{$ENDIF}

{$IFDEF CPU32}

  TEST      EAX, $FF000000
  JZ        @Done
  CMP       EAX, $FF000000
  JNC       @Copy

{$IFDEF AlternativeSSE2}
  MOVD      XMM0, EAX         // XMM0 contains foreground
  PXOR      XMM3, XMM3        // XMM3 is zero
  PUNPCKLBW XMM0, XMM3        // stretch foreground
  MOVD      XMM1, [EDX]       // XMM1 contains background
  PUNPCKLWD XMM0, XMM3        // stretch foreground (even further)
  MOV       ECX,  ScaleBiasPointer
  PSHUFD    XMM2, XMM0, $FF   // XMM2 contains foreground alpha
  PUNPCKLBW XMM1, XMM3        // stretch background
  PMULLD    XMM2, [ECX]       // scale alpha
  PUNPCKLWD XMM1, XMM3        // stretch background (even further)
  PSUBD     XMM0, XMM1        // XMM0 = XMM0 - XMM1 (= foreground - background)
  PMULLD    XMM0, XMM2        // XMM0 = XMM0 * XMM2 (= alpha - (  "  )        )
  PSLLD     XMM1, 24          // shift left XMM1 (background)
  PADDD     XMM0, [ECX + $10] // add bias
  PADDD     XMM0, XMM1        // add background to weighted difference
  PSRLD     XMM0, 24          // shift right XMM0
  PACKUSWB  XMM0, XMM3        // pack data
  PACKUSWB  XMM0, XMM3        // pack data
  MOVD      [EDX], XMM0       // return result

{$ELSE}

  PXOR      XMM3, XMM3
  MOVD      XMM0, EAX
  MOVD      XMM2, [EDX]
  PUNPCKLBW XMM0, XMM3
  MOV       ECX,  BiasPointer
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM0
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [ECX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      [EDX], XMM2
{$ENDIF}

@Done:
  RET

@Copy:
  MOV       [EDX], EAX

{$ENDIF}
end;

procedure BlendPixelLineSSE2(Foreground: TPixel32; Destination: PPixel32; Count: Integer);
asm
{$IFDEF CPUx86_64}
  TEST      R8D, R8D
  JZ        @Done

  TEST      ECX, $FF000000
  JZ        @Done

  MOV       RAX, RCX
  SHR       EAX, 24

  CMP       EAX, $FF
  JZ        @CopyPixel

{$IFDEF AlternativeSSE2}
  MOVD      XMM4, ECX         // XMM4 contains foreground
  PXOR      XMM3, XMM3        // XMM3 is zero
  PUNPCKLBW XMM4, XMM3        // stretch foreground
  PUNPCKLWD XMM4, XMM3        // stretch foreground (even further)
  MOV       RAX,  ScaleBiasPointer
  PSHUFD    XMM5, XMM4, $FF   // XMM2 contains foreground alpha
  PMULLD    XMM5, [RAX]       // scale alpha

@LoopStart:
  MOVD      XMM1, [RDX]       // XMM1 contains background
  PUNPCKLBW XMM1, XMM3        // stretch background
  PUNPCKLWD XMM1, XMM3        // stretch background (even further)
  MOVDQA    XMM0, XMM4        // XMM0 = stretched foreground
  PSUBD     XMM0, XMM1        // XMM0 = XMM0 - XMM1 (= foreground - background)
  PMULLD    XMM0, XMM5        // XMM0 = XMM0 * XMM2 (= alpha - (  "  )        )
  PSLLD     XMM1, 24          // shift left XMM1 (background)
  PADDD     XMM0, [RAX + $10] // add bias
  PADDD     XMM0, XMM1        // add background to weighted difference
  PSRLD     XMM0, 24          // shift right XMM0
  PACKUSWB  XMM0, XMM3        // pack data
  PACKUSWB  XMM0, XMM3        // pack data
  MOVD      [RDX], XMM0       // return result

{$ELSE}

  MOVD      XMM4, ECX
  PXOR      XMM3, XMM3
  PUNPCKLBW XMM4, XMM3
  MOV       RAX,  BiasPointer

@LoopStart:

  MOVD      XMM2, [RDX]
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM4
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  MOVQ      XMM0, XMM4
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [RAX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      [RDX], XMM2

{$ENDIF}

@NextPixel:
  ADD     RDX, 4

  DEC     R8D
  JNZ     @LoopStart

@Done:
  RET

@CopyPixel:
  MOV     [RDX], ECX
  ADD     RDX, 4

  DEC     R8D
  JNZ     @CopyPixel
{$ENDIF}

{$IFDEF CPU32}
  TEST      ECX, ECX
  JZ        @Done

  TEST      EAX, $FF000000
  JZ        @Done

  PUSH      EBX

  MOV       EBX, EAX
  SHR       EBX, 24

  CMP       EBX, $FF
  JZ        @CopyPixel

{$IFDEF AlternativeSSE2}
  MOVD      XMM4, EAX         // XMM4 contains foreground
  PXOR      XMM3, XMM3        // XMM3 is zero
  PUNPCKLBW XMM4, XMM3        // stretch foreground
  PUNPCKLWD XMM4, XMM3        // stretch foreground (even further)
  MOV       EBX,  ScaleBiasPointer
  PSHUFD    XMM5, XMM4, $FF   // XMM2 contains foreground alpha
  PMULLD    XMM5, [EBX]       // scale alpha

@LoopStart:
  MOVD      XMM1, [EDX]       // XMM1 contains background
  PUNPCKLBW XMM1, XMM3        // stretch background
  PUNPCKLWD XMM1, XMM3        // stretch background (even further)
  MOVDQA    XMM0, XMM4        // XMM0 = stretched foreground
  PSUBD     XMM0, XMM1        // XMM0 = XMM0 - XMM1 (= foreground - background)
  PMULLD    XMM0, XMM5        // XMM0 = XMM0 * XMM2 (= alpha - (  "  )        )
  PSLLD     XMM1, 24          // shift left XMM1 (background)
  PADDD     XMM0, [EBX + $10] // add bias
  PADDD     XMM0, XMM1        // add background to weighted difference
  PSRLD     XMM0, 24          // shift right XMM0
  PACKUSWB  XMM0, XMM3        // pack data
  PACKUSWB  XMM0, XMM3        // pack data
  MOVD      [EDX], XMM0       // return result

{$ELSE}

  MOVD      XMM4, EAX
  PXOR      XMM3, XMM3
  PUNPCKLBW XMM4, XMM3
  MOV       EBX,  BiasPointer

@LoopStart:

  MOVD      XMM2, [EDX]
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM4
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  MOVQ      XMM0, XMM4
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [EBX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      [EDX], XMM2

{$ENDIF}

@NextPixel:
  ADD     EDX, 4

  DEC     ECX
  JNZ     @LoopStart

  POP     EBX

@Done:
  RET

@CopyPixel:
  MOV     [EDX], EAX
  ADD     EDX, 4

  DEC     ECX
  JNZ     @CopyPixel

  POP     EBX
{$ENDIF}
end;

procedure BlendLineSSE2(Source, Destination: PPixel32; Count: Integer);
asm
{$IFDEF CPUx86_64}
  TEST      R8D, R8D
  JZ        @Done

  MOV       RDX, RDX

@LoopStart:
  MOV       EAX, [RCX]
  TEST      EAX, $FF000000
  JZ        @NextPixel
  CMP       EAX, $FF000000
  JNC       @CopyPixel

  MOVD      XMM0, EAX
  PXOR      XMM3, XMM3
  MOVD      XMM2, [RDX]
  PUNPCKLBW XMM0, XMM3
  MOV       RAX,  BiasPointer
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM0
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [RAX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      EAX,  XMM2

@CopyPixel:
  MOV       [RDX], EAX

@NextPixel:
  ADD       RCX, 4
  ADD       RDX, 4

  DEC       R8D
  JNZ       @LoopStart

@Done:
  RET
{$ENDIF}

{$IFDEF CPU32}
  TEST      ECX, ECX
  JZ        @Done

  PUSH      EDI

  MOV       EDI, EAX

@LoopStart:
  MOV       EAX, [EDI]
  TEST      EAX, $FF000000
  JZ        @NextPixel
  CMP       EAX, $FF000000
  JNC       @CopyPixel

  MOVD      XMM0, EAX
  PXOR      XMM3, XMM3
  MOVD      XMM2, [EDX]
  PUNPCKLBW XMM0, XMM3
  MOV       EAX,  BiasPointer
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM0
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [EAX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      EAX,  XMM2

@CopyPixel:
  MOV       [EDX], EAX

@NextPixel:
  ADD       EDI, 4
  ADD       EDX, 4

  DEC       ECX
  JNZ       @LoopStart

  POP       EDI

@Done:
  RET
{$ENDIF}
end;

function CombinePixelSSE2(ForeGround, Background: TPixel32; Weight: TPixel32): TPixel32;
asm
{$IFDEF CPUx86_64}
  MOVD      XMM1, ECX
  PXOR      XMM0, XMM0
  SHL       R8, 4

  MOVD      XMM2, EDX
  PUNPCKLBW XMM1, XMM0
  PUNPCKLBW XMM2, XMM0

  ADD       R8, AlphaPointer

  PSUBW     XMM1, XMM2
  PMULLW    XMM1, [R8]
  PSLLW     XMM2, 8

  MOV       RAX, BiasPointer

  PADDW     XMM2, [RAX]
  PADDW     XMM1, XMM2
  PSRLW     XMM1, 8
  PACKUSWB  XMM1, XMM0
  MOVD      EAX, XMM1
{$ENDIF}

{$IFDEF CPU32}
  MOVD      XMM1, EAX
  PXOR      XMM0, XMM0
  SHL       ECX, 4

  MOVD      XMM2, EDX
  PUNPCKLBW XMM1, XMM0
  PUNPCKLBW XMM2, XMM0

  ADD       ECX, AlphaPointer

  PSUBW     XMM1, XMM2
  PMULLW    XMM1, [ECX]
  PSLLW     XMM2, 8

  MOV       ECX, BiasPointer

  PADDW     XMM2, [ECX]
  PADDW     XMM1, XMM2
  PSRLW     XMM1, 8
  PACKUSWB  XMM1, XMM0
  MOVD      EAX, XMM1
{$ENDIF}
end;

procedure CombinePixelInplaceSSE2(F: TPixel32; var B: TPixel32; W: TPixel32);
asm
{$IFDEF CPUx86_64}
  TEST      R8, R8
  JZ        @Done

  CMP       R8D, $FF
  JZ        @Copy

  MOVD      XMM1, ECX
  PXOR      XMM0, XMM0

  SHL       R8, 4

  MOVD      XMM2, [RDX]
  PUNPCKLBW XMM1, XMM0
  PUNPCKLBW XMM2, XMM0

  ADD       R8, AlphaPointer

  PSUBW     XMM1, XMM2
  PMULLW    XMM1, [R8]
  PSLLW     XMM2, 8

  MOV       RAX, BiasPointer

  PADDW     XMM2, [RAX]
  PADDW     XMM1, XMM2
  PSRLW     XMM1, 8
  PACKUSWB  XMM1, XMM0
  MOVD      [RDX], XMM1

@Done:
  RET

@Copy:
  MOV       [RDX], ECX
{$ENDIF}

{$IFDEF CPU32}
  JCXZ      @Done

  CMP       ECX, $FF
  JZ        @Copy

  MOVD      XMM1, EAX
  PXOR      XMM0, XMM0

  SHL       ECX, 4

  MOVD      XMM2, [EDX]
  PUNPCKLBW XMM1, XMM0
  PUNPCKLBW XMM2, XMM0

  ADD       ECX, AlphaPointer

  PSUBW     XMM1, XMM2
  PMULLW    XMM1, [ECX]
  PSLLW     XMM2, 8

  MOV       ECX, BiasPointer

  PADDW     XMM2, [ECX]
  PADDW     XMM1, XMM2
  PSRLW     XMM1, 8
  PACKUSWB  XMM1, XMM0
  MOVD      [EDX], XMM1

@Done:
  RET

@Copy:
  MOV       [EDX], EAX
{$ENDIF}
end;

{$IFNDEF CPUx86_64}
procedure CombineLineSSE2(Source, Destination: PPixel32; Count: Integer;
  Weight: Cardinal);
asm
  TEST      ECX, ECX
  JZ        @Done

  PUSH      EBX
  MOV       EBX, Weight

  TEST      EBX, EBX
  JZ        @LoopEnd

  CMP       EBX, $FF
  JZ        @DoneMove

  SHL       EBX, 4
  ADD       EBX, AlphaPointer
  MOVQ      XMM3, [EBX]
  MOV       EBX, BiasPointer
  MOVQ      XMM4, [EBX]

@LoopStart:
  MOVD      XMM1, [EAX]
  PXOR      XMM0, XMM0
  MOVD      XMM2, [EDX]
  PUNPCKLBW XMM1, XMM0
  PUNPCKLBW XMM2, XMM0

  PSUBW     XMM1, XMM2
  PMULLW    XMM1, XMM3
  PSLLW     XMM2, 8

  PADDW     XMM2, XMM4
  PADDW     XMM1, XMM2
  PSRLW     XMM1, 8
  PACKUSWB  XMM1, XMM0
  MOVD      [EDX], XMM1

  ADD       EAX, 4
  ADD       EDX, 4

  DEC       ECX
  JNZ       @LoopStart
@LoopEnd:
  POP       EBX
  POP       EBP
@Done:
  RET       $0004

@DoneMove:
  SHL       ECX, 2
  CALL      Move
  POP       EBX
end;

function MergePixelSSE2(Foreground, Background: TPixel32): TPixel32;
asm
  TEST      EAX, $FF000000  // foreground completely transparent =>
  JZ        @CopyPixel      // result = background
  CMP       EAX, $FF000000  // foreground completely opaque =>
  JNC       @Done           // result = foreground
  TEST      EDX, $FF000000  // background completely transparent =>
  JZ        @Done           // result = foreground

  PXOR      XMM7, XMM7      // XMM7  <-  00 00 00 00 00 00 00 00
  MOVD      XMM0, EAX       // XMM0  <-  00 00 00 00 Fa Fr Fg Fb
  SHR       EAX, 24         //  EAX  <-  00 00 00 Fa
  ROR       EDX, 24
  MOVZX     ECX, DL         //  ECX  <-  00 00 00 Ba
  PUNPCKLBW XMM0, XMM7      // XMM0  <-  00 Fa 00 Fr 00 Fg 00 Fb
  SUB       EAX, $FF        //  EAX  <-  (Fa - 1)
  XOR       ECX, $FF        //  ECX  <-  (1 - Ba)
  IMUL      ECX, EAX        //  ECX  <-  (Fa - 1) * (1 - Ba)  =  Ra - 1
  IMUL      ECX, $8081      //  ECX  <-  Xa 00 00 00
  ADD       ECX, $8081*$FF*$FF
  SHR       ECX, 15         //  ECX  <-  Ra
  MOV       DL, CH          //  EDX  <-  Br Bg Bb Ra
  ROR       EDX, 8          //  EDX  <-  Ra Br Bg Bb
  MOVD      XMM1, EDX       // XMM1  <-  Ra Br Bg Bb
  PUNPCKLBW XMM1, XMM7      // XMM1  <-  00 Ra 00 Br 00 Bg 00 Bb
  SHL       EAX, 20         //  EAX  <-  Fa 00 00
  PSUBW     XMM0, XMM1      // XMM0  <-  ** Da ** Dr ** Dg ** Db
  ADD       EAX, $0FF01000
  PSLLW     XMM0, 4
  XOR       EDX, EDX        //  EDX  <-  00
  DIV       EAX, ECX        //  EAX  <-  Fa / Ra  =  Wa
  MOVD      XMM4, EAX       // XMM3  <-  Wa
  PSHUFLW   XMM4, XMM4, $C0 // XMM3  <-  00 00 ** Wa ** Wa ** Wa
  PMULHW    XMM0, XMM4      // XMM0  <-  00 00 ** Pr ** Pg ** Pb
  PADDW     XMM0, XMM1      // XMM0  <-  00 Ra 00 Rr 00 Rg 00 Rb
  PACKUSWB  XMM0, XMM7      // XMM0  <-  Ra Rr Rg Rb
  MOVD      EAX, XMM0

  RET

@CopyPixel:
  MOV       EAX, EDX

@Done:
end;
{$ENDIF}


{$ENDIF}


{ Global Functions }

procedure CreateTables;
{$IFNDEF PUREPASCAL}
var
  I : Integer;
  L : Longword;
  P : PLongword;
{$ENDIF}
begin
 {$IFNDEF PUREPASCAL}
 GetAlignedMemory(AlphaPointer, 256 * 8 * SizeOf(Cardinal));

 P := AlphaPointer;
 for I := 0 to 255 do
  begin
   L := I + I shl 16;
   P^ := L;
   Inc(P);
   P^ := L;
   Inc(P);
   P^ := L;
   Inc(P);
   P^ := L;
   Inc(P);
  end;
 P := AlphaPointer;
 Inc(P, $FF * 4);
 BiasPointer := P;
 Assert(PCardinal(BiasPointer)^ = $00FF00FF);

 {$IFDEF AlternativeSSE2}
 GetAlignedMemory(ScaleBiasPointer, 8 * SizeOf(Cardinal));
 P := ScaleBiasPointer;
 for I := 0 to 3 do
  begin
   P^ := $10101;
   Inc(P);
  end;
 for I := 0 to 3 do
  begin
   P^ := $800000;
   Inc(P);
  end;
 {$ENDIF}
 {$ENDIF}
end;

procedure FreeTables;
begin
 {$IFNDEF PUREPASCAL}
 BiasPointer := nil;
 FreeAlignedMemory(AlphaPointer);
 {$IFDEF AlternativeSSE2}
 FreeAlignedMemory(ScaleBiasPointer);
 {$ENDIF}
 {$ENDIF}
end;

procedure BindFunctions;
begin
 // create function binding list for 32-bit float conversions
 GBindingBlend := TFunctionBindingList.Create;

 // create function binding for EMMS procedure
 GBindingEMMS := TFunctionBinding.Create(@@EMMS, @EMMSNative);
 GBindingBlend.AddBinding(GBindingEMMS);
 with GBindingEMMS do
  begin
   Add(@EMMSNative);
   {$IFNDEF PUREPASCAL}
   Add(@EMMSMMX, [pfMMX]);
   Add(@EMMSNative, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for blend register
 GBindingBlendPixel := TFunctionBinding.Create(
   @@BlendPixel, @BlendPixelNative);
 GBindingBlend.AddBinding(GBindingBlendPixel);
 with GBindingBlendPixel do
  begin
   Add(@BlendPixelNative);
   {$IFNDEF PUREPASCAL}
//   Add(@BlendPixelMMX, [pfMMX]);
   Add(@BlendPixelSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for blend memory
 GBindingBlendPixelInplace := TFunctionBinding.Create(
   @@BlendPixelInplace, @BlendPixelInplaceNative);
 GBindingBlend.AddBinding(GBindingBlendPixelInplace);
 with GBindingBlendPixelInplace do
  begin
   Add(@BlendPixelInplaceNative);
   {$IFNDEF PUREPASCAL}
   Add(@BlendPixelInplaceMMX, [pfMMX]);
   Add(@BlendPixelInplaceSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for blend line
 GBindingBlendPixelLine := TFunctionBinding.Create(
   @@BlendPixelLine, @BlendPixelLineNative);
 GBindingBlend.AddBinding(GBindingBlendPixelLine);
 with GBindingBlendPixelLine do
  begin
   Add(@BlendPixelLineNative);
   {$IFNDEF PUREPASCAL}
//   Add(@BlendPixelLineMMX, [pfMMX]);
   Add(@BlendPixelLineSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for blend line
 GBindingBlendLine := TFunctionBinding.Create(
   @@BlendLine, @BlendLineNative);
 GBindingBlend.AddBinding(GBindingBlendLine);
 with GBindingBlendLine do
  begin
   Add(@BlendLineNative);
   {$IFNDEF PUREPASCAL}
   Add(@BlendLineMMX, [pfMMX]);
   Add(@BlendLineSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine register
 GBindingCombinePixel := TFunctionBinding.Create(
   @@CombinePixel, @CombinePixelNative);
 GBindingBlend.AddBinding(GBindingCombinePixel);
 with GBindingCombinePixel do
  begin
   Add(@CombinePixelNative);
   {$IFNDEF PUREPASCAL}
   Add(@CombinePixelMMX, [pfMMX]);
   Add(@CombinePixelSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine memory
 GBindingCombinePixelInplace := TFunctionBinding.Create(
   @@CombinePixelInplace, @CombinePixelInplaceNative);
 GBindingBlend.AddBinding(GBindingCombinePixelInplace);
 with GBindingCombinePixelInplace do
  begin
   Add(@CombinePixelInplaceNative);
   {$IFNDEF PUREPASCAL}
   Add(@CombinePixelInplaceMMX, [pfMMX]);
   Add(@CombinePixelInplaceSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine memory
 GBindingCombinePixelLine := TFunctionBinding.Create(
   @@CombinePixelLine, @CombinePixelLineNative);
 GBindingBlend.AddBinding(GBindingCombinePixelLine);
 with GBindingCombinePixelLine do
  begin
   Add(@CombinePixelLineNative);
   {$IFNDEF PUREPASCAL}
//   Add(@CombinePixelLineMMX, [pfMMX]);
//   Add(@CombinePixelLineSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine line
 GBindingCombineLine := TFunctionBinding.Create(
   @@CombineLine, @CombineLineNative);
 GBindingBlend.AddBinding(GBindingCombineLine);
 with GBindingCombineLine do
  begin
   Add(@CombineLineNative);
   {$IFNDEF PUREPASCAL}
   Add(@CombineLineMMX, [pfMMX]);
   {$IFNDEF CPUx86_64}
   Add(@CombineLineSSE2, [pfSSE2]);
   {$ENDIF}
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine register
 GBindingMergePixel := TFunctionBinding.Create(
   @@MergePixel, @MergePixelNative);
 GBindingBlend.AddBinding(GBindingMergePixel);
 with GBindingMergePixel do
  begin
   Add(@MergePixelNative);
   {$IFNDEF PUREPASCAL}
//   Add(@MergePixelMMX, [pfMMX]);
//   Add(@MergePixelSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for Merge memory
 GBindingMergePixelInplace := TFunctionBinding.Create(
   @@MergePixelInplace, @MergePixelInplaceNative);
 GBindingBlend.AddBinding(GBindingMergePixelInplace);
 with GBindingMergePixelInplace do
  begin
   Add(@MergePixelInplaceNative);
   {$IFNDEF PUREPASCAL}
//   Add(@MergePixelInplaceMMX, [pfMMX]);
//   Add(@MergePixelInplaceSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for Merge line
 GBindingMergeLine := TFunctionBinding.Create(
   @@MergeLine, @MergeLineNative);
 GBindingBlend.AddBinding(GBindingMergeLine);
 with GBindingMergeLine do
  begin
   Add(@MergeLineNative);
   {$IFNDEF PUREPASCAL}
//   Add(@MergeLineMMX, [pfMMX]);
//   Add(@MergeLineSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // processor specific rebind
 GBindingBlend.RebindProcessorSpecific;
end;

procedure UnbindFunctions;
begin
 GBindingBlend.Free;
 GBindingBlend := nil;
end;

initialization
  CreateTables;
  BindFunctions;

finalization
  FreeTables;
  UnbindFunctions;

end.
