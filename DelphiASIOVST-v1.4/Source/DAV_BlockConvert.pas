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

unit DAV_BlockConvert;

//  Assembler code optimizations are based on Agner Fog's excellent
//  documentations. In particular the following document was used
//  http://www.agner.org/optimize/optimizing_assembly.pdf

interface

{$I DAV_Compiler.inc}
{ -$DEFINE PUREPASCAL }

uses
  DAV_Bindings;

{ Function Pointers }

var
  BlockConvertFloat64ToFloat32: procedure(Destination: PSingle; Source: PDouble;
    Count: Integer);
  BlockConvertFloat32ToFloat64: procedure(Destination: PDouble; Source: PSingle;
    Count: Integer);

  { Binding Function Pointers }

var
  BindingBlockConvertFloat32ToFloat64: TFunctionBinding;
  BindingBlockConvertFloat64ToFloat32: TFunctionBinding;

implementation

procedure BlockConvertFloat64ToFloat32Native(Destination: PSingle;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^;
    Inc(Destination);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 4
  LEA     EDX, EDX + ECX * 8
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EDX + ECX * 8].Double
  FSTP    [EAX + ECX * 4].Single
  ADD     ECX, 1
  JS      @Start

@Done:
end;
{$ENDIF}

procedure BlockConvertFloat32ToFloat64Native(Destination: PDouble;
  Source: PSingle; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    Destination^ := Source^;
    Inc(Destination);
    Inc(Source);
  end;
end;
{$ELSE}
asm
  LEA     EAX, EAX + ECX * 8
  LEA     EDX, EDX + ECX * 4
  NEG     ECX
  JNL     @Done

@Start:
  FLD     [EDX + ECX * 4].Single
  FSTP    [EAX + ECX * 8].Double
  ADD     ECX, 1
  JS      @Start

@Done:
end;
{$ENDIF}

procedure BindFunctions;
begin
  // create function binding for 64-bit to 32-bit float conversion
  BindingBlockConvertFloat64ToFloat32 := TFunctionBinding.Create
    (@@BlockConvertFloat64ToFloat32, @BlockConvertFloat64ToFloat32Native);
  with BindingBlockConvertFloat64ToFloat32 do
  begin
    ADD(@BlockConvertFloat64ToFloat32Native);
  end;

  // create function binding for 64-bit to 32-bit float conversion
  BindingBlockConvertFloat32ToFloat64 := TFunctionBinding.Create
    (@@BlockConvertFloat32ToFloat64, @BlockConvertFloat32ToFloat64Native);
  with BindingBlockConvertFloat64ToFloat32 do
  begin
    ADD(@BlockConvertFloat32ToFloat64Native);
  end;
end;

initialization

BindFunctions;

end.
