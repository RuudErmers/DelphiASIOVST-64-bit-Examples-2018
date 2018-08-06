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

unit DAV_MiniFloat;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common;

type
  TMiniFloat = Byte;
  TDAVMiniFloatFixedArray = array [0 .. 0] of TMiniFloat;
  PDAVMiniFloatFixedArray = ^TDAVMiniFloatFixedArray;

function FastMiniFloatToSingle(const Value: TMiniFloat): Single;
function SingleToMiniFloat(const Value: Single): TMiniFloat;
function MiniFloatToSingle(const Value: TMiniFloat): Single;

implementation

function SingleToMiniFloat(const Value: Single): TMiniFloat;
var
  IntValue: Integer;
  SignInt: Integer;
  Exponent: Integer;
  ExpMask: Integer;
  Mantissa: Integer;
begin
  // Get the sign bit. Shift it for later use without further modification
  if Value < 0 then
    SignInt := (1 shl 7)
  else
    SignInt := 0;

  // The magnitude must fit in 15 bits to avoid overflow
  if (Abs(Value) > 1) then
    IntValue := $7FFF
  else
    IntValue := Round($7FFF * Abs(Value));

  // Finding the "exponent"
  // Bits:
  // 1 2 3 4 5 6 7 8 9 A B C D E F G
  // S 7 6 5 4 3 2 1 0 0 0 0 0 0 0 0
  // We want to find where the first 1 after the sign bit is.
  // We take the corresponding value
  // from the second row as the exponent value.
  // (i.e. if first 1 at position 7 -> exponent = 2)
  // The exponent is 0 if the 1 is not found in bits 2 through 8.
  // This means the exponent is 0 even if the "first 1" doesn't exist.
  Exponent := 7;

  // Move to the right and decrement exponent until we hit the 1 or the
  // exponent hits 0
  ExpMask := $4000;
  while ((IntValue and ExpMask) = 0) and (Exponent > 0) do
  begin
    ExpMask := ExpMask shr 1;
    Dec(Exponent);
  end;

  // The last part - the "mantissa"
  // We need to take the four bits after the 1 we just found.
  // To get it, we shift $0F :
  // 1 2 3 4 5 6 7 8 9 A B C D E F G
  // S 0 0 0 0 0 1 . . . . . . . . . (say that exponent is 2)
  // . . . . . . . . . . . . 1 1 1 1
  // We shift it 5 times for an exponent of two, meaning we will shift our
  // four bits (exponent + 3) bits.
  // For convenience, we will actually just shift the number, then AND with $0F.
  //
  // NOTE: If the exponent is 0:
  // 1 2 3 4 5 6 7 8 9 A B C D E F G
  // S 0 0 0 0 0 0 0 Z Y X W V U T S (we know nothing about bit 9)
  // . . . . . . . . . . . . 1 1 1 1
  // We want to get ZYXW, which means a shift of 4 instead of 3

  if (Exponent = 0) then
    Mantissa := IntValue shr 4
  else
    Mantissa := IntValue shr (Exponent + 3);

  // The a-law byte bit arrangement is SEEEMMMM (Sign, Exponent, and Mantissa.)
  // Last is to flip every other bit, and the sign bit ($D5 = 1101 0101)
  Result := ((SignInt or (Exponent shl 4) or (Mantissa and $0F))) xor $D5;
end;

const
  CMiniFloatDecompressTable: array [0 .. 255] of SmallInt = (-5504, -5248,
    -6016, -5760, -4480, -4224, -4992, -4736, -7552, -7296, -8064, -7808, -6528,
    -6272, -7040, -6784, -2752, -2624, -3008, -2880, -2240, -2112, -2496, -2368,
    -3776, -3648, -4032, -3904, -3264, -3136, -3520, -3392, -22016, -20992,
    -24064, -23040, -17920, -16896, -19968, -18944, -30208, -29184, -32256,
    -31232, -26112, -25088, -28160, -27136, -11008, -10496, -12032, -11520,
    -8960, -8448, -9984, -9472, -15104, -14592, -16128, -15616, -13056, -12544,
    -14080, -13568, -344, -328, -376, -360, -280, -264, -312, -296, -472, -456,
    -504, -488, -408, -392, -440, -424, -88, -72, -120, -104, -24, -8, -56, -40,
    -216, -200, -248, -232, -152, -136, -184, -168, -1376, -1312, -1504, -1440,
    -1120, -1056, -1248, -1184, -1888, -1824, -2016, -1952, -1632, -1568, -1760,
    -1696, -688, -656, -752, -720, -560, -528, -624, -592, -944, -912, -1008,
    -976, -816, -784, -880, -848, 5504, 5248, 6016, 5760, 4480, 4224, 4992,
    4736, 7552, 7296, 8064, 7808, 6528, 6272, 7040, 6784, 2752, 2624, 3008,
    2880, 2240, 2112, 2496, 2368, 3776, 3648, 4032, 3904, 3264, 3136, 3520,
    3392, 22016, 20992, 24064, 23040, 17920, 16896, 19968, 18944, 30208, 29184,
    32256, 31232, 26112, 25088, 28160, 27136, 11008, 10496, 12032, 11520, 8960,
    8448, 9984, 9472, 15104, 14592, 16128, 15616, 13056, 12544, 14080, 13568,
    344, 328, 376, 360, 280, 264, 312, 296, 472, 456, 504, 488, 408, 392, 440,
    424, 88, 72, 120, 104, 24, 8, 56, 40, 216, 200, 248, 232, 152, 136, 184,
    168, 1376, 1312, 1504, 1440, 1120, 1056, 1248, 1184, 1888, 1824, 2016, 1952,
    1632, 1568, 1760, 1696, 688, 656, 752, 720, 560, 528, 624, 592, 944, 912,
    1008, 976, 816, 784, 880, 848);

function FastMiniFloatToSingle(const Value: TMiniFloat): Single;
const
  CScale: Single = 3.0518509475997192297128208258309E-5;
begin
  Result := CMiniFloatDecompressTable[Value] * CScale;
end;

function MiniFloatToSingle(const Value: TMiniFloat): Single;
var
  Input: Byte;
  Sign: Integer;
  Exponent: Integer;
  Data: Integer;
const
  CScale: Single = 3.0518509475997192297128208258309E-5;
begin
  // Invert every other bit,
  // and the sign bit ($D5 = 1101 0101)
  Input := Value xor $D5;

  // Pull out the value of the sign bit
  Sign := Input and $80;

  // Pull out and shift over the value of the exponent
  Exponent := (Input and $70) shr 4;

  // Pull out the four bits of data and shift the data four bits to the left
  // Add 8 to put the Result in the middle of the range (like adding a half)
  Data := ((Input and $0F) shl 4) + 8;

  // If the exponent is not 0, then we know the four bits followed a 1,
  // and can thus add this implicit 1 with 0x100.
  if (Exponent <> 0) then
    Data := Data + $100;

  // Shift the bits to where they need to be: left (exponent - 1) places
  // Why (exponent - 1) ?
  // 1 2 3 4 5 6 7 8 9 A B C D E F G
  // . 7 6 5 4 3 2 1 . . . . . . . . <-- starting bit (based on exponent)
  // . . . . . . . Z x x x x 1 0 0 0 <-- our data (Z is 0 only when
  // exponent is 0)
  // We need to move the one under the value of the exponent,
  // which means it must move (exponent - 1) times
  // It also means shifting is unnecessary if exponent is 0 or 1.

  if (Exponent > 1) then
    Data := Data shl (Exponent - 1);

  Result := Data * CScale;
  if Sign <> 0 then
    Result := -Result;
end;

end.
