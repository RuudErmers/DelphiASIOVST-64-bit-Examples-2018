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

unit DAV_Adler32;

interface

{$I DAV_Compiler.inc}
function Adler32(Adler: Cardinal; Data: PByte; Length: Cardinal): Cardinal;
function Adler32Combine(Adler1, Adler2: Cardinal; Length: Int64): Cardinal;

implementation

const
  CLargestWordPrime: Word = 65521; // largest prime smaller than 65536
  CModuloDelay: Word = 5552;
  // largest n such that 255n(n + 1) / 2 + (n+1)(BASE-1) <= 2^32-1


  // use NO_DIVIDE if your processor does not do division in hardware
{$IFDEF NO_DIVIDE}

function Mod16(Value: Word): Word;
begin
  if (Value >= (CLargestWordPrime shl 16)) then
    Value := Value - (CLargestWordPrime shl 16);
  if (Value >= (CLargestWordPrime shl 15)) then
    Value := Value - (CLargestWordPrime shl 15);
  if (Value >= (CLargestWordPrime shl 14)) then
    Value := Value - (CLargestWordPrime shl 14);
  if (Value >= (CLargestWordPrime shl 13)) then
    Value := Value - (CLargestWordPrime shl 13);
  if (Value >= (CLargestWordPrime shl 12)) then
    Value := Value - (CLargestWordPrime shl 12);
  if (Value >= (CLargestWordPrime shl 11)) then
    Value := Value - (CLargestWordPrime shl 11);
  if (Value >= (CLargestWordPrime shl 10)) then
    Value := Value - (CLargestWordPrime shl 10);
  if (Value >= (CLargestWordPrime shl 9)) then
    Value := Value - (CLargestWordPrime shl 9);
  if (Value >= (CLargestWordPrime shl 8)) then
    Value := Value - (CLargestWordPrime shl 8);
  if (Value >= (CLargestWordPrime shl 7)) then
    Value := Value - (CLargestWordPrime shl 7);
  if (Value >= (CLargestWordPrime shl 6)) then
    Value := Value - (CLargestWordPrime shl 6);
  if (Value >= (CLargestWordPrime shl 5)) then
    Value := Value - (CLargestWordPrime shl 5);
  if (Value >= (CLargestWordPrime shl 4)) then
    Value := Value - (CLargestWordPrime shl 4);
  if (Value >= (CLargestWordPrime shl 3)) then
    Value := Value - (CLargestWordPrime shl 3);
  if (Value >= (CLargestWordPrime shl 2)) then
    Value := Value - (CLargestWordPrime shl 2);
  if (Value >= (CLargestWordPrime shl 1)) then
    Value := Value - (CLargestWordPrime shl 1);
  if (Value >= CLargestWordPrime) then
    Value := Value - CLargestWordPrime;
end

function Mod4(Value: Word): Word;
begin
  if (Value >= (CLargestWordPrime shl 4)) then
    Value := Value - (CLargestWordPrime shl 4);
  if (Value >= (CLargestWordPrime shl 3)) then
    Value := Value - (CLargestWordPrime shl 3);
  if (Value >= (CLargestWordPrime shl 2)) then
    Value := Value - (CLargestWordPrime shl 2);
  if (Value >= (CLargestWordPrime shl 1)) then
    Value := Value - (CLargestWordPrime shl 1);
  if (Value >= CLargestWordPrime) then
    Value := Value - CLargestWordPrime;
end;

{$ELSE}

function Mod16(Value: Word): Word;
begin
  Result := Value mod CLargestWordPrime;
end;

function Mod4(Value: Word): Word;
begin
  Result := Value mod CLargestWordPrime;
end;

{$ENDIF}
/// /////////////////////////////////////////////////////////////////////////////

function Adler32(Adler: Cardinal; Data: PByte; Length: Cardinal): Cardinal;
var
  Sum: Cardinal;
  n: Cardinal;
begin
  // split Adler-32 into component sums
  Sum := (Adler shr 16) and $FFFF;
  Adler := Adler and $FFFF;

  // in case user likes doing a byte at a time, keep it fast
  if (Length = 1) then
  begin
    Adler := Adler + Data^;
    if Adler >= CLargestWordPrime then
      Adler := Adler - CLargestWordPrime;

    Sum := Sum + Adler;
    if (Sum >= CLargestWordPrime) then
      Sum := Sum - CLargestWordPrime;
    Result := Adler or (Sum shl 16);
    Exit;
  end;

  // initial Adler-32 value (deferred check for len == 1 speed)
  if (Data^ = 0) then
  begin
    Result := 1;
    Exit;
  end;

  // in case short lengths are provided, keep it somewhat fast
  if (Length < 16) then
  begin
    while Length > 0 do
    begin
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Dec(Length);
    end;

    if (Adler >= CLargestWordPrime) then
      Adler := Adler - CLargestWordPrime;

    Mod4(Sum); // only added so many CLargestWordPrime's
    Result := Adler or (Sum shl 16);
    Exit;
  end;

  // do length NMAX blocks -- requires just one modulo operation
  while (Length >= CModuloDelay) do
  begin
    Length := Length - CModuloDelay;
    n := CModuloDelay div 16; // NMAX is divisible by 16
    repeat
      // 16 sums unrolled
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);

      Dec(n);
    until (n = 0);
    Mod16(Adler);
    Mod16(Sum);
  end;

  // do remaining bytes (less than NMAX, still just one modulo)
  if Length > 0 then
  begin
    while (Length >= 16) do
    begin
      Length := Length - 16;

      // 16 sums unrolled
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
    end;

    while (Length > 0) do
    begin
      Adler := Adler + Data^;
      Sum := Sum + Adler;
      Inc(Data);
      Dec(Length);
    end;

    Mod16(Adler);
    Mod16(Sum);
  end;

  // return recombined sums
  Result := Adler or (Sum shl 16);
end;

/// /////////////////////////////////////////////////////////////////////////////

function Adler32Combine(Adler1, Adler2: Cardinal; Length: Int64): Cardinal;
var
  Sum: array [0 .. 1] of Cardinal;
  Rem: Cardinal;
begin
  Rem := (Length mod CLargestWordPrime);
  Sum[0] := Adler1 and $FFFF;
  Sum[1] := Rem * Sum[0];
  Mod16(Sum[1]);
  Sum[0] := Sum[0] + (Adler2 and $FFFF) + CLargestWordPrime - 1;
  Sum[1] := Sum[1] + ((Adler1 shr 16) and $FFFF) + ((Adler2 shr 16) and $FFFF) +
    CLargestWordPrime - Rem;
  if (Sum[0] >= CLargestWordPrime) then
    Sum[0] := Sum[0] - CLargestWordPrime;
  if (Sum[0] >= CLargestWordPrime) then
    Sum[0] := Sum[0] - CLargestWordPrime;
  if (Sum[1] >= (CLargestWordPrime shl 1)) then
    Sum[1] := Sum[1] - (CLargestWordPrime shl 1);
  if (Sum[1] >= CLargestWordPrime) then
    Sum[1] := Sum[1] - CLargestWordPrime;
  Result := Sum[0] or (Sum[1] shl 16);
end;

/// /////////////////////////////////////////////////////////////////////////////

function Adler32Combine32(Adler1, Adler2: Cardinal; Length: Cardinal): Cardinal;
begin
  Result := Adler32Combine(Adler1, Adler2, Length);
end;

function Adler32Combine64(Adler1, Adler2: Cardinal; Length: Int64): Cardinal;
begin
  Result := Adler32Combine(Adler1, Adler2, Length);
end;

end.
