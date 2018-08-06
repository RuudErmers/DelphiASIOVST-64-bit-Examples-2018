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

unit DAV_BitReserve;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common;

type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array [0 .. 0] of Cardinal;

  TBitReserve = class
  private
    FOffset: Cardinal;
    FTotalBits: Cardinal;
    FBufByteIdx: Cardinal;
    FBuffer: PCardinalArray;
    FBufBitIdx: Cardinal;
    FPutMask: PCardinalArray;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function GetBits(Bits: Cardinal): Cardinal;
    function Get1Bit: Cardinal;
    procedure WriteToBitstream(Value: Cardinal);

    procedure RewindBits(Bits: Cardinal);
    procedure RewindBytes(Bytes: Cardinal);

    property TotalBits: Cardinal read FTotalBits;
  end;

implementation

uses
  SysUtils;

const
  CBufferSize = 4096;

  { TBitReserve }

constructor TBitReserve.Create;
var
  ShiftedOne, Bit: Cardinal;
begin
  inherited Create;

  ShiftedOne := 1;
  FOffset := 0;
  FTotalBits := 0;
  FBufByteIdx := 0;
  GetMem(FBuffer, CBufferSize * SizeOf(Cardinal));
  FBufBitIdx := 8;
  GetMem(FPutMask, 32 * SizeOf(Cardinal));

  FPutMask[0] := 0;
  for Bit := 1 to 31 do
  begin
    FPutMask[Bit] := FPutMask[Bit - 1] + ShiftedOne;
    ShiftedOne := ShiftedOne shl 1;
  end;
end;

destructor TBitReserve.Destroy;
begin
  Dispose(FPutMask);
  Dispose(FBuffer);
  inherited Destroy;
end;

// read 1 bit from the bit stream
function TBitReserve.Get1Bit: Cardinal;
begin
  Inc(FTotalBits);
  if (FBufBitIdx = 0) then
  begin
    FBufBitIdx := 8;
    Inc(FBufByteIdx);
  end;

  // CBufferSize = 4096 = 2^12, so
  // FBufByteIdx mod CBufferSize = FBufByteIdx and $FFF
  Result := FBuffer[FBufByteIdx and $FFF] and FPutMask[FBufBitIdx];
  Dec(FBufBitIdx);
  Result := Result shr FBufBitIdx;
end;

// read Bits bits from the bit stream
function TBitReserve.GetBits(Bits: Cardinal): Cardinal;
var
  Bit: Cardinal;
  k, Temp: Cardinal;
begin
  Result := 0;
  if Bits = 0 then
    Exit;

  Inc(FTotalBits, Bits);
  Bit := Bits;

  while (Bit > 0) do
  begin
    if (FBufBitIdx = 0) then
    begin
      FBufBitIdx := 8;
      Inc(FBufByteIdx);
    end;

    if (Bit < FBufBitIdx) then
      k := Bit
    else
      k := FBufBitIdx;

    // CBufferSize = 4096 = 2^12, so
    // FBufByteIdx mod CBufferSize = FBufByteIdx and $FFF
    Temp := FBuffer[FBufByteIdx and $FFF] and FPutMask[FBufBitIdx];
    Dec(FBufBitIdx, k);
    Temp := Temp shr FBufBitIdx;
    Dec(Bit, k);
    Result := Result or (Temp shl Bit);
  end;
end;

// write 8 bits into the bit stream
procedure TBitReserve.WriteToBitstream(Value: Cardinal);
begin
  FBuffer[FOffset] := Value;
  FOffset := (FOffset + 1) and $FFF;
end;

procedure TBitReserve.RewindBits(Bits: Cardinal);
begin
  Dec(FTotalBits, Bits);
  Inc(FBufBitIdx, Bits);

  while (FBufBitIdx >= 8) do
  begin
    Dec(FBufBitIdx, 8);
    Dec(FBufByteIdx);
  end;
end;

procedure TBitReserve.RewindBytes(Bytes: Cardinal);
begin
  Dec(FTotalBits, (Bytes shl 3));
  Dec(FBufByteIdx, Bytes);
end;

end.
