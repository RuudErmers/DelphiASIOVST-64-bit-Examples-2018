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

unit DAV_CircularByteBuffer;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types;

const
  CMaximumCircularByteBufferOrder = 20;

type
  TByteArray = array [0 .. 0] of Byte;
  PByteArray = ^TByteArray;

  TCircularByteBuffer = class(TObject)
  private
    FSize: Integer;
    FPosition: Integer;
    FData: PByteArray;
    FBitMask: Byte;
    function GetValue(BackwardDistance: Integer): Byte;
  public
    constructor Create(Order: Byte); virtual;
    destructor Destroy; override;
    procedure Push(Value: Byte);

    property Size: Integer read FSize;
    property Value[BackwardDistance: Integer]: Byte read GetValue;
  end;

implementation

uses
  SysUtils;

{ TCircularByteBuffer }

constructor TCircularByteBuffer.Create(Order: Byte);
begin
  if Order >= CMaximumCircularByteBufferOrder then
    raise Exception.CreateFmt('Only orders up to %d are supported!',
      [CMaximumCircularByteBufferOrder]);

  Assert(Order <= 31);
  FSize := 1 shl Order;
  FBitMask := (1 shl (Order + 1)) - 1;
  GetMem(FData, FSize);
  FillChar(FData^, FSize, 0);
end;

destructor TCircularByteBuffer.Destroy;
begin
  Dispose(FData);
  inherited;
end;

function TCircularByteBuffer.GetValue(BackwardDistance: Integer): Byte;
begin
  if BackwardDistance > FSize then
    raise Exception.CreateFmt('Backward distance too large! (%d)',
      [BackwardDistance]);

  Result := FData[(Size + (FPosition - BackwardDistance)) and FBitMask];
end;

procedure TCircularByteBuffer.Push(Value: Byte);
begin
  FData[FPosition] := Value;

  Inc(FPosition);
  FPosition := FPosition and FBitMask;
end;

end.
