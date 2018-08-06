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

unit DAV_StereoBuffer;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types;

type
  TStereoBuffer = class
  private
    FOutput: array [0 .. 1] of PDAVSingleFixedArray;
    FBufferPos: array [0 .. 1] of Integer;
    FBufferSize: Integer;
    procedure SetBufferSize(const Value: Integer);
    procedure BufferSizeChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(Channel: Cardinal; Value: Single);
    procedure Reset;
    procedure Clear;

    property BufferSize: Integer read FBufferSize write SetBufferSize;
    property OutputLeft: PDAVSingleFixedArray read FOutput[0];
    property OutputRight: PDAVSingleFixedArray read FOutput[1];
  end;

implementation

const
  COutputBufferSize = 1152; // max. 2 * 1152 samples per frame

constructor TStereoBuffer.Create;
begin
  inherited;
  FBufferSize := COutputBufferSize;
  GetMem(FOutput[0], FBufferSize * SizeOf(Single));
  GetMem(FOutput[1], FBufferSize * SizeOf(Single));
  Reset;
end;

destructor TStereoBuffer.Destroy;
begin
  Dispose(FOutput[0]);
  Dispose(FOutput[1]);
  inherited;
end;

procedure TStereoBuffer.Append(Channel: Cardinal; Value: Single);
begin
  FOutput[Channel, FBufferPos[Channel]] := Value;
  FBufferPos[Channel] := FBufferPos[Channel] + 1;
  Assert(FBufferPos[Channel] <= FBufferSize);
end;

procedure TStereoBuffer.Clear;
begin
  FillChar(FOutput[0]^, FBufferSize * SizeOf(Single), 0);
  FillChar(FOutput[1]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TStereoBuffer.Reset;
begin
  FBufferPos[0] := 0;
  FBufferPos[1] := 0;
end;

procedure TStereoBuffer.SetBufferSize(const Value: Integer);
begin
  if FBufferSize <> Value then
  begin
    FBufferSize := Value;
    BufferSizeChanged;
  end;
end;

procedure TStereoBuffer.BufferSizeChanged;
begin
  ReallocMem(FOutput[0], FBufferSize * SizeOf(Single));
  ReallocMem(FOutput[1], FBufferSize * SizeOf(Single));
end;

end.
