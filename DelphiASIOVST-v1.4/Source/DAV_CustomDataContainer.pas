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

unit DAV_CustomDataContainer;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes;

type
  TCustomDataContainer = class(TPersistent)
  private
    function GetData(Index: Integer): Double;
    procedure SetData(Index: Integer; const Value: Double);
  protected
    FData: PDAVDoubleFixedArray;
    FCount: Integer;
    procedure AllocateMemory; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Data[Index: Integer]: Double read GetData write SetData;
    property DataPointer: PDAVDoubleFixedArray read FData;
    property Count: Integer read FCount;
  end;

implementation

uses
  SysUtils;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

  { TCustomDataContainer }

constructor TCustomDataContainer.Create;
begin
  inherited;
end;

destructor TCustomDataContainer.Destroy;
begin
  Dispose(FData);
  inherited;
end;

procedure TCustomDataContainer.AllocateMemory;
begin
  ReallocMem(FData, FCount * SizeOf(Double));
end;

procedure TCustomDataContainer.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDataContainer then
    with TCustomDataContainer(Dest) do
    begin
      FCount := Self.FCount;
      AllocateMemory;
      Move(Self.FData^, FData^, FCount * SizeOf(Double));
    end
  else
    inherited;
end;

function TCustomDataContainer.GetData(Index: Integer): Double;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FData[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomDataContainer.SetData(Index: Integer; const Value: Double);
begin
  if (Index >= 0) and (Index < FCount) then
    FData[Index] := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

end.
