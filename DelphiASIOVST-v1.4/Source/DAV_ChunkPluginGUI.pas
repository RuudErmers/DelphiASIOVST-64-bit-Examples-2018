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

unit DAV_ChunkPluginGUI;

interface

{$I DAV_Compiler.inc}

uses
  Classes, Graphics, DAV_Types, DAV_ChunkClasses, DAV_GuiBaseControl;

type
  TDAVPluginGuiChunkRecord = packed record
    BackgroundColor: TColor;
    KnobsPerRow: Byte;
    FontAntiAliasing: Byte;
    FontSize: Byte;
    FontColor: TColor;
  end;

  TDAVPluginGuiChunk = class(TFixedDefinedChunk)
  private
    function GetKnobsPerRow: TGuiAntiAlias;
    procedure SetFontAntiAliasing(const Value: TGuiAntiAlias);
  protected
    FPluginGuiChunkRecord: TDAVPluginGuiChunkRecord;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName: TChunkName; override;
  published
    property BackgroundColor: TColor read FPluginGuiChunkRecord.BackgroundColor
      write FPluginGuiChunkRecord.BackgroundColor;
    property KnobsPerRow: Byte read FPluginGuiChunkRecord.KnobsPerRow
      write FPluginGuiChunkRecord.KnobsPerRow;
    property FontAntiAliasing: TGuiAntiAlias read GetKnobsPerRow
      write SetFontAntiAliasing;
    property FontSize: Byte read FPluginGuiChunkRecord.FontSize
      write FPluginGuiChunkRecord.FontSize;
    property FontColor: TColor read FPluginGuiChunkRecord.FontColor
      write FPluginGuiChunkRecord.FontColor;
  end;

implementation

{ TDAVPluginGuiChunk }

constructor TDAVPluginGuiChunk.Create;
begin
  inherited;
  StartAddress := @FPluginGuiChunkRecord;
  with FPluginGuiChunkRecord do
  begin
    BackgroundColor := $007F7F7F;
    KnobsPerRow := 6;
    FontAntiAliasing := 0;
    FontSize := 8;
  end;
end;

procedure TDAVPluginGuiChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TDAVPluginGuiChunk then
    TDAVPluginGuiChunk(Dest).FPluginGuiChunkRecord := FPluginGuiChunkRecord;
end;

class function TDAVPluginGuiChunk.GetClassChunkName: TChunkName;
begin
  result := 'PGUI';
end;

class function TDAVPluginGuiChunk.GetClassChunkSize: Integer;
begin
  result := SizeOf(TDAVPluginGuiChunkRecord);
end;

function TDAVPluginGuiChunk.GetKnobsPerRow: TGuiAntiAlias;
begin
  result := TGuiAntiAlias(FPluginGuiChunkRecord.FontAntiAliasing);
end;

procedure TDAVPluginGuiChunk.SetFontAntiAliasing(const Value: TGuiAntiAlias);
begin
  FPluginGuiChunkRecord.FontAntiAliasing := Byte(Value);
end;

end.
