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

unit DAV_ZStream;

interface

{$I DAV_Compiler.inc}

uses
  Classes, ZLib;

type
{$IFNDEF DELPHI14_UP}
  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);
{$ENDIF}

  TCustomZStream = class(TStream)
  private
    FStream: TStream;
    FStreamPos: Int64;
    FOnProgress: TNotifyEvent;
    FZStream: TZStreamRec;
    FBuffer: array [Word] of AnsiChar;
  protected
    constructor Create(Stream: TStream);
    procedure DoProgress; dynamic;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(Destination: TStream;
      CompressionLevel: TZCompressionLevel = zcDefault); overload;
    constructor Create(CompressionLevel: TCompressionLevel;
      Destination: TStream); overload;
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;

    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;
    property OnProgress;
  end;

implementation

uses
  SysUtils;

resourcestring
  RCStrInvalidStreamOperation = 'Invalid stream operation';

const
  CZErrorMessages: array [0 .. 9] of PAnsiChar = ('need dictionary',
    'stream end', '', 'file error', 'stream error', 'data error',
    'insufficient memory', 'buffer error', 'incompatible version', '');

function ZResultCheck(Code: Integer): Integer;
begin
  Result := Code;

  if Code < 0 then
    raise Exception.Create(string(CZErrorMessages[2 - Code]));
end;

{ TCustomZStream }

constructor TCustomZStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FStreamPos := Stream.Position;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

{ TZCompressionStream }

constructor TZCompressionStream.Create(Destination: TStream;
  CompressionLevel: TZCompressionLevel);
const
  ZLevels: array [TZCompressionLevel] of ShortInt = (Z_NO_COMPRESSION,
    Z_BEST_SPEED, Z_DEFAULT_COMPRESSION, Z_BEST_COMPRESSION);
begin
  inherited Create(Destination);

  FZStream.next_out := FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZResultCheck(DeflateInit_(FZStream, ZLevels[CompressionLevel], ZLIB_VERSION,
    SizeOf(TZStreamRec)));
end;

constructor TZCompressionStream.Create(CompressionLevel: TCompressionLevel;
  Destination: TStream);
begin
  Create(Destination, TZCompressionLevel(Byte(CompressionLevel)));
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := nil;
  FZStream.avail_in := 0;

  try
    if FStream.Position <> FStreamPos then
      FStream.Position := FStreamPos;

    while ZResultCheck(deflate(FZStream, Z_FINISH)) <> Z_STREAM_END do
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    DeflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var Buffer; Count: Integer): Integer;
begin
  raise Exception.Create(RCStrInvalidStreamOperation);
end;

function TZCompressionStream.Write(const Buffer; Count: Integer): Integer;
begin
  FZStream.next_in := @Buffer;
  FZStream.avail_in := Count;

  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  while FZStream.avail_in > 0 do
  begin
    ZResultCheck(deflate(FZStream, Z_NO_FLUSH));

    if FZStream.avail_out = 0 then
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer));

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);

      FStreamPos := FStream.Position;

      DoProgress;
    end;
  end;

  Result := Count;
end;

function TZCompressionStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then
    Result := FZStream.total_in
  else
    raise Exception.Create(RCStrInvalidStreamOperation);
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then
    Result := 0
  else
    Result := (1 - (FZStream.total_out / FZStream.total_in)) * 100;
end;

constructor TZDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FZStream.next_in := FBuffer;
  FZStream.avail_in := 0;
  ZResultCheck(InflateInit_(FZStream, ZLIB_VERSION, SizeOf(TZStreamRec)));
end;

destructor TZDecompressionStream.Destroy;
begin
  InflateEnd(FZStream);
  inherited Destroy;
end;

function TZDecompressionStream.Read(var Buffer; Count: Integer): Integer;
var
  ZResult: Integer;
begin
  FZStream.next_out := @Buffer;
  FZStream.avail_out := Count;

  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  ZResult := Z_OK;

  while (FZStream.avail_out > 0) and (ZResult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := FStream.Read(FBuffer, SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        Result := Count - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := FBuffer;
      FStreamPos := FStream.Position;

      DoProgress;
    end;

    ZResult := ZResultCheck(inflate(FZStream, Z_NO_FLUSH));
  end;

  if (ZResult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    FStream.Position := FStream.Position - FZStream.avail_in;
    FStreamPos := FStream.Position;

    FZStream.avail_in := 0;
  end;

  Result := Count - FZStream.avail_out;
end;

function TZDecompressionStream.Write(const Buffer; Count: Integer): Integer;
begin
  raise Exception.Create(RCStrInvalidStreamOperation);
end;

function TZDecompressionStream.Seek(Offset: Integer; Origin: Word): Integer;
var
  TempBuffer: array [0 .. 8191] of Byte;
  Index: Integer;
begin
  if (Offset = 0) and (Origin = soFromBeginning) then
  begin
    ZResultCheck(inflateReset(FZStream));

    FZStream.next_in := FBuffer;
    FZStream.avail_in := 0;

    FStream.Position := 0;
    FStreamPos := 0;
  end
  else if ((Offset >= 0) and (Origin = soFromCurrent)) or
    (((Offset - FZStream.total_out) > 0) and (Origin = soFromBeginning)) then
  begin
    if Origin = soFromBeginning then
      Dec(Offset, FZStream.total_out);

    if Offset > 0 then
    begin
      for Index := 1 to Offset div SizeOf(TempBuffer) do
        ReadBuffer(TempBuffer, SizeOf(TempBuffer));
      ReadBuffer(TempBuffer, Offset mod SizeOf(TempBuffer));
    end;
  end
  else if (Offset = 0) and (Origin = soFromEnd) then
    while Read(TempBuffer, SizeOf(TempBuffer)) > 0 do
    else
      raise Exception.Create(RCStrInvalidStreamOperation);

  Result := FZStream.total_out;
end;

end.
