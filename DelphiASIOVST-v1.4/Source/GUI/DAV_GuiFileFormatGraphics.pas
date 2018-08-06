unit DAV_GuiFileFormatGraphics;
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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, Graphics, DAV_GuiFileFormats, DAV_GuiPixelMap;

type
  TGuiFileFormatGraphics = class(TGuiCustomFileFormat)
  private
    FBitmap : TBitmap;
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetHeight(const Value: Integer); override;

    class function CanLoad(const FileName: TFileName): Boolean; overload; override;
    class function CanLoad(Stream: TStream): Boolean; overload; override;
    class function CanHandleExtension(const FileName: TFileName): Boolean; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

implementation

{ TGuiFileFormatGraphics }

class function TGuiFileFormatGraphics.CanHandleExtension(
  const FileName: TFileName): Boolean;
begin
 Result := Pos('bmp', FileName) >= 0;
end;

class function TGuiFileFormatGraphics.CanLoad(Stream: TStream): Boolean;
var
  OldStreamPosition : Integer;
begin
 with TBitmap.Create do
  try
   try
    OldStreamPosition := Stream.Position;
    try
     LoadFromStream(Stream);
     Result := True;
    except
     Result := False;
    end;
   finally
    Stream.Position := OldStreamPosition;
   end;
  finally
   Free;
  end;
end;

class function TGuiFileFormatGraphics.CanLoad(
  const FileName: TFileName): Boolean;
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   Result := CanLoad(FileStream);
  finally
   Free;
  end;
end;


constructor TGuiFileFormatGraphics.Create;
begin
 inherited;
 FBitmap := TBitmap.Create;
end;

destructor TGuiFileFormatGraphics.Destroy;
begin
 FreeAndNil(FBitmap);
 inherited;
end;

procedure TGuiFileFormatGraphics.Assign(Source: TPersistent);
begin
 if Source is TBitmap
  then FBitmap.Assign(Source)
  else
 if Source is TGuiCustomPixelMap then
  with TGuiCustomPixelMap(Source) do
   begin
    FBitmap.Width := Width;
    FBitmap.Height := Height;
    PaintTo(FBitmap.Canvas);
   end
 else inherited;
end;

procedure TGuiFileFormatGraphics.AssignTo(Dest: TPersistent);
begin
 if Dest is TBitmap then
  with TBitmap(Dest) do
   begin
    Width := FBitmap.Width;
    Height := FBitmap.Height;
    Assign(FBitmap);
   end else
 if Dest is TGuiCustomPixelMap then
  with TGuiCustomPixelMap(Dest) do
   begin
    Width := FBitmap.Width;
    Height := FBitmap.Height;
    Draw(FBitmap);
   end
 else inherited;
end;

function TGuiFileFormatGraphics.GetHeight: Integer;
begin
 Result := FBitmap.Height;
end;

function TGuiFileFormatGraphics.GetWidth: Integer;
begin
 Result := FBitmap.Width;
end;

procedure TGuiFileFormatGraphics.LoadFromStream(Stream: TStream);
begin
 inherited;
 FBitmap.LoadFromStream(Stream);
end;

procedure TGuiFileFormatGraphics.SaveToStream(Stream: TStream);
begin
 inherited;
 FBitmap.SaveToStream(Stream);
end;

procedure TGuiFileFormatGraphics.SetHeight(const Value: Integer);
begin
 FBitmap.Height := Value;
 inherited;
end;

procedure TGuiFileFormatGraphics.SetWidth(const Value: Integer);
begin
 FBitmap.Width := Value;
 inherited;
end;

initialization
  RegisterGraphicFileFormat(TGuiFileFormatGraphics);

end.
