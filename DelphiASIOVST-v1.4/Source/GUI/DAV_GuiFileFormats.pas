unit DAV_GuiFileFormats;

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
  Classes, SysUtils;

type
  IGuiFileFormats = interface
    ['{C434E656-14A8-4EB7-AE74-5314990C44AD}']
(*
    function CanLoad(const FileName: TFileName): Boolean; overload;
    function CanLoad(Stream: TStream): Boolean; overload;
*)
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);

    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

  TGuiCustomFileFormat = class(TInterfacedPersistent, IGuiFileFormats,
    IStreamPersist)
  protected
    function GetWidth: Integer; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    procedure SetWidth(const Value: Integer); virtual; abstract;
    procedure SetHeight(const Value: Integer); virtual; abstract;

    class function CanLoad(const FileName: TFileName): Boolean; overload; virtual; abstract;
    class function CanLoad(Stream: TStream): Boolean; overload; virtual; abstract;
    class function CanHandleExtension(const FileName: TFileName): Boolean; virtual; abstract;
  public
    constructor Create; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;

    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromFile(Filename: TFilename); virtual;
    procedure SaveToFile(Filename: TFilename); virtual;
  end;
  TGuiCustomFileFormatClass = class of TGuiCustomFileFormat;

procedure RegisterGraphicFileFormat(FileFormatClass: TGuiCustomFileFormatClass);
procedure RegisterGraphicFileFormats(FileFormatClasses: array of TGuiCustomFileFormatClass);
function FindGraphicFileFormatByExtension(Extension: TFileName): TGuiCustomFileFormatClass;
function FindGraphicFileFormatByFileName(FileName: TFileName): TGuiCustomFileFormatClass;
function FindGraphicFileFormatByStream(Stream: TStream): TGuiCustomFileFormatClass;

implementation

var
  GGraphicFileFormatClasses : array of TGuiCustomFileFormatClass;

function IsGraphicFileFormatRegistered(FileFormatClass: TGuiCustomFileFormatClass): Boolean;
var
  FileFormatClassIndex : Integer;
begin
 Result := False;
 for FileFormatClassIndex := 0 to Length(GGraphicFileFormatClasses) - 1 do
  if GGraphicFileFormatClasses[FileFormatClassIndex] = FileFormatClass then
   begin
    Result := True;
    Exit;
   end;
end;

procedure RegisterGraphicFileFormat(FileFormatClass: TGuiCustomFileFormatClass);
begin
 Assert(IsGraphicFileFormatRegistered(FileFormatClass) = False);
 SetLength(GGraphicFileFormatClasses, Length(GGraphicFileFormatClasses) + 1);
 GGraphicFileFormatClasses[Length(GGraphicFileFormatClasses) - 1] := FileFormatClass;
end;

procedure RegisterGraphicFileFormats(FileFormatClasses: array of TGuiCustomFileFormatClass);
var
  FileFormatClassIndex : Integer;
begin
 for FileFormatClassIndex := 0 to Length(FileFormatClasses) - 1
  do RegisterGraphicFileFormat(FileFormatClasses[FileFormatClassIndex]);
end;

function FindGraphicFileFormatByExtension(Extension: TFileName): TGuiCustomFileFormatClass;
var
  FileFormatClassIndex : Integer;
begin
 Result := nil;
 for FileFormatClassIndex := 0 to Length(GGraphicFileFormatClasses) - 1 do
  with GGraphicFileFormatClasses[FileFormatClassIndex] do
   if CanHandleExtension(Extension) then
    begin
     Result := GGraphicFileFormatClasses[FileFormatClassIndex];
     Exit;
    end;
end;

function FindGraphicFileFormatByStream(Stream: TStream): TGuiCustomFileFormatClass;
var
  FileFormatClassIndex : Integer;
begin
 Result := nil;
 for FileFormatClassIndex := 0 to Length(GGraphicFileFormatClasses) - 1 do
  with GGraphicFileFormatClasses[FileFormatClassIndex] do
   if CanLoad(Stream) then
    begin
     Result := GGraphicFileFormatClasses[FileFormatClassIndex];
     Exit;
    end;
end;

function FindGraphicFileFormatByFileName(FileName: TFileName): TGuiCustomFileFormatClass;
var
  FileFormatClassIndex : Integer;
begin
 Result := nil;
 for FileFormatClassIndex := 0 to Length(GGraphicFileFormatClasses) - 1 do
  with GGraphicFileFormatClasses[FileFormatClassIndex] do
   if CanLoad(FileName) then
    begin
     Result := GGraphicFileFormatClasses[FileFormatClassIndex];
     Exit;
    end;
end;


{ TGuiCustomFileFormat }

procedure TGuiCustomFileFormat.AssignTo(Dest: TPersistent);
begin
 inherited AssignTo(Dest);
end;

procedure TGuiCustomFileFormat.LoadFromFile(Filename: TFilename);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 try
  LoadFromStream(FileStream);
 finally
  FreeAndNil(FileStream);
 end;
end;

procedure TGuiCustomFileFormat.SaveToFile(Filename: TFilename);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmCreate);
 try
  SaveToStream(FileStream);
 finally
  FreeAndNil(FileStream);
 end;
end;

end.

