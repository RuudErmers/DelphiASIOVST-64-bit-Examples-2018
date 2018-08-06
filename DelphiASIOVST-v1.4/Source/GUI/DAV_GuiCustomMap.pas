unit DAV_GuiCustomMap;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, LResources,
  {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_MemoryUtils, DAV_GuiBlend;

type
  TGuiCustomMap = class(TInterfacedPersistent, IStreamPersist)
  private
    function GetClientRect: TRect;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    FDataSize : Integer;
    FWidth    : Integer;
    FHeight   : Integer;
    FOnChange : TNotifyEvent;
    FOnResize : TNotifyEvent;

    function Empty: Boolean; virtual;
    procedure Changed; virtual;
    procedure Resized; virtual;
    procedure SizeChangedAtOnce; virtual;

    procedure ReadData(Stream: TStream); virtual; abstract;
    procedure WriteData(Stream: TStream); virtual; abstract;

    procedure HeightChanged(UpdateBitmap: Boolean = True); virtual; abstract;
    procedure WidthChanged(UpdateBitmap: Boolean = True); virtual; abstract;
  public
    constructor Create; virtual;

    procedure Clear; virtual; abstract;
    procedure PaintTo(Canvas: TCanvas); overload; virtual;
    procedure PaintTo(Canvas: TCanvas; X, Y: Integer); overload; virtual; abstract;
    procedure PaintTo(Canvas: TCanvas; Rect: TRect; X: Integer = 0; Y: Integer = 0); overload; virtual; abstract;

    procedure LoadFromFile(const Filename: TFileName); virtual;
    procedure SaveToFile(const Filename: TFileName); virtual;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    procedure SetSize(Width, Height: Integer); virtual;
    procedure Resize(Width, Height: Integer); virtual; abstract;
    procedure Turn(CounterClockwise: Boolean = False); virtual; abstract;

    property ClientRect: TRect read GetClientRect;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
  end;

resourcestring
  RCStrNoDibHandle = 'Can''t allocate the DIB handle';
  RCStrNoCompatibleDC = 'Can''t create compatible DC';
  RCStrNoSelectedDC = 'Can''t select an object into DC';

implementation

uses
  DAV_GuiFileFormats;

{ TGuiCustomMap }

constructor TGuiCustomMap.Create;
begin
 inherited;
 FDataSize := 0;
 FWidth    := 0;
 FHeight   := 0;
 FOnChange := nil;
 FOnResize := nil;
end;

procedure TGuiCustomMap.SaveToFile(const Filename: TFileName);
var
  Stream          : TStream;
  FileFormatClass : TGuiCustomFileFormatClass;
begin
 FileFormatClass := FindGraphicFileFormatByExtension(ExtractFileExt(Filename));
 if Assigned(FileFormatClass) then
  begin
   with FileFormatClass.Create do
    try
     Assign(Self);
     SaveToFile(Filename);
     Exit;
    finally
     Free;
    end;
  end;

 // if no file format was found use the default method
 Stream := TFileStream.Create(Filename, fmCreate);
 try
  SaveToStream(Stream);
 finally
  FreeAndNil(Stream);
 end;
end;

procedure TGuiCustomMap.SetHeight(const Value: Integer);
begin
 if Value < 0
  then raise Exception.Create('Height may not be negative!');

 if FHeight <> Value then
  begin
   FHeight := Value;
   HeightChanged;
  end;
end;

procedure TGuiCustomMap.SetSize(Width, Height: Integer);
begin
 if Width < 0
  then raise Exception.Create('Width may not be negative!');
 if Height < 0
  then raise Exception.Create('Height may not be negative!');

 if (FWidth <> Width) or (FHeight <> Height) then
  begin
   FWidth := Width;
   FHeight := Height;
   SizeChangedAtOnce;
  end;
end;

procedure TGuiCustomMap.SetWidth(const Value: Integer);
begin
 if Value < 0
  then raise Exception.Create('Width may not be negative!');

 if FWidth <> Value then
  begin
   FWidth := Value;
   WidthChanged;
  end;
end;

procedure TGuiCustomMap.Changed;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGuiCustomMap.Resized;
begin
 if Assigned(FOnResize) then FOnResize(Self);
 Changed;
end;

procedure TGuiCustomMap.SizeChangedAtOnce;
begin
 HeightChanged(False);
 WidthChanged(False);
 Resized;
end;

function TGuiCustomMap.Empty: Boolean;
begin
 Result := FDataSize = 0;
end;

function TGuiCustomMap.GetClientRect: TRect;
begin
 Result := Rect(0, 0, Width, Height);
end;

procedure TGuiCustomMap.LoadFromFile(const Filename: TFileName);
var
  Stream: TStream;
begin
 Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(Stream);
 finally
  FreeAndNil(Stream);
 end;
end;

procedure TGuiCustomMap.PaintTo(Canvas: TCanvas);
begin
 PaintTo(Canvas, 0, 0);
end;

end.
