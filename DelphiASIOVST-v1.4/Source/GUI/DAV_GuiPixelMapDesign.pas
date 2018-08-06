unit DAV_GuiPixelMapDesign;

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
  {$IFDEF FPC} LCLIntf, LCLClasses, LCLType, LResources, LMessages, RtlConsts,
  LazIDEIntf, PropEdits, ComponentEditors, FormEditingIntf,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$ELSE} Windows, Messages, DesignIntf, DesignEditors, VCLEditors, Registry,
  Clipbrd, Consts,{$ENDIF}
  Forms, Graphics, Classes, SysUtils, Dialogs, StdCtrls, Controls, ExtCtrls,
  Menus, DAV_Common, DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiPng,
  DAV_GuiFileFormatGraphics;

type
  TFmPixelMapDialog = class(TForm)
    PaintBox: TPaintBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    MainMenu: TMainMenu;
    MiFile: TMenuItem;
    MiLoad: TMenuItem;
    N1: TMenuItem;
    MiExit: TMenuItem;
    MiSaveImage: TMenuItem;
    MiGenerate: TMenuItem;
    MiBrushedMetal: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiLoadClick(Sender: TObject);
    procedure MiSaveImageClick(Sender: TObject);
    procedure MiBrushedMetalClick(Sender: TObject);
  private
    FPixelMap : TGuiPixelMapMemory;
    FBuffer   : TGuiPixelMapMemory;
  public
    property PixelMap: TGuiPixelMapMemory read FPixelMap;
  end;

  TPixelMapEditorComponent = class(TComponent)
  private
    FPixelMap       : TGuiCustomPixelMap;
    FPixelMapDialog : TFmPixelMapDialog;
    procedure SetPixelMap(Value: TGuiCustomPixelMap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;

    property PixelMap: TGuiCustomPixelMap read FPixelMap write SetPixelMap;
  end;

  TPixelMapProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TPixelMapEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{$R *.dfm}

uses
  DAV_GuiFileFormats;

procedure TFmPixelMapDialog.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
 FPixelMap.SetSize(PaintBox.Width, PaintBox.Height);

 FBuffer := TGuiPixelMapMemory.Create;
 FBuffer.SetSize(PaintBox.Width, PaintBox.Height);
end;

procedure TFmPixelMapDialog.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FPixelMap);
 FreeAndNil(FBuffer);
end;

procedure TFmPixelMapDialog.MiBrushedMetalClick(Sender: TObject);
var
  x, y      : Integer;
  s         : array [0..1] of Single;
  hr, h     : Single;
  ScnLn     : PPixel32Array;
begin
 s[0] := 0;
 s[1] := 0;
 with FPixelMap do
  begin
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h     := 0.1 * (1 - Sqr(2 * (y - Height * 0.5) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       ScnLn[x].B := Round($70 - $34 * (s[1] - h));
       ScnLn[x].G := Round($84 - $48 * (s[1] - h));
       ScnLn[x].R := Round($8D - $50 * (s[1] - h));
       ScnLn[x].A := $FF;
      end;
    end;
  end;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapDialog.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmPixelMapDialog.MiLoadClick(Sender: TObject);
var
  FileFormatClass  : TGuiCustomFileFormatClass;
begin
 with OpenDialog do
  if Execute then
   begin
    FileFormatClass := FindGraphicFileFormatByFileName(OpenDialog.FileName);
    if Assigned(FileFormatClass) then
     begin
      with FileFormatClass.Create do
       try
        LoadFromFile(OpenDialog.FileName);
        AssignTo(FPixelMap);
        FBuffer.SetSize(PaintBox.Width, PaintBox.Height);
       finally
        Free;
       end;
     end;

    Self.ClientWidth := FPixelMap.Width + 16;
    Self.ClientHeight := FPixelMap.Height + 16;
    PaintBox.Invalidate;
   end;
end;

procedure TFmPixelMapDialog.MiSaveImageClick(Sender: TObject);
begin
 with SaveDialog do
  if Execute
   then FPixelMap.SaveToFile(FileName);
end;

procedure TFmPixelMapDialog.PaintBoxPaint(Sender: TObject);
var
  X, Y   : Integer;
  ScnLn  : PPixel32Array;
  XIndex : Integer;
  YIndex : Integer;
const
  CCheckerBoardColors : array [0..1] of TPixel32 = ((ARGB : $FFFFFFFF),
    (ARGB : $FF7F7F7F));
begin
 if Assigned(FPixelMap) then
  begin
   if Assigned(FBuffer) then
    begin
     FBuffer.SetSize(FPixelMap.Width, FPixelMap.Height);

     // draw checker board
     XIndex := 0;
     YIndex := 0;
     for Y := 0 to FPixelMap.Height - 1 do
      begin
       ScnLn := FBuffer.ScanLine[Y];
       XIndex := YIndex;
       for X := 0 to FPixelMap.Width - 1 do
        begin
         ScnLn[X] := CCheckerBoardColors[XIndex];
         if X mod 8 = 7 then XIndex := 1 - XIndex;
        end;
       if Y mod 8 = 7 then YIndex := 1 - YIndex;
      end;

     FBuffer.DrawTransparent(FPixelMap);
     FBuffer.PaintTo(PaintBox.Canvas);
    end
   else FPixelMap.PaintTo(PaintBox.Canvas);
  end;
end;


{ TPixelMapEditorComponent }

constructor TPixelMapEditorComponent.Create(AOwner: TComponent);
begin
 inherited;
 FPixelMap := TGuiPixelMapMemory.Create;
 FPixelMapDialog := TFmPixelMapDialog.Create(Self);
end;

destructor TPixelMapEditorComponent.Destroy;
begin
 FreeAndNil(FPixelMap);
 FreeAndNil(FPixelMapDialog);
 inherited;
end;

function TPixelMapEditorComponent.Execute: Boolean;
begin
 with FPixelMapDialog do
  begin
   PixelMap.Assign(Self.FPixelMap);
   ClientWidth := FPixelMap.Width + 16;
   ClientHeight := FPixelMap.Height + 16;
   Result := ShowModal = mrOK;
   // if Result then

   Self.FPixelMap.Assign(FPixelMapDialog.PixelMap);
   Result := True;
  end;
end;

procedure TPixelMapEditorComponent.SetPixelMap(Value: TGuiCustomPixelMap);
begin
 try
  FPixelMap.Assign(Value);
 except
  on E: Exception do ShowMessage(E.Message);
 end;
end;


{ TPixelMapProperty }

procedure TPixelMapProperty.Edit;
var
  PixelMapEditor: TPixelMapEditorComponent;
begin
 try
  PixelMapEditor := TPixelMapEditorComponent.Create(nil);
  try
   PixelMapEditor.PixelMap := TGuiCustomPixelMap(Pointer(GetOrdValue));
   if PixelMapEditor.Execute then
    begin
     SetOrdValue(Longint(PixelMapEditor.PixelMap));
     {$IFNDEF FPC}
     Designer.Modified;
     {$ENDIF}
    end;
  finally
   FreeAndNil(PixelMapEditor);
  end;
 except
  on E: Exception do ShowMessage(E.Message);
 end;
end;

function TPixelMapProperty.GetAttributes: TPropertyAttributes;
begin
 Result := [paDialog, paSubProperties];
end;

function TPixelMapProperty.GetValue: string;
var
  PixelMap: TGuiCustomPixelMap;
begin
 try
  PixelMap := TGuiCustomPixelMap(GetOrdValue);
  if (PixelMap = nil) then Result := srNone
  else Result := Format('%s [%d,%d]', [PixelMap.ClassName, PixelMap.Width, PixelMap.Height]);
 except
  on E: Exception do ShowMessage(E.Message);
 end;
end;

procedure TPixelMapProperty.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;


{ TPixelMapEditor }

procedure TPixelMapEditor.ExecuteVerb(Index: Integer);
begin
 inherited;
 // yet todo, not used yet!
end;

function TPixelMapEditor.GetVerb(Index: Integer): string;
begin
 if Index = 0 then Result := 'PixelMap Editor...';
end;

function TPixelMapEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
