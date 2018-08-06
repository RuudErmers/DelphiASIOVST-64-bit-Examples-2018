unit DAV_GuiPngDesign;

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
  {$IFDEF FPC} LCLIntf, LCLClasses, LCLType, LResources, LMessages, RtlConsts,
  LazIDEIntf, PropEdits, ComponentEditors, FormEditingIntf,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$ELSE} Windows, Messages, DesignIntf, DesignEditors, VCLEditors, Registry,
  Clipbrd, Consts,{$ENDIF}
  Forms, Graphics, Classes, SysUtils, Dialogs, StdCtrls, Controls, ExtCtrls,
  Menus, DAV_Common, DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiPng;

type
  TFmPngDialog = class(TForm)
    PaintBox: TPaintBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    MainMenu: TMainMenu;
    MiFile: TMenuItem;
    MiLoadPng: TMenuItem;
    MiSavePng: TMenuItem;
    N1: TMenuItem;
    MiExit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiLoadPngClick(Sender: TObject);
    procedure MiSavePngClick(Sender: TObject);
  private
    FPixelMap : TGuiPixelMapMemory;
    FBuffer   : TGuiPixelMapMemory;
    FPng      : TPortableNetworkGraphicPixel32;
  public
    property PixelMap: TGuiPixelMapMemory read FPixelMap;
    property PortableNetworkGraphic: TPortableNetworkGraphicPixel32 read FPng;
  end;

  TPngEditorComponent = class(TComponent)
  private
    FPng       : TPortableNetworkGraphicPixel32;
    FPngDialog : TFmPngDialog;
    procedure SetPng(Value: TPortableNetworkGraphicPixel32);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;

    property PortableNetworkGraphic: TPortableNetworkGraphicPixel32 read FPng write SetPng;
  end;

  TPngProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TPngEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


implementation

{$R *.dfm}

uses
  DAV_GuiFileFormats;

procedure TFmPngDialog.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
 FPixelMap.SetSize(PaintBox.Width, PaintBox.Height);

 FBuffer := TGuiPixelMapMemory.Create;
 FBuffer.SetSize(PaintBox.Width, PaintBox.Height);

 FPng := TPortableNetworkGraphicPixel32.Create;
end;

procedure TFmPngDialog.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FPng);
 FreeAndNil(FPixelMap);
 FreeAndNil(FBuffer);
end;

procedure TFmPngDialog.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmPngDialog.MiLoadPngClick(Sender: TObject);
begin
 with OpenDialog do
  if Execute then
   begin
    FPng.LoadFromFile(OpenDialog.FileName);
    FPng.AssignTo(FPixelMap);

    Self.ClientWidth := FPixelMap.Width + 16;
    Self.ClientHeight := FPixelMap.Height + 16;
    PaintBox.Invalidate;
   end;
end;

procedure TFmPngDialog.MiSavePngClick(Sender: TObject);
begin
 with SaveDialog do
  if Execute
   then FPng.SaveToFile(FileName);
end;

procedure TFmPngDialog.PaintBoxPaint(Sender: TObject);
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


{ TPngEditorComponent }

constructor TPngEditorComponent.Create(AOwner: TComponent);
begin
 inherited;
 FPng := TPortableNetworkGraphicPixel32.Create;
 FPngDialog := TFmPngDialog.Create(Self);
end;

destructor TPngEditorComponent.Destroy;
begin
 FreeAndNil(FPng);
 FreeAndNil(FPngDialog);
 inherited;
end;

function TPngEditorComponent.Execute: Boolean;
begin
 with FPngDialog do
  begin
   PortableNetworkGraphic.Assign(Self.FPng);
   PortableNetworkGraphic.AssignTo(PixelMap);
   ClientWidth := FPng.Width + 16;
   ClientHeight := FPng.Height + 16;
   Result := ShowModal = mrOK;
   Self.FPng.Assign(FPngDialog.PortableNetworkGraphic);
   Result := True;
  end;
end;

procedure TPngEditorComponent.SetPng(Value: TPortableNetworkGraphicPixel32);
begin
 try
  FPng.Assign(Value);
 except
  on E: Exception do ShowMessage(E.Message);
 end;
end;


{ TPngProperty }

procedure TPngProperty.Edit;
var
  PngEditor: TPngEditorComponent;
begin
 try
  PngEditor := TPngEditorComponent.Create(nil);
  try
   PngEditor.PortableNetworkGraphic := TPortableNetworkGraphicPixel32(Pointer(GetOrdValue));
   if PngEditor.Execute then
    begin
     SetOrdValue(Longint(PngEditor.PortableNetworkGraphic));
     {$IFNDEF FPC}
     Designer.Modified;
     {$ENDIF}
    end;
  finally
   FreeAndNil(PngEditor);
  end;
 except
  on E: Exception do ShowMessage(E.Message);
 end;
end;

function TPngProperty.GetAttributes: TPropertyAttributes;
begin
 Result := [paDialog, paSubProperties];
end;

function TPngProperty.GetValue: string;
var
  Png: TPortableNetworkGraphicPixel32;
begin
 try
  Png := TPortableNetworkGraphicPixel32(GetOrdValue);
  if (Png = nil) then Result := srNone
  else Result := Format('%s [%d,%d]', [Png.ClassName, Png.Width, Png.Height]);
 except
  on E: Exception do ShowMessage(E.Message);
 end;
end;

procedure TPngProperty.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;


{ TPngEditor }

procedure TPngEditor.ExecuteVerb(Index: Integer);
begin
 inherited;
 // yet todo, not used yet!
end;

function TPngEditor.GetVerb(Index: Integer): string;
begin
 if Index = 0 then Result := 'Png Editor...';
end;

function TPngEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
