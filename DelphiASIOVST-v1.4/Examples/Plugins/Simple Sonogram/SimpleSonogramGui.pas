unit SimpleSonogramGui;

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

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Controls, ExtCtrls, Graphics, StdCtrls, Menus, DAV_Types, 
  DAV_VSTModule, DAV_GuiPixelMap;

type
  TFmSonogram = class(TForm)
    Timer: TTimer;
    LbFftOrder: TLabel;
    PuFftOrder: TPopupMenu;
    MiOrder6: TMenuItem;
    MiOrder7: TMenuItem;
    MiOrder8: TMenuItem;
    MiOrder9: TMenuItem;
    MiOrder10: TMenuItem;
    MiOrder11: TMenuItem;
    MiOrder12: TMenuItem;
    MiOrder13: TMenuItem;
    MiOrder14: TMenuItem;
    LbOverlapFactor: TLabel;
    PuOverlapFactor: TPopupMenu;
    MiOverlapOrder1: TMenuItem;
    MiOverlapOrder2: TMenuItem;
    MiOverlapOrder3: TMenuItem;
    MiOverlapOrder4: TMenuItem;
    MiOverlapOrder5: TMenuItem;
    MiOverlapOrder6: TMenuItem;
    MiOverlapOrder7: TMenuItem;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure MiOrderClick(Sender: TObject);
    procedure MiOverlapOrderClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

implementation

uses
  Math, DAV_GuiCommon, SimpleSonogramDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmSonogram.FormCreate(Sender: TObject);
begin
 // Create Background Image
 FBackground := TGuiPixelMapMemory.Create;

 ControlStyle := ControlStyle + [csOpaque];
// Sonogram.ControlStyle := Sonogram.ControlStyle + [csOpaque];
end;

procedure TFmSonogram.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmSonogram.TimerTimer(Sender: TObject);
begin
 Invalidate;
end;

procedure TFmSonogram.FormPaint(Sender: TObject);
begin
 with FBackground do
  begin
   Draw(TSonogramDataModule(Owner).Sonogram.Bitmap, 8, 8);
(*
   Pen.Color := $0070848D;
   Pen.Width := 1;
   Brush.Style := bsClear;
   RoundRect(7, 7, 265, 265, 2, 2);
*)
  end;
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSonogram.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
       Filter[0] := Filter[1];

       ScnLn[x].B := Round($70 - $34 * (Filter[1] - h));
       ScnLn[x].G := Round($84 - $48 * (Filter[1] - h));
       ScnLn[x].R := Round($8D - $50 * (Filter[1] - h));
      end;
    end;
  end;
end;

procedure TFmSonogram.FormShow(Sender: TObject);
var
  MenuItem : TMenuItem;
begin
 MenuItem := nil;
 with TSonogramDataModule(Owner) do
  begin
   case Round(Parameter[0]) of
     6 : MenuItem := MiOrder6;
     7 : MenuItem := MiOrder7;
     8 : MenuItem := MiOrder8;
     9 : MenuItem := MiOrder9;
    10 : MenuItem := MiOrder10;
    11 : MenuItem := MiOrder11;
   end;
  end;

 if Assigned(MenuItem) then
  with MenuItem do
   begin
    Checked := True;
    LbFftOrder.Caption := 'FFT Order: ' + IntToStr(Tag);
   end;

 with TSonogramDataModule(Owner) do
  begin
   case Round(Log2(Parameter[1])) of
     1 : MenuItem := MiOverlapOrder1;
     2 : MenuItem := MiOverlapOrder2;
     3 : MenuItem := MiOverlapOrder3;
     4 : MenuItem := MiOverlapOrder4;
     5 : MenuItem := MiOverlapOrder5;
     6 : MenuItem := MiOverlapOrder6;
   end;
  end;

 if Assigned(MenuItem) then
  with MenuItem do
   begin
    Checked := True;
    LbOverlapFactor.Caption := 'Overlap Order: ' + IntToStr(Tag);
   end;
end;

procedure TFmSonogram.MiOrderClick(Sender: TObject);
begin
 with TMenuItem(Sender) do
  begin
   TSonogramDataModule(Self.Owner).Parameter[0] := Tag;
   Checked := True;
   LbFftOrder.Caption := 'FFT Order: ' + IntToStr(Tag);
  end;
end;

procedure TFmSonogram.MiOverlapOrderClick(Sender: TObject);
begin
 with TMenuItem(Sender) do
  begin
   TSonogramDataModule(Self.Owner).Parameter[1] := 1 shl Tag;
   Checked := True;
   LbOverlapFactor.Caption := 'Overlap Order: ' + IntToStr(Tag);
  end;
end;

end.
