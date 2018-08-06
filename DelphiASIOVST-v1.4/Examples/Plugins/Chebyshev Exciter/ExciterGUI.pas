unit ExciterGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Graphics, Controls, ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, 
  DAV_GuiPanel, DAV_GuiPixelMap, DAV_GuiStitchedControls, DAV_GuiStitchedDial, 
  DAV_GuiStitchedPngList, DAV_GuiImageControl, DAV_GuiCustomControl,
  DAV_GuiGraphicControl;

type
  TFmExciter = class(TForm)
    DialMix: TGuiStitchedDial;
    DialOrder: TGuiStitchedDial;
    DialShape: TGuiStitchedDial;
    DialTune: TGuiStitchedDial;
    DSPL: TGuiStitchedPNGList;
    LbFreq: TGuiLabel;
    LbFreqValue: TGuiLabel;
    LbMix: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbShape: TGuiLabel;
    LbShapeValue: TGuiLabel;
    PnControl: TGuiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialTuneChange(Sender: TObject);
    procedure DialShapeChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateTune;
    procedure UpdateOrder;
    procedure UpdateShape;
    procedure UpdateMix;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_GUICommon, ExciterDM;

procedure TFmExciter.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmExciter.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmExciter.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmExciter.FormResize(Sender: TObject);
var
  x, y  : Integer;
  s     : array [0..1] of Single;
  b     : ShortInt;
  ScnLn : PPixel32Array;
  h, hr : Single;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   hr := 1 / Height;
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h := 0.6 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * Random - 1);
       b := Round($3F + $1A * (h + s[1]));
       s[0] := s[1];
       ScnLn[x].B := b;
       ScnLn[x].G := b;
       ScnLn[x].R := b;
      end;
    end;
  end;
end;

procedure TFmExciter.DialTuneChange(Sender: TObject);
begin
 with Owner as TExciterDataModule do
  begin
   ParameterByName['Tune'] := DialTune.Value;
  end;
end;

procedure TFmExciter.DialMixChange(Sender: TObject);
begin
 with Owner as TExciterDataModule do
  begin
   ParameterByName['Mix'] := DialMix.Value;
  end; 
end;

procedure TFmExciter.DialShapeChange(Sender: TObject);
begin
 with Owner as TExciterDataModule do
  begin
   ParameterByName['Shape'] := DialShape.Value;
  end;
end;

procedure TFmExciter.DialOrderChange(Sender: TObject);
var
  CurrentOrder : Single;
  DesiredOrder : Integer;
begin
 with Owner as TExciterDataModule do
  begin
   DesiredOrder := Round(DialOrder.Value);
   CurrentOrder := ParameterByName['Order'];
   if DesiredOrder <> CurrentOrder
    then ParameterByName['Order'] := CurrentOrder;
(*
   if Round(CurrentOrder) = DesiredOrder then
    if DialOrder.Value < CurrentOrder
     then ParameterByName['Order'] := DesiredOrder - 1 else
    if DialOrder.Value > CurrentOrder
     then ParameterByName['Order'] := DesiredOrder + 1 else
   else ParameterByName['Order'] := DesiredOrder;
*)
  end;
end;

procedure TFmExciter.FormShow(Sender: TObject);
begin
 UpdateTune;
 UpdateOrder;
 UpdateShape;
 UpdateMix;
end;

procedure TFmExciter.UpdateTune;
var
  Freq : Single;
begin
 with Owner as TExciterDataModule do
  begin
   Freq := ParameterByName['Tune'];
   if Freq < 1000
    then LbFreqValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + 'Hz'
    else LbFreqValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + 'kHz';
   if DialTune.Value <> Freq
    then DialTune.Value := Freq;
  end;
end;

procedure TFmExciter.UpdateMix;
var
  Mix : Single;
begin
 with Owner as TExciterDataModule do
  begin
   Mix := ParameterByName['Mix'];
   LbMixValue.Caption := FloatToStrF(Mix, ffGeneral, 3, 1) + '%';
   if DialMix.Value <> Mix
    then DialMix.Value := Mix;
  end;
end;

procedure TFmExciter.UpdateShape;
var
  Shape : Single;
begin
 with Owner as TExciterDataModule do
  begin
   Shape := ParameterByName['Shape'];
   LbShapeValue.Caption := FloatToStrF(Shape, ffGeneral, 3, 1) + '%';
   if DialShape.Value <> Shape
    then DialShape.Value := Shape;
  end;
end;

procedure TFmExciter.UpdateOrder;
var
  Order : Integer;
begin
 with Owner as TExciterDataModule do
  begin
   Order := Round(ParameterByName['Order']);
   LbOrderValue.Caption := IntToStr(Order);
   if DialOrder.Value <> Order
    then DialOrder.Value := Order;
  end;
end;

end.
