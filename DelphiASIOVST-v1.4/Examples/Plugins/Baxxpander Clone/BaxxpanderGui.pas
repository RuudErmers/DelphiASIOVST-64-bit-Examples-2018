unit BaxxpanderGui;

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

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Controls, Graphics, ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiDial, 
  DAV_GuiPanel, DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiLED, DAV_GuiPixelMap,
  DAV_GuiCustomControl, DAV_GuiImageControl, DAV_GuiStitchedControls,
  DAV_GuiStitchedDial, DAV_GuiStitchedPngList, DAV_GuiGraphicControl;

type
  TFmBaxxpanderGui = class(TForm)
    DlDryWet: TGuiStitchedDial;
    DlLimit: TGuiStitchedDial;
    DlMixer: TGuiStitchedDial;
    DlShape: TGuiStitchedDial;
    LbBaxxpander: TGuiLabel;
    LbClone: TGuiLabel;
    LbDryWet: TGuiLabel;
    LbLimit: TGuiLabel;
    LbManufacturer: TGuiLabel;
    LbMixer: TGuiLabel;
    LbSaturation: TGuiLabel;
    LbShape: TGuiLabel;
    LEDSaturation: TGuiLED;
    PnControls: TGuiPanel;
    StitchedPNG: TGuiStitchedPNGList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DlDryWetChange(Sender: TObject);
    procedure DlMixerChange(Sender: TObject);
    procedure DlLimitChange(Sender: TObject);
    procedure DlShapeChange(Sender: TObject);
    procedure LEDSaturationClick(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateShape;
    procedure UpdateLimit;
    procedure UpdateMixer;
    procedure UpdateDryWet;
    procedure UpdateSaturation;
  end;

implementation

uses
  DAV_Common, DAV_GuiCommon, BaxxpanderModule;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmBaxxpanderGui.DlDryWetChange(Sender: TObject);
begin
 with TBaxxpanderModule(Owner) do
  if Parameter[0] <> DlDryWet.Value
   then Parameter[0] := DlDryWet.Value;
end;

procedure TFmBaxxpanderGui.DlLimitChange(Sender: TObject);
begin
 with TBaxxpanderModule(Owner) do
  if Parameter[1] <> DlLimit.Value
   then Parameter[1] := DlLimit.Value;
end;

procedure TFmBaxxpanderGui.DlMixerChange(Sender: TObject);
begin
 with TBaxxpanderModule(Owner) do
  if Parameter[2] <> DlMixer.Value
   then Parameter[2] := DlMixer.Value;
end;

procedure TFmBaxxpanderGui.DlShapeChange(Sender: TObject);
begin
 with TBaxxpanderModule(Owner) do
  if Parameter[4] <> DlShape.Value
   then Parameter[4] := DlShape.Value;
end;

procedure TFmBaxxpanderGui.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array [0..3] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
//  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackground := TGuiPixelMapMemory.Create;
 with FBackground do
  begin
   Width := ClientWidth;
   Height := ClientHeight;
   s[0] := 0;
   s[1] := 0;
   s[2] := 0.5;
   s[3] := 0.5;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.2 * (1 - Sqr(2 * (y - Height div 2) * hr)) + 0.3 * s[3];
     if Random < 0.2
      then s[2] := Random;
     s[3] := 0.5 * (s[3] + s[2]);

     for x := 0 to Width - 1 do
      begin
       s[1] := 0.98 * s[0] + 0.02 * Random;
       s[0] := s[1];

       ScnLn[x].B := Round($52 - $36 * (s[1] - h));
       ScnLn[x].G := Round($94 - $62 * (s[1] - h));
       ScnLn[x].R := Round($CF - $8A * (s[1] - h));
       ScnLn[x].A := $FF;
      end;
    end;
  end;
end;

procedure TFmBaxxpanderGui.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmBaxxpanderGui.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmBaxxpanderGui.LEDSaturationClick(Sender: TObject);
begin
 LEDSaturation.Brightness_Percent := 100 - LEDSaturation.Brightness_Percent;
 with TBaxxpanderModule(Owner) do
  if Parameter[3] <> 0.01 * LEDSaturation.Brightness_Percent
   then Parameter[3] := 0.01 * LEDSaturation.Brightness_Percent;
end;

procedure TFmBaxxpanderGui.UpdateDryWet;
begin
 with TBaxxpanderModule(Owner) do
  if DlDryWet.Value <> Parameter[0]
   then DlDryWet.Value := Parameter[0];
end;

procedure TFmBaxxpanderGui.UpdateLimit;
begin
 with TBaxxpanderModule(Owner) do
  if DlLimit.Value <> Parameter[1]
   then DlLimit.Value := Parameter[1];
end;

procedure TFmBaxxpanderGui.UpdateMixer;
begin
 with TBaxxpanderModule(Owner) do
  if DlMixer.Value <> Parameter[2]
   then DlMixer.Value := Parameter[2];
end;

procedure TFmBaxxpanderGui.UpdateSaturation;
begin
 with TBaxxpanderModule(Owner) do
  if LEDSaturation.Brightness_Percent <> 100 * Limit(Parameter[3], 0, 1)
   then LEDSaturation.Brightness_Percent := 100 * Limit(Parameter[3], 0, 1);
end;

procedure TFmBaxxpanderGui.UpdateShape;
begin
 with TBaxxpanderModule(Owner) do
  if DlShape.Value <> Parameter[4]
   then DlShape.Value := Parameter[4];
end;

end.
