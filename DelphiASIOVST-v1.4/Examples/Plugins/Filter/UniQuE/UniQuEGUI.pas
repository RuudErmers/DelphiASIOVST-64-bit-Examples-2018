unit UniQuEGUI;

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
  Forms, Controls, Graphics, StdCtrls, DAV_Types, DAV_VSTModule, 
  DAV_GuiPixelMap, DAV_GuiCustomControl, DAV_GuiGraphicControl, DAV_GuiLabel, 
  DAV_GuiGroup, DAV_GuiLED, DAV_GuiStitchedControls, DAV_GuiStitchedDial, 
  DAV_GuiStitchedPngList, DAV_GuiImageControl;

type
  TFmUniQuE = class(TForm)
    DialHigh: TGuiStitchedDial;
    DialLow: TGuiStitchedDial;
    DialMid: TGuiStitchedDial;
    DialPresence: TGuiStitchedDial;
    GpUnique: TGuiGroup;
    GSPL: TGuiStitchedPNGList;
    LbHigh: TGuiLabel;
    LbInvert: TGuiLabel;
    LbLow: TGuiLabel;
    LbMid: TGuiLabel;
    LbOnOff: TGuiLabel;
    LbPad: TGuiLabel;
    LEDInvert: TGuiLED;
    LEDOnOff: TGuiLED;
    LEDPad: TGuiLED;
    LbPRes: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DialHighChange(Sender: TObject);
    procedure DialLowChange(Sender: TObject);
    procedure DialMidChange(Sender: TObject);
    procedure DialPresenceChange(Sender: TObject);
    procedure InvertClick(Sender: TObject);
    procedure OnOffClick(Sender: TObject);
    procedure PadClick(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateOnOff;
    procedure UpdatePad;
    procedure UpdateInvert;
    procedure UpdateLow;
    procedure UpdateMid;
    procedure UpdatePres;
    procedure UpdateHigh;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_GuiCommon, UniQuEDM;

procedure TFmUniQuE.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmUniQuE.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmUniQuE.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmUniQuE.FormResize(Sender: TObject);
var
  X, Y   : Integer;
  Filter : array [0..1] of Single;
  Value  : ShortInt;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   for Y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[Y];
     for X := 0 to Width - 1 do
      begin
       Filter[1] := 0.9 * Filter[0] + 0.1 * Random;
       Value := Round($1F + $32 * Filter[1]);
       Filter[0] := Filter[1];
       ScnLn[X].B := Value;
       ScnLn[X].G := Value;
       ScnLn[X].R := Value;
      end;
    end;
  end;
end;

procedure TFmUniQuE.FormShow(Sender: TObject);
begin
 UpdateOnOff;
 UpdatePad;
 UpdateInvert;
 UpdateLow;
 UpdateMid;
 UpdatePres;
 UpdateHigh;
end;

procedure TFmUniQuE.UpdateInvert;
begin
 with TUniQuEDataModule(Owner) do
  begin
   LEDInvert.Brightness_Percent := 20 + 80 * (Parameter[2]);
  end;
end;

procedure TFmUniQuE.UpdateOnOff;
begin
 with TUniQuEDataModule(Owner) do
  begin
   LEDOnOff.Brightness_Percent := 20 + 80 * Parameter[0];
  end;
end;

procedure TFmUniQuE.UpdatePad;
begin
 with TUniQuEDataModule(Owner) do
  begin
   LEDPad.Brightness_Percent := 20 + 6 * Parameter[1];
  end;
end;

procedure TFmUniQuE.UpdateHigh;
begin
 with TUniQuEDataModule(Owner) do
  begin
   if DialHigh.Value <> Parameter[6]
    then DialHigh.Value := Parameter[6];
  end;
end;

procedure TFmUniQuE.UpdateLow;
begin
 with TUniQuEDataModule(Owner) do
  begin
   if DialLow.Value <> Parameter[3]
    then DialLow.Value := Parameter[3];
  end;
end;

procedure TFmUniQuE.UpdateMid;
begin
 with TUniQuEDataModule(Owner) do
  begin
   if DialMid.Value <> Parameter[4]
    then DialMid.Value := Parameter[4];
  end;
end;

procedure TFmUniQuE.UpdatePres;
begin
 with TUniQuEDataModule(Owner) do
  begin
   if DialPresence.Value <> Parameter[5]
    then DialPresence.Value := Parameter[5];
  end;
end;

procedure TFmUniQuE.DialLowChange(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[3] := DialLow.Value;
  end;
end;

procedure TFmUniQuE.DialMidChange(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[4] := DialMid.Value;
  end;
end;

procedure TFmUniQuE.DialPresenceChange(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[5] := DialPresence.Value;
  end;
end;

procedure TFmUniQuE.DialHighChange(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[6] := DialHigh.Value;
  end;
end;

procedure TFmUniQuE.OnOffClick(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[0] := Round(1 - Parameter[0]);
   UpdateOnOff;
  end;
end;

procedure TFmUniQuE.InvertClick(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[2] := Round(1 - Parameter[2]);
   UpdateInvert;
  end;
end;

procedure TFmUniQuE.PadClick(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   if Parameter[1] < 6
    then Parameter[1] := 12
    else Parameter[1] := 0;
   UpdatePad;
  end;
end;

end.
