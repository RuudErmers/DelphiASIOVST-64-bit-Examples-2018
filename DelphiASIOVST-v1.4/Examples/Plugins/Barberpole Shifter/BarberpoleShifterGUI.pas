unit BarberpoleShifterGUI;

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
  Forms, Controls, ExtCtrls, StdCtrls, Graphics, DAV_Types, DAV_VSTModule,
  DAV_GuiPixelMap, DAV_GuiCustomControl, DAV_GuiGraphicControl, DAV_GuiLabel,
  DAV_GuiImageControl, DAV_GuiStitchedControls, DAV_GuiStitchedPngList,
  DAV_GuiStitchedDial, DAV_GuiPanel, DAV_GuiGroup, DAV_GuiCheckBox;

type
  TFmBarberpoleShifter = class(TForm)
    DialFrequency: TGuiStitchedDial;
    DialMix: TGuiStitchedDial;
    DSPL: TGuiStitchedPNGList;
    GpFrequency: TGuiGroupTop;
    GpMix: TGuiGroupTop;
    LbFrequencyValue: TGuiLabel;
    LbMixValue: TGuiLabel;
    PnDisplay: TGuiPanel;
    PnMix: TGuiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateFrequency;
    procedure UpdateMix;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  DAV_GuiCommon, DAV_VSTModuleWithPrograms, BarberpoleShifterDM;

{ TFmBarberpoleShifter }

procedure TFmBarberpoleShifter.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmBarberpoleShifter.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * Random;
       s[0] := s[1];

       ScnLn[x].B := Round($70 - $34 * (s[1] - h));
       ScnLn[x].G := Round($84 - $48 * (s[1] - h));
       ScnLn[x].R := Round($8D - $50 * (s[1] - h));
       ScnLn[x].A := $FF;
      end;
    end;
  end;
end;

procedure TFmBarberpoleShifter.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateMix;
 DialFrequency.Transparent := True;
 DialMix.Transparent := True;
end;

procedure TFmBarberpoleShifter.FormPaint(Sender: TObject);
begin
 FBackground.PaintTo(Canvas);
end;

procedure TFmBarberpoleShifter.DialFrequencyChange(Sender: TObject);
begin
 with TBarberpoleShifterDataModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Value
    then Parameter[0] := DialFrequency.Value;
  end;
end;

procedure TFmBarberpoleShifter.DialMixChange(Sender: TObject);
begin
 with TBarberpoleShifterDataModule(Owner) do
  begin
   if Parameter[1] <> DialMix.Value
    then Parameter[1] := DialMix.Value;
  end;
end;

procedure TFmBarberpoleShifter.UpdateFrequency;
begin
 with TBarberpoleShifterDataModule(Owner) do
  begin
   if DialFrequency.Value <> Parameter[0]
    then DialFrequency.Value := Parameter[0];
   LbFrequencyValue.Caption := string(ParameterDisplay[0] + 'Hz');
  end;
end;

procedure TFmBarberpoleShifter.UpdateMix;
begin
 with TBarberpoleShifterDataModule(Owner) do
  begin
   if DialMix.Value <> Parameter[1]
    then DialMix.Value := Parameter[1];
   LbMixValue.Caption := string(ParameterDisplay[1] + '%');
  end;
end;

end.
