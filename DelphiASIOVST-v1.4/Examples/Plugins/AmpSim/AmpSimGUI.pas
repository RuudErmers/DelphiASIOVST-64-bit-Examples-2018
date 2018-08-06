unit AmpSimGUI;

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

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  DAV_GuiBaseControl, DAV_GuiSelectBox, DAV_GuiLED, DAV_GuiPanel,
  DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_GuiStitchedDial,
  DAV_GuiPixelMap, DAV_GuiInscription, DAV_GuiFont, DAV_GuiFontList,
  DAV_GuiImageControl, DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TFmCombo = class(TForm)
    GuiLED: TGuiLED;
    PnControls: TGuiPanel;
    LbBiasValue: TLabel;
    LbDriveValue: TLabel;
    LbFrequencyValue: TLabel;
    LbOutputValue: TLabel;
    LbResonanceValue: TLabel;
    SBModel: TGuiSelectBox;
    DialDrive: TGuiStitchedDial;
    DialBias: TGuiStitchedDial;
    DialOutput: TGuiStitchedDial;
    DialFrequency: TGuiStitchedDial;
    DialResonance: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbModel: TGuiInscription;
    LbStereo: TGuiInscription;
    LbDrive: TGuiInscription;
    LbBias: TGuiInscription;
    LbOutput: TGuiInscription;
    LbFrequency: TGuiInscription;
    LbResonance: TGuiInscription;
    GFL: TGuiFontList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialBiasChange(Sender: TObject);
    procedure DialDriveChange(Sender: TObject);
    procedure DialDriveDblClick(Sender: TObject);
    procedure DialFreqChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialResoChange(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure LbStereoClick(Sender: TObject);
    procedure PnControlsClick(Sender: TObject);
    procedure SBModelChange(Sender: TObject);
    procedure DialBiasDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
    FEdValue    : TEdit;
  public
    procedure UpdateBias;
    procedure UpdateDrive;
    procedure UpdateFreq;
    procedure UpdateModel;
    procedure UpdateNoise;
    procedure UpdateOutput;
    procedure UpdateProcess;
    procedure UpdateReso;
  end;

var
  FmCombo: TFmCombo;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  DAV_GuiCommon, AmpSimDM;

procedure TFmCombo.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmCombo.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmCombo.FormShow(Sender: TObject);
begin
 UpdateModel;
 UpdateDrive;
 UpdateBias;
 UpdateOutput;
 UpdateFreq;
 UpdateReso;
 UpdateProcess;
 UpdateNoise;
 LbModel.Transparent := True;
 LbStereo.Transparent := True;
end;

procedure TFmCombo.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmCombo.FormResize(Sender: TObject);
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
       s[1] := 0.97 * s[0] + 0.06 * random - 0.03;
       s[0] := s[1];

       ScnLn[x].B := Round($40 - $10 * (s[1] - h));
       ScnLn[x].G := Round($80 - $20 * (s[1] - h));
       ScnLn[x].R := Round($80 - $20 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmCombo.SBModelChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[0] <> SBModel.ItemIndex
    then Parameter[0] := SBModel.ItemIndex;
  end;
end;

procedure TFmCombo.UpdateDrive;
var
  Drive : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Drive := ParameterByName['Drive'];
   if DialDrive.Value <> Drive
    then DialDrive.Value := Drive;
   LbDriveValue.Caption := FloatToStrF(Drive, ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmCombo.UpdateBias;
var
  Bias : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Bias := ParameterByName['Bias'];
   if DialBias.Value <> Bias
    then DialBias.Value := Bias;
   LbBiasValue.Caption :=  FloatToStrF(Bias, ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmCombo.UpdateFreq;
var
  Frequency : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Frequency := ParameterByName['HPF Frequency'];
   if DialFrequency.Value <> Frequency
    then DialFrequency.Value := Frequency;
   LbFrequencyValue.Caption := FloatToStrF(Frequency, ffGeneral, 5, 5) + ' Hz';
  end;
end;

procedure TFmCombo.UpdateProcess;
begin
 with TComboDataModule(Owner) do
  begin
   if Stereo then
    begin
     LbStereo.Caption := 'Stereo';
     GuiLED.Brightness_Percent := 100;
    end
   else
    begin
     LbStereo.Caption := 'Mono';
     GuiLED.Brightness_Percent := 20;
    end;
  end;
end;

procedure TFmCombo.UpdateModel;
var
  Model : Integer;
begin
 with TComboDataModule(Owner) do
  begin
   Model := Round(ParameterByName['Model']);
   if SBModel.ItemIndex <> Model
    then SBModel.ItemIndex := Model;
  end;
end;

procedure TFmCombo.UpdateNoise;
begin

end;

procedure TFmCombo.UpdateOutput;
var
  Output : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Output := ParameterByName['Output'];
   if DialOutput.Value <> Output
    then DialOutput.Value := Output;
   LbOutputValue.Caption := FloatToStrF(Output, ffGeneral, 3, 3) + ' dB';
  end;
end;

procedure TFmCombo.UpdateReso;
var
  Reso : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Reso := ParameterByName['HPF Resonance'];
   if DialResonance.Value <> Reso
    then DialResonance.Value := Reso;
   LbResonanceValue.Caption := FloatToStrF(Reso, ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmCombo.DialBiasChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[2] <> DialBias.Value
    then Parameter[2] := DialBias.Value;
  end;
end;

procedure TFmCombo.DialDriveChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[1] <> DialDrive.Value
    then Parameter[1] := DialDrive.Value;
  end;
end;

procedure TFmCombo.DialDriveDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbDriveValue.Left;
   Top := LbDriveValue.Top;
   Width := LbDriveValue.Width;
   Height := LbDriveValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbDriveValue.Caption;
   Tag := 1;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmCombo.DialBiasDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbBiasValue.Left;
   Top := LbBiasValue.Top;
   Width := LbBiasValue.Width;
   Height := LbBiasValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbBiasValue.Caption;
   Tag := 2;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmCombo.DialOutputChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[3] <> DialOutput.Value
    then Parameter[3] := DialOutput.Value;
  end;
end;

procedure TFmCombo.DialFreqChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[5] <> DialFrequency.Value
    then Parameter[5] := DialFrequency.Value;
  end;
end;

procedure TFmCombo.DialResoChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[6] <> DialResonance.Value
    then Parameter[6] := DialResonance.Value;
  end;
end;

procedure TFmCombo.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 with TComboDataModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, AnsiString(FEdValue.Text));
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmCombo.PnControlsClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmCombo.LbStereoClick(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   Parameter[4] := 1 - Parameter[4];
  end;
end;

end.
