unit LookaheadLimiterGUI;

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
  Forms, Controls, ExtCtrls, StdCtrls, Menus, DAV_Types, DAV_VSTModule, 
  DAV_GuiBaseControl, DAV_GuiLED, DAV_GuiLevelMeter, DAV_GuiSelectBox, 
  DAV_GuiLabel, DAV_GuiStitchedControls, DAV_GuiStitchedPngList, 
  DAV_GuiStitchedDial;

type
  TFmLookaheadLimiter = class(TForm)
    DialInput: TGuiStitchedDial;
    DialOutput: TGuiStitchedDial;
    DialRelease: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    Lb0dB: TGuiLabel;
    Lb10dB: TGuiLabel;
    Lb15dB: TGuiLabel;
    Lb5dB: TGuiLabel;
    LbGR: TGuiLabel;
    LbInput: TGuiLabel;
    LbInputValue: TGuiLabel;
    LbOutput: TGuiLabel;
    LbOutputValue: TGuiLabel;
    LbProcessingMode: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LMGainReduction: TGuiColorLevelMeter;
    Mi001dB: TMenuItem;
    Mi002dB: TMenuItem;
    Mi003dB: TMenuItem;
    Mi005dB: TMenuItem;
    Mi01dB: TMenuItem;
    Mi02dB: TMenuItem;
    Mi0dB: TMenuItem;
    MiGain0dB: TMenuItem;
    MiGain1dB: TMenuItem;
    MiGain2dB: TMenuItem;
    MiGain3dB: TMenuItem;
    PuInputValues: TPopupMenu;
    PuOutputValues: TPopupMenu;
    SbProcessingType: TGuiSelectBox;
    Timer: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure DialInputChange(Sender: TObject);
    procedure DialInputDblClick(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure Mi001dBClick(Sender: TObject);
    procedure Mi002dBClick(Sender: TObject);
    procedure Mi003dBClick(Sender: TObject);
    procedure Mi005dBClick(Sender: TObject);
    procedure Mi01dBClick(Sender: TObject);
    procedure Mi02dBClick(Sender: TObject);
    procedure Mi0dBClick(Sender: TObject);
    procedure MiGain0dBClick(Sender: TObject);
    procedure MiGain1dBClick(Sender: TObject);
    procedure MiGain2dBClick(Sender: TObject);
    procedure MiGain3dBClick(Sender: TObject);
    procedure SbProcessingTypeChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure DialOutputDblClick(Sender: TObject);
    procedure DialReleaseDblClick(Sender: TObject);
  private
    FEdValue: TEdit;
  public
    procedure UpdateInput;
    procedure UpdateOutput;
    procedure UpdateRelease;
    procedure UpdateProcessingMode;
  end;

implementation

uses
  DAV_Approximations, DAV_VSTModuleWithPrograms, LookaheadLimiterDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmLookaheadLimiter.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmLookaheadLimiter.FormShow(Sender: TObject);
begin
 UpdateInput;
 UpdateOutput;
 UpdateRelease;
 UpdateProcessingMode;
end;

procedure TFmLookaheadLimiter.FormClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmLookaheadLimiter.Mi0dBClick(Sender: TObject);
begin
 DialOutput.Value := 0;
end;

procedure TFmLookaheadLimiter.MiGain0dBClick(Sender: TObject);
begin
 DialInput.Value := 0;
end;

procedure TFmLookaheadLimiter.MiGain1dBClick(Sender: TObject);
begin
 DialInput.Value := 1;
end;

procedure TFmLookaheadLimiter.MiGain2dBClick(Sender: TObject);
begin
 DialInput.Value := 2;
end;

procedure TFmLookaheadLimiter.MiGain3dBClick(Sender: TObject);
begin
 DialInput.Value := 3;
end;

procedure TFmLookaheadLimiter.SbProcessingTypeChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Parameter[2] := SbProcessingType.ItemIndex;
  end;
end;

procedure TFmLookaheadLimiter.Mi001dBClick(Sender: TObject);
begin
 DialOutput.Value := -0.01;
end;

procedure TFmLookaheadLimiter.Mi002dBClick(Sender: TObject);
begin
 DialOutput.Value := -0.02;
end;

procedure TFmLookaheadLimiter.Mi003dBClick(Sender: TObject);
begin
 DialOutput.Value := -0.03;
end;

procedure TFmLookaheadLimiter.Mi005dBClick(Sender: TObject);
begin
 DialOutput.Value := -0.05;
end;

procedure TFmLookaheadLimiter.Mi01dBClick(Sender: TObject);
begin
 DialOutput.Value := -0.1;
end;

procedure TFmLookaheadLimiter.Mi02dBClick(Sender: TObject);
begin
 DialOutput.Value := -0.2;
end;

procedure TFmLookaheadLimiter.TimerTimer(Sender: TObject);
var
  GR : Single;
begin
 with TLookaheadLimiterDataModule(Owner), LMGainReduction do
  begin
   GR := FastAmptodBMinError3(Limiter.GainReductionFactor);
   if GR > PeakLevel
    then PeakLevel := 0.2 * PeakLevel + 0.8 * GR
    else PeakLevel := 0.8 * PeakLevel + 0.2 * GR;
  end;
end;

procedure TFmLookaheadLimiter.DialInputChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   if Parameter[0] <> DialInput.Value
    then Parameter[0] := DialInput.Value;
  end;
end;

procedure TFmLookaheadLimiter.DialInputDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent      := Self;
   Left        := LbInputValue.Left;
   Top         := LbInputValue.Top;
   Width       := LbInputValue.Width;
   Height      := LbInputValue.Height;
   BorderStyle := bsNone;
   Color       := Self.Color;
   Text        := LbInputValue.Caption;
   Tag         := 0;
   TabOrder    := 0;
   OnKeyPress  := EdValueKeyPress;
   Font.Assign(Self.Font);
   SetFocus;
  end;
end;

procedure TFmLookaheadLimiter.DialOutputChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   if Parameter[1] <> DialOutput.Value
    then Parameter[1] := DialOutput.Value;
  end;
end;

procedure TFmLookaheadLimiter.DialOutputDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent      := Self;
   Left        := LbOutputValue.Left;
   Top         := LbOutputValue.Top;
   Width       := LbOutputValue.Width;
   Height      := LbOutputValue.Height;
   BorderStyle := bsNone;
   Color       := Self.Color;
   Text        := LbOutputValue.Caption;
   Tag         := 1;
   TabOrder    := 0;
   OnKeyPress  := EdValueKeyPress;
   Font.Assign(Self.Font);
   SetFocus;
  end;
end;

procedure TFmLookaheadLimiter.DialReleaseChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   if Parameter[3] <> DialRelease.Value
    then Parameter[3] := DialRelease.Value;
  end;
end;

procedure TFmLookaheadLimiter.DialReleaseDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent      := Self;
   Left        := LbReleaseValue.Left;
   Top         := LbReleaseValue.Top;
   Width       := LbReleaseValue.Width;
   Height      := LbReleaseValue.Height;
   BorderStyle := bsNone;
   Color       := Self.Color;
   Text        := LbReleaseValue.Caption;
   Tag         := 3;
   TabOrder    := 0;
   OnKeyPress  := EdValueKeyPress;
   Font.Assign(Self.Font);
   SetFocus;
  end;
end;

procedure TFmLookaheadLimiter.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 with TLookaheadLimiterDataModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, FEdValue.Text);
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmLookaheadLimiter.UpdateInput;
var
  Input : Single;
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Input := Parameter[0];
   if Input <> DialInput.Value
    then DialInput.Value := Input;
   LbInputValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLookaheadLimiter.UpdateOutput;
var
  Output : Single;
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Output := Parameter[1];
   if Output <> DialOutput.Value
    then DialOutput.Value := Output;
   LbOutputValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmLookaheadLimiter.UpdateProcessingMode;
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   if SbProcessingType.ItemIndex <> Round(Parameter[2])
    then SbProcessingType.ItemIndex := Round(Parameter[2]);
  end;
end;

procedure TFmLookaheadLimiter.UpdateRelease;
var
  Release : Single;
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Release := Parameter[3];
   if Release <> DialRelease.Value
    then DialRelease.Value := Release;
   LbReleaseValue.Caption := ParameterDisplay[3] + ' ' + ParameterLabel[3];
  end;
end;

end.
