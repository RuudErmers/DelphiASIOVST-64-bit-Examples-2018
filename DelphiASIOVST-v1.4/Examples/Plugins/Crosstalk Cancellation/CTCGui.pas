unit CTCGui;

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
  Forms, Controls, StdCtrls, ExtCtrls, Graphics, DAV_Types, DAV_VSTModule, 
  DAV_GuiLabel, DAV_GuiPixelMap, DAV_GuiPng, DAV_GuiCheckBox,
  DAV_GuiGraphicControl, DAV_GuiSlider;

type
  TFmCTC = class(TForm)
    Image1: TImage;
    Image2: TImage;
    LbAttenuation: TLabel;
    LbAttenuationValue: TLabel;
    LbFilterFrequency: TLabel;
    LbFilterFrequencyValue: TLabel;
    LbFilterGain: TLabel;
    LbFilterGainValue: TLabel;
    LbFilterType: TLabel;
    LbFilterTypeValue: TLabel;
    LbListenerDistance: TLabel;
    LbListenerDistanceValue: TLabel;
    LbOutputGain: TLabel;
    LbOutputGainValue: TLabel;
    LbRecursionSteps: TLabel;
    LbRecursionStepsValue: TLabel;
    LbSpeakerDistance: TLabel;
    LbSpeakerDistanceValue: TLabel;
    LbSwitches: TLabel;
    LbTitle: TGuiLabel;
    CBBypass: TGuiControlsCheckBox;
    CBAGC: TGuiControlsCheckBox;
    SbSpeakerDistance: TGuiSlider;
    SbListenerDistance: TGuiSlider;
    SbAttenuation: TGuiSlider;
    SbRecursionSteps: TGuiSlider;
    SbFilterFrequency: TGuiSlider;
    SbFilterGain: TGuiSlider;
    SbOutputGain: TGuiSlider;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CBBypassClick(Sender: TObject);
    procedure SbAttenuationChange(Sender: TObject);
    procedure SbFilterFrequencyChange(Sender: TObject);
    procedure SbFilterGainChange(Sender: TObject);
    procedure SbListenerDistanceChange(Sender: TObject);
    procedure SbOutputGainChange(Sender: TObject);
    procedure SbRecursionStepsChange(Sender: TObject);
    procedure SbSpeakerDistanceChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateSpeakerDistance;
    procedure UpdateListenerDistance;
    procedure UpdateRecursionSteps;
    procedure UpdateAttenuation;
    procedure UpdateFilterType;
    procedure UpdateFilterGain;
    procedure UpdateFilterFrequency;
    procedure UpdateOutputGain;
    procedure UpdateBypass;
    procedure UpdateAGC;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} DAV_Common,
  DAV_GuiCommon, DAV_VSTModuleWithPrograms, CTCDM;

procedure TFmCTC.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmCTC.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmCTC.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmCTC.FormResize(Sender: TObject);
var
  RS     : TResourceStream;
  Png32  : TPortableNetworkGraphicPixel32;
  PM     : TGuiCustomPixelMap;
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
     h    := 0.3 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * Random;
       s[0] := s[1];

       ScnLn[x].B := Round($C8 - $3A * (s[1] - h));
       ScnLn[x].G := Round($D0 - $3C * (s[1] - h));
       ScnLn[x].R := Round($D4 - $40 * (s[1] - h));
      end;
    end;
  end;

 Png32 := TPortableNetworkGraphicPixel32.Create;
 try
  PM := TGuiPixelMapMemory.Create;
  try
   RS := TResourceStream.Create(hInstance, 'Left', 'PNG');
   try
    Png32.LoadFromStream(RS);
    PM.Assign(Png32);
    FBackground.Draw(PM, 8, 8);
   finally
    RS.Free;
   end;

   RS := TResourceStream.Create(hInstance, 'Right', 'PNG');
   try
    Png32.LoadFromStream(RS);
    PM.Assign(Png32);
    FBackground.Draw(PM, Width - Png32.Width - 8, 8);
   finally
    RS.Free;
   end;
  finally
   FreeAndNil(PM);
  end;
 finally
  FreeAndNil(Png32);
 end;
end;

procedure TFmCTC.FormShow(Sender: TObject);
begin
 UpdateSpeakerDistance;
 UpdateListenerDistance;
 UpdateAttenuation;
 UpdateRecursionSteps;
 UpdateFilterType;
 UpdateFilterGain;
 UpdateFilterFrequency;
 UpdateOutputGain;
 LbTitle.Height := LbTitle.Height + 1;
end;

procedure TFmCTC.SbSpeakerDistanceChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[0] := 0.1 * SbSpeakerDistance.Value;
  end;
end;

procedure TFmCTC.SbListenerDistanceChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[1] := 0.1 * SbListenerDistance.Value;
  end;
end;

procedure TFmCTC.SbOutputGainChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[7] := 0.1 * SbOutputGain.Value;
  end;
end;

procedure TFmCTC.SbRecursionStepsChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[2] := SbRecursionSteps.Value;
  end;
end;

procedure TFmCTC.SbAttenuationChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[3] := 0.1 * SbAttenuation.Value;
  end;
end;

procedure TFmCTC.SbFilterFrequencyChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[5] := FreqLinearToLog(0.0001 * SbFilterFrequency.Value);
  end;
end;

procedure TFmCTC.SbFilterGainChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[6] := 0.1 * SbFilterGain.Value;
  end;
end;

procedure TFmCTC.CBBypassClick(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[8] := Integer(CBBypass.Checked);
  end;
end;

procedure TFmCTC.UpdateSpeakerDistance;
begin
 with TCTCDataModule(Owner) do
  begin
   if Round(10 * Parameter[0]) <> SbSpeakerDistance.Value
    then SbSpeakerDistance.Value := Round(10 * Parameter[0]);
   LbSpeakerDistanceValue.Caption := string(ParameterDisplay[0] + ' ' + ParameterLabel[0]);
  end;
end;

procedure TFmCTC.UpdateListenerDistance;
begin
 with TCTCDataModule(Owner) do
  begin
   if Round(10 * Parameter[1]) <> SbListenerDistance.Value
    then SbListenerDistance.Value := Round(10 * Parameter[1]);
   LbListenerDistanceValue.Caption := string(ParameterDisplay[1] + ' ' + ParameterLabel[1]);
  end;
end;

procedure TFmCTC.UpdateRecursionSteps;
begin
 with TCTCDataModule(Owner) do
  begin
   if Round(Parameter[2]) <> SbRecursionSteps.Value
    then SbRecursionSteps.Value := Round(Parameter[2]);
   LbRecursionStepsValue.Caption := IntToStr(Round(Parameter[2]));
  end;
end;

procedure TFmCTC.UpdateAGC;
begin

end;

procedure TFmCTC.UpdateAttenuation;
begin
 with TCTCDataModule(Owner) do
  begin
   if Round(10 * Parameter[3]) <> SbAttenuation.Value
    then SbAttenuation.Value := Round(10 * Parameter[3]);
   LbAttenuationValue.Caption := string(ParameterDisplay[3] + ' dB');
  end;
end;

procedure TFmCTC.UpdateBypass;
begin
 with TCTCDataModule(Owner)
  do CBBypass.Checked := Boolean(Round(Parameter[8]));
end;

procedure TFmCTC.UpdateFilterType;
begin
 with TCTCDataModule(Owner) do
  begin
   LbFilterTypeValue.Caption := 'Simple Highshelf';
  end;
end;

procedure TFmCTC.UpdateFilterFrequency;
begin
 with TCTCDataModule(Owner) do
  begin
   if Round(10000 * FreqLogToLinear(Parameter[5])) <> SbFilterFrequency.Value
    then SbFilterFrequency.Value := Round(10000 * FreqLogToLinear(Parameter[5]));
   LbFilterFrequencyValue.Caption := string(ParameterDisplay[5] + ' ' + ParameterLabel[5]);
  end;
end;

procedure TFmCTC.UpdateFilterGain;
begin
 with TCTCDataModule(Owner) do
  begin
   if Round(10 * Parameter[6]) <> SbFilterGain.Value
    then SbFilterGain.Value := Round(10 * Parameter[6]);
   LbFilterGainValue.Caption := string(ParameterDisplay[6] + ' dB');
  end;
end;

procedure TFmCTC.UpdateOutputGain;
begin
 with TCTCDataModule(Owner) do
  begin
   if Round(10 * Parameter[7]) <> SbOutputGain.Value
    then SbOutputGain.Value := Round(10 * Parameter[7]);
   LbOutputGainValue.Caption := string(ParameterDisplay[7] + ' dB');
  end;
end;

end.
