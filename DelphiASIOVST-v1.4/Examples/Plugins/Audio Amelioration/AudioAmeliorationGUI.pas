unit AudioAmeliorationGUI;

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
  Forms, DAV_Types, DAV_VSTModule, TeEngine, Series, Controls, ExtCtrls, 
  TeeProcs, Chart, StdCtrls, ComCtrls;

type
  TFmAudioAmelioration = class(TForm)
    Cb3DSound: TCheckBox;
    CbAmbience: TCheckBox;
    CbCompressor: TCheckBox;
    CbExciter: TCheckBox;
    CbExtrabass: TCheckBox;
    CbPower: TCheckBox;
    RbHeadphones: TRadioButton;
    RbSpeaker: TRadioButton;
    SpectrumAnalyser: TChart;
    SpectrumBars: TBarSeries;
    Tb3DSurround: TTrackBar;
    TbAmbience: TTrackBar;
    TbCompressor: TTrackBar;
    TbExciter: TTrackBar;
    TbExtraBass: TTrackBar;
    LbSetup: TLabel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure CbAmbienceClick(Sender: TObject);
    procedure TbAmbienceChange(Sender: TObject);
    procedure CbPowerClick(Sender: TObject);
    procedure CbExciterClick(Sender: TObject);
    procedure Cb3DSoundClick(Sender: TObject);
    procedure CbCompressorClick(Sender: TObject);
    procedure CbExtrabassClick(Sender: TObject);
    procedure TbCompressorChange(Sender: TObject);
    procedure TbExciterChange(Sender: TObject);
    procedure Tb3DSurroundChange(Sender: TObject);
    procedure TbExtraBassChange(Sender: TObject);
    procedure RbSpeakerHeadphoneClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  public
    procedure Update3DSurround;
    procedure Update3DSurroundActive;
    procedure UpdateAmbience;
    procedure UpdateAmbienceActive;
    procedure UpdateCompressor;
    procedure UpdateCompressorActive;
    procedure UpdateExciter;
    procedure UpdateExciterActive;
    procedure UpdateExtraBass;
    procedure UpdateExtraBassActive;
    procedure UpdatePower;
    procedure UpdateSpeaker;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  AudioAmeliorationDM, DAV_VSTModuleWithPrograms;

{ TFmAudioAmelioration }

procedure TFmAudioAmelioration.FormCreate(Sender: TObject);
var
  Band : Integer;
begin
 Update3DSurround;
 UpdateAmbience;
 UpdateCompressor;
 UpdateExciter;
 UpdateExtraBass;
 UpdatePower;

 // add bands
 for Band := 0 to CNumFrequencies - 1 do
  begin
   {$IFNDEF FPC}
   if CFrequencyArray[Band] < 1000
    then SpectrumBars.Add(0, FloatToStr(CFrequencyArray[Band]) + ' Hz')
    else SpectrumBars.Add(0, FloatToStr(0.001 * CFrequencyArray[Band]) + ' kHz');
   {$ELSE}
   if CFrequencyArray[Band] < 1000
    then SpectrumAnalyser.AddBar(FloatToStr(CFrequencyArray[Band]) + ' Hz', 0, $000000FF)
    else SpectrumAnalyser.AddBar(FloatToStr(0.001 * CFrequencyArray[Band]) + ' kHz', 0, $000000FF);
   {$ENDIF}
  end;
end;

procedure TFmAudioAmelioration.RbSpeakerHeadphoneClick(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[11] <> Integer(RbSpeaker.Checked)
    then Parameter[11] := Integer(RbSpeaker.Checked);
  end;
end;

procedure TFmAudioAmelioration.CbPowerClick(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[0] <> Integer(CbPower.Checked)
    then Parameter[0] := Integer(CbPower.Checked);
  end;
end;

procedure TFmAudioAmelioration.CbExciterClick(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[1] <> Integer(CbExciter.Checked)
    then Parameter[1] := Integer(CbExciter.Checked);
  end;
end;

procedure TFmAudioAmelioration.CbAmbienceClick(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[2] <> Integer(CbAmbience.Checked)
    then Parameter[2] := Integer(CbAmbience.Checked);
  end;
end;

procedure TFmAudioAmelioration.Cb3DSoundClick(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[3] <> Integer(Cb3DSound.Checked)
    then Parameter[3] := Integer(Cb3DSound.Checked);
  end;
end;

procedure TFmAudioAmelioration.CbCompressorClick(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[4] <> Integer(CbCompressor.Checked)
    then Parameter[4] := Integer(CbCompressor.Checked);
  end;
end;

procedure TFmAudioAmelioration.CbExtrabassClick(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[5] <> Integer(CbExtrabass.Checked)
    then Parameter[5] := Integer(CbExtrabass.Checked);
  end;
end;

procedure TFmAudioAmelioration.TbExciterChange(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[6] <> TbExciter.Position * 10
    then Parameter[6] := TbExciter.Position * 10;
  end;
end;

procedure TFmAudioAmelioration.TbAmbienceChange(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[7] <> TbAmbience.Position * 10
    then Parameter[7] := TbAmbience.Position * 10;
  end;
end;

procedure TFmAudioAmelioration.Tb3DSurroundChange(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[8] <> Tb3DSurround.Position * 10
    then Parameter[8] := Tb3DSurround.Position * 10;
  end;
end;

procedure TFmAudioAmelioration.TbCompressorChange(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[9] <> TbCompressor.Position * 10
    then Parameter[9] := TbCompressor.Position * 10;
  end;
end;

procedure TFmAudioAmelioration.TbExtraBassChange(Sender: TObject);
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Parameter[10] <> TbExtraBass.Position * 10
    then Parameter[10] := TbExtraBass.Position * 10;
  end;
end;

procedure TFmAudioAmelioration.TimerTimer(Sender: TObject);
var
  Band : Integer;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   {$IFNDEF FPC}
   for Band := 0 to cNumFrequencies - 1
    do SpectrumBars.YValue[Band] := BandRMS[cNumFrequencies - Band - 1];
   SpectrumAnalyser.Invalidate;
   {$ELSE}
   for j := 0 to cNumFrequencies - 1
    do TBar(SpectrumAnalyser.Bars.Items[Band]).Value := Round(FFilterArray[cNumFrequencies - Band - 1].RMS + FFSGain);
   SpectrumAnalyser.Invalidate;
   {$ENDIF}
  end;
end;

procedure TFmAudioAmelioration.UpdatePower;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if CbPower.Checked <> PowerActive
    then CbPower.Checked := PowerActive;
  end;
end;

procedure TFmAudioAmelioration.UpdateSpeaker;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if RbSpeaker.Checked <> IsSpeaker
    then RbSpeaker.Checked := IsSpeaker;
   RbHeadphones.Checked := not RbSpeaker.Checked;
  end;
end;

procedure TFmAudioAmelioration.UpdateExciter;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if TbExciter.Position <> Round(0.1 * Parameter[6])
    then TbExciter.Position := Round(0.1 * Parameter[6]);
  end;
end;

procedure TFmAudioAmelioration.UpdateExciterActive;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if CbExciter.Checked <> ExciterActive
    then CbExciter.Checked := ExciterActive;
  end;
end;

procedure TFmAudioAmelioration.UpdateAmbience;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if TbAmbience.Position <> Round(0.1 * Parameter[7])
    then TbAmbience.Position := Round(0.1 * Parameter[7]);
  end;
end;

procedure TFmAudioAmelioration.UpdateAmbienceActive;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if CbAmbience.Checked <> AmbienceActive
    then CbAmbience.Checked := AmbienceActive;
  end;
end;

procedure TFmAudioAmelioration.Update3DSurround;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Tb3DSurround.Position <> Round(0.1 * Parameter[8])
    then Tb3DSurround.Position := Round(0.1 * Parameter[8]);
  end;
end;

procedure TFmAudioAmelioration.Update3DSurroundActive;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if Cb3DSound.Checked <> SourroundActive
    then Cb3DSound.Checked := SourroundActive;
  end;
end;

procedure TFmAudioAmelioration.UpdateCompressor;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if TbCompressor.Position <> Round(0.1 * Parameter[9])
    then TbCompressor.Position := Round(0.1 * Parameter[9]);
  end;
end;

procedure TFmAudioAmelioration.UpdateCompressorActive;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if CbCompressor.Checked <> CompressorActive
    then CbCompressor.Checked := CompressorActive;
  end;
end;

procedure TFmAudioAmelioration.UpdateExtraBass;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if TbExtraBass.Position <> Round(0.1 * Parameter[10])
    then TbExtraBass.Position := Round(0.1 * Parameter[10]);
  end;
end;

procedure TFmAudioAmelioration.UpdateExtraBassActive;
begin
 with TAudioAmeliorationModule(Owner) do
  begin
   if CbExtraBass.Checked <> ExtraBassActive
    then CbExtraBass.Checked := ExtraBassActive;
  end;
end;

end.
