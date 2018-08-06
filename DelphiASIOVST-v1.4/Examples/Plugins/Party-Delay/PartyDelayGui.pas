unit PartyDelayGui;

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
  Forms, Graphics, Controls, StdCtrls, ComCtrls, DAV_Types, DAV_VSTModule,
  DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmPartyDelay = class(TForm)
    CbActive: TCheckBox;
    CbFilterType: TComboBox;
    CbInvert: TCheckBox;
    DialFreqShift: TGuiDial;
    LbBalance: TLabel;
    LbBandwidth: TLabel;
    LbDelay: TLabel;
    LbDrive: TLabel;
    LbDry: TLabel;
    LbFeedback: TLabel;
    LbFilterType: TLabel;
    LbFreqShift: TLabel;
    LbFrequency: TLabel;
    LbGain: TLabel;
    LbLevel: TLabel;
    LbMix: TLabel;
    LbPan: TLabel;
    LbWet: TLabel;
    SbBalance: TScrollBar;
    SbDelay: TScrollBar;
    SbDrive: TScrollBar;
    SbFeedback: TScrollBar;
    SbFilterBW: TScrollBar;
    SbFilterGain: TScrollBar;
    SbFreqShift: TScrollBar;
    SbFrequency: TScrollBar;
    SbLevel: TScrollBar;
    SbMix: TScrollBar;
    SbPan: TScrollBar;
    StatusBar: TStatusBar;
    TC: TTabControl;
    procedure CbActiveClick(Sender: TObject);
    procedure CbInvertClick(Sender: TObject);
    procedure SbPanChange(Sender: TObject);
    procedure SbLevelChange(Sender: TObject);
    procedure SbDelayChange(Sender: TObject);
    procedure SbFeedbackChange(Sender: TObject);
    procedure CbFilterTypeChange(Sender: TObject);
    procedure SbFrequencyChange(Sender: TObject);
    procedure SbFilterGainChange(Sender: TObject);
    procedure SbFilterBWChange(Sender: TObject);
    procedure DialFreqShiftChange(Sender: TObject);
    procedure SbFreqShiftChange(Sender: TObject);
    procedure SbDriveChange(Sender: TObject);
    procedure SbBalanceChange(Sender: TObject);
    procedure TCChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SbMixChange(Sender: TObject);
  public
    procedure UpdateAll;
    procedure UpdateActive;
    procedure UpdateGain;
    procedure UpdatePan;
    procedure UpdateInvert;
    procedure UpdateDelay;
    procedure UpdateFeedback;
    procedure UpdateFilterType;
    procedure UpdateFilterFrequency;
    procedure UpdateFilterGain;
    procedure UpdateFilterBandwidth;
    procedure UpdateFrequencyShifter;
    procedure UpdateShiftFrequency;
    procedure UpdateDrive;
    procedure UpdateBalance;
    procedure UpdateMix;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, PartyDelayDM;

procedure TFmPartyDelay.FormShow(Sender: TObject);
begin
 UpdateAll;
end;

procedure TFmPartyDelay.TCChange(Sender: TObject);
begin
 UpdateAll;
end;

procedure TFmPartyDelay.CbActiveClick(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 0;
   if Parameter[ParamNo] <> Integer(CbActive.Checked)
    then Parameter[ParamNo] := Integer(CbActive.Checked);
  end;
end;

procedure TFmPartyDelay.SbPanChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 1;
   if Parameter[ParamNo] <> 0.1 * SbPan.Position
    then Parameter[ParamNo] := 0.1 * SbPan.Position;
  end;
end;

procedure TFmPartyDelay.SbLevelChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 2;
   if Parameter[ParamNo] <> 0.1 * SbLevel.Position
    then Parameter[ParamNo] := 0.1 * SbLevel.Position;
  end;
end;

procedure TFmPartyDelay.SbMixChange(Sender: TObject);
begin
 with TPartyDelayDataModule(Owner) do
  begin
   if Parameter[60] <> 0.1 * SbMix.Position
    then Parameter[60] := 0.1 * SbMix.Position;
  end;
end;

procedure TFmPartyDelay.CbInvertClick(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 3;
   if Parameter[ParamNo] <> Integer(CbActive.Checked)
    then Parameter[ParamNo] := Integer(CbActive.Checked);
  end;
end;

procedure TFmPartyDelay.SbDelayChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 4;
   if Parameter[ParamNo] <> ScaleLinearToLog(SbDelay.Position * 0.0001, 1, 2000)
    then Parameter[ParamNo] := ScaleLinearToLog(SbDelay.Position * 0.0001, 1, 2000);
  end;
end;

procedure TFmPartyDelay.SbFeedbackChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 5;
   if Parameter[ParamNo] <> 0.1 * SbFeedback.Position
    then Parameter[ParamNo] := 0.1 * SbFeedback.Position;
  end;
end;

procedure TFmPartyDelay.CbFilterTypeChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 6;
   if Parameter[ParamNo] <> CbFilterType.ItemIndex
    then Parameter[ParamNo] := CbFilterType.ItemIndex;
  end;
end;

procedure TFmPartyDelay.SbFrequencyChange(Sender: TObject);
var
  ParamNo : Integer;
  FreqLin : Single;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 7;
   FreqLin := FreqLinearToLog(0.0001 * SbFrequency.Position);
   if Parameter[ParamNo] <> FreqLin
    then Parameter[ParamNo] := FreqLin;
  end;
end;

procedure TFmPartyDelay.SbFilterGainChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 8;
   if Parameter[ParamNo] <> 0.1 * SbFilterGain.Position
    then Parameter[ParamNo] := 0.1 * SbFilterGain.Position;
  end;
end;

procedure TFmPartyDelay.SbFilterBWChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 9;
   if Parameter[ParamNo] <> ScaleLinearToLog(SbFilterBW.Position * 0.0001, 0.1, 10)
    then Parameter[ParamNo] := ScaleLinearToLog(SbFilterBW.Position * 0.0001, 0.1, 10);
  end;
end;

procedure TFmPartyDelay.DialFreqShiftChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 10;
   if Parameter[ParamNo] <> DialFreqShift.Position
    then Parameter[ParamNo] := DialFreqShift.Position;
  end;
end;

procedure TFmPartyDelay.SbFreqShiftChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 11;
   if Parameter[ParamNo] <> ScaleLinearToLog(SbFreqShift.Position * 0.0001, 0.001, 100)
    then Parameter[ParamNo] := ScaleLinearToLog(SbFreqShift.Position * 0.0001, 0.001, 100);
  end;
end;

procedure TFmPartyDelay.SbDriveChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 13;
   if Parameter[ParamNo] <> 0.1 * SbDrive.Position
    then Parameter[ParamNo] := 0.1 * SbDrive.Position;
  end;
end;

procedure TFmPartyDelay.SbBalanceChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 14;
   if Parameter[ParamNo] <> 0.1 * SbBalance.Position
    then Parameter[ParamNo] := 0.1 * SbBalance.Position;
  end;
end;


procedure TFmPartyDelay.UpdateAll;
begin
 UpdateActive;
 UpdateGain;
 UpdatePan;
 UpdateInvert;
 UpdateDelay;
 UpdateFeedback;
 UpdateFilterType;
 UpdateFilterFrequency;
 UpdateFilterGain;
 UpdateFilterBandwidth;
 UpdateFrequencyShifter;
 UpdateShiftFrequency;
 UpdateDrive;
 UpdateBalance;
end;

procedure TFmPartyDelay.UpdateActive;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 0;
   if CbActive.Checked <> (Parameter[ParamNo] > 0.5)
    then CbActive.Checked := Parameter[ParamNo] > 0.5;
  end;
end;

procedure TFmPartyDelay.UpdatePan;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 1;
   if SbPan.Position <> Round(10 * Parameter[ParamNo])
    then SbPan.Position := Round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateGain;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 2;
   if SbLevel.Position <> Round(10 * Parameter[ParamNo])
    then SbLevel.Position := Round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateInvert;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 3;
   if CbInvert.Checked <> (Parameter[ParamNo] > 0.5)
    then CbInvert.Checked := Parameter[ParamNo] > 0.5;
  end;
end;

procedure TFmPartyDelay.UpdateDelay;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 4;
   if SbDelay.Position <> Round(10000 * ScaleLogToLinear(Parameter[ParamNo], 1, 2000))
    then SbDelay.Position := Round(10000 * ScaleLogToLinear(Parameter[ParamNo], 1, 2000));
  end;
end;

procedure TFmPartyDelay.UpdateFeedback;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 5;
   if SbFeedback.Position <> Round(10 * Parameter[ParamNo])
    then SbFeedback.Position := Round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateFilterType;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 6;
   if CbFilterType.ItemIndex <> Round(Parameter[ParamNo])
    then CbFilterType.ItemIndex := Round(Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateFilterFrequency;
var
  ParamNo : Integer;
  FreqLog : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 7;
   FreqLog := Round(10000 * FreqLogToLinear(Parameter[ParamNo]));
   if SbFrequency.Position <> FreqLog
    then SbFrequency.Position := FreqLog;
  end;
end;

procedure TFmPartyDelay.UpdateFilterGain;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 8;
   if SbFilterGain.Position <> Round(10 * Parameter[ParamNo])
    then SbFilterGain.Position := Round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateFilterBandwidth;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 9;
   if SbFilterBW.Position <> Round(10000 * ScaleLogToLinear(Parameter[ParamNo], 0.1, 10))
    then SbFilterBW.Position := Round(10000 * ScaleLogToLinear(Parameter[ParamNo], 0.1, 10));
  end;
end;

procedure TFmPartyDelay.UpdateFrequencyShifter;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 10;
   if DialFreqShift.Position <> Parameter[ParamNo]
    then DialFreqShift.Position := Parameter[ParamNo];
  end;
end;

procedure TFmPartyDelay.UpdateShiftFrequency;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 11;
   if SbFreqShift.Position <> Round(10000 * ScaleLogToLinear(Parameter[ParamNo], 0.001, 100))
    then SbFreqShift.Position := Round(10000 * ScaleLogToLinear(Parameter[ParamNo], 0.001, 100));
  end;
end;

procedure TFmPartyDelay.UpdateDrive;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 13;
   if SbDrive.Position <> Round(10 * Parameter[ParamNo])
    then SbDrive.Position := Round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateBalance;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 14;
   if SbBalance.Position <> Round(10 * Parameter[ParamNo])
    then SbBalance.Position := Round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateMix;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   if SbMix.Position <> Round(10 * Parameter[60])
    then SbMix.Position := Round(10 * Parameter[60]);
  end;
end;

end.
