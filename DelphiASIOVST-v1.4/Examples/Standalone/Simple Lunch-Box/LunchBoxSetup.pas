unit LunchBoxSetup;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LMessages, LResources, Buttons,
  {$ELSE} Windows, Messages,{$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls, Spin;

type
  TFmSetup = class(TForm)
    LbPreset: TLabel;
    LbOutput: TLabel;
    CBDrivers: TComboBox;
    CBOutput: TComboBox;
    BtControlPanel: TButton;
    LbPlaybackSampleRate: TLabel;
    SESampleRate: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure CBDriversChange(Sender: TObject);
    procedure CBInputChange(Sender: TObject);
    procedure CBOutputChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure SESampleRateChange(Sender: TObject);
  private
  public
  end;

var
  FmSetup: TFmSetup;

implementation

uses
  IniFiles, DAV_ASIOHost, LunchBoxMain;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmSetup.FormCreate(Sender: TObject);
var Settings : TInifile;
begin
 CBDrivers.Items:=FmLunchBox.ASIOHost.DriverList;
 Settings:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI');
 Top:=Settings.ReadInteger('Layout','Setup Top',Top);
 Left:=Settings.ReadInteger('Layout','Setup Left',Left);
 CBDrivers.ItemIndex:=Settings.ReadInteger('Setup','ASIO Driver',CBDrivers.ItemIndex);
 CBDriversChange(Self);
 Settings.Free;
end;

procedure TFmSetup.BtControlPanelClick(Sender: TObject);
begin
 FmLunchBox.ASIOHost.ControlPanel;
end;

procedure TFmSetup.CBDriversChange(Sender: TObject);
var i : Integer;
begin
 if CBDrivers.ItemIndex >= 0 then
  with FmLunchBox.ASIOHost do
   begin
    Active:=False;
    DriverIndex := CBDrivers.ItemIndex;
    CBOutput.Clear;
    for i := 0 to (OutputChannelCount div 2) - 1 do
     begin
      CBOutput.Items.Add(
      OutputChannelInfos[2 * i].name + ' / ' +
      OutputChannelInfos[2 * i + 1].name);
     end;
    CBOutput.ItemIndex := 0;
    SESampleRate.Value:=Round(Samplerate);
    OnReset(Self);
    Active:=True;
   end;
end;

procedure TFmSetup.CBInputChange(Sender: TObject);
begin
// FmVSTEditor.ASIOHost.InputChannels:=CBInput.ItemIndex*2;
end;

procedure TFmSetup.CBOutputChange(Sender: TObject);
begin
// FmVSTEditor.ASIOHost.OutputChannels:=CBOutput.ItemIndex*2;
end;

procedure TFmSetup.FormDestroy(Sender: TObject);
var Settings : TInifile;
begin
 Settings:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI');
 Settings.WriteInteger('Layout','Setup Top',Top);
 Settings.WriteInteger('Layout','Setup Left',Left);
 Settings.WriteInteger('Setup','ASIO Driver',CBDrivers.ItemIndex);
 Settings.Free;
end;

procedure TFmSetup.SESampleRateChange(Sender: TObject);
var i : Integer;
begin
 for i:=0 to FmLunchBox.EventList.Count-1 do
  with FmLunchBox.EventList[i] do
   begin
    SampleRate:=sqr(FmLunchBox.ASIOHost.SampleRate)/SESampleRate.Value;
    Frequency:=Samples[SampleIndex].SampleRate/SampleRate;
   end;
end;

end.
