unit NoiseGenForm;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2004-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, DAV_Types, DAV_ASIOHost;

type
  TFmASIO = class(TForm)
    ASIOHost: TASIOHost;
    BtStartStop: TButton;
    DriverCombo: TComboBox;
    LbCopyright: TLabel;
    LbDrivername: TLabel;
    LbPanorama: TLabel;
    LbVolume: TLabel;
    SbPan: TScrollBar;
    SbVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure Bt_CPClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure SbPanChange(Sender: TObject);
    procedure SbVolumeChange(Sender: TObject);
  private
    FVol, FPan : Single;  
  end;

var
  FmASIO : TFmASIO;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles, DAV_Common;

procedure TFmASIO.FormCreate(Sender: TObject);
begin
 DriverCombo.Items := ASIOHost.DriverList;
 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'NoiseGen.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);
  finally
   Free;
  end;
end;

procedure TFmASIO.DriverComboChange(Sender: TObject);
begin
 BtStartStop.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'NoiseGen.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   BtStartStop.Enabled := True;
  end;
end;

procedure TFmASIO.Bt_CPClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmASIO.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'NoiseGen.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
  finally
   Free;
  end; 
end;

procedure TFmASIO.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = 'Start Audio' then
  begin
   ASIOHost.Active := True; // Start Audio
   BtStartStop.Caption := 'Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   BtStartStop.Caption := 'Start Audio';
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel, Sample : integer;
begin
 for Channel := 0 to ASIOHost.BufferSize - 1 do
  begin
   for Sample := 0 to ASIOHost.OutputChannelCount - 1
    do OutBuffer[Channel, Sample] := (Random - Random) * FVol;
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch64(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  Channel, Sample : integer;
begin
 for Channel := 0 to ASIOHost.BufferSize - 1 do
  begin
   for Sample := 0 to ASIOHost.OutputChannelCount - 1
    do OutBuffer[Channel, Sample] := (Random - Random) * FVol;
  end;
end;

procedure TFmASIO.SbVolumeChange(Sender: TObject);
begin
 FVol := SbVolume.position * 0.00001;
 if FVol=0
  then LbVolume.Caption := 'Volume: 0 equals -oo dB'
  else LbVolume.Caption := 'Volume: ' +
                           FloatToStrF(FVol, ffFixed, 2, 2) + ' equals ' +
                           FloatToStrF(Amp_to_dB(FVol), ffGeneral, 2, 2) + ' dB';
end;

procedure TFmASIO.SbPanChange(Sender: TObject);
begin
 FPan := SbPan.Position * 0.01;
 if FPan = 0.5
  then LbPanorama.Caption := 'Panorama: C'
  else LbPanorama.Caption := 'Panorama: ' + IntToStr(round(100 * (FPan * 2 - 1)));
end;

{$IFDEF FPC}
initialization
  {$i NoiseGenForm.lrs}
{$ENDIF}

end.

