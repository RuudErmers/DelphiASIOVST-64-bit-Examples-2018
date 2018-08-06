unit JNDEQTaudio;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  DAV_GuiPixelMap, DAV_GuiLabel, DAV_GuiSelectBox, DAV_GuiGraphicControl,
  DAV_GuiCustomControl, DAV_GuiButton;

type
  TFmSetup = class(TForm)
    LbOutputChannels: TGuiLabel;
    LbPreset: TGuiLabel;
    SbChannels: TGuiSelectBox;
    SbDrivers: TGuiSelectBox;
    BtControlPanel: TGuiButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure SbDriversChange(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure SbChannelsChange(Sender: TObject);
  private
    FBackgroundBitmap : TGuiCustomPixelMap;
  end;

var
  FmSetup: TFmSetup;

implementation

uses
  IniFiles, DAV_GuiCommon, JNDEQTmain;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFmSetup.FormCreate(Sender: TObject);
begin
 // create background bitmap
 FBackgroundBitmap := TGuiPixelMapMemory.Create;
 FormResize(Self);

 SbDrivers.Items := FmJNDEQT.ASIOHost.DriverList;
 with TIniFile.Create(FmJNDEQT.IniFile) do
  try
   Top := ReadInteger('Layout', 'Setup Top', Top);
   Left := ReadInteger('Layout', 'Setup Left', Left);
   SbDrivers.ItemIndex := ReadInteger('Setup', 'ASIO Driver', SbDrivers.ItemIndex);
   if SbDrivers.ItemIndex = -1
    then SbDrivers.ItemIndex := FmJNDEQT.AsioHost.DriverList.IndexOf('ASIO4ALL v2');
   SbDriversChange(Self);

   SbChannels.ItemIndex := ReadInteger('Setup', 'Output Channel Pair Index', 0);
   SbChannelsChange(Self);
  finally
   Free;
  end;
end;

procedure TFmSetup.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmJNDEQT.IniFile) do
  try
   WriteInteger('Layout', 'Setup Top', Top);
   WriteInteger('Layout', 'Setup Left', Left);
   WriteInteger('Setup', 'ASIO Driver', SbDrivers.ItemIndex);
  finally
   Free;
  end;

 FreeAndNil(FBackgroundBitmap);
end;

procedure TFmSetup.FormPaint(Sender: TObject);
begin
 if Assigned(FBackgroundBitmap)
  then FBackgroundBitmap.PaintTo(Canvas);
end;

procedure TFmSetup.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 if Assigned(FBackgroundBitmap) then
  with FBackgroundBitmap do
   begin
    SetSize(ClientWidth, ClientHeight);
    s[0] := 0;
    s[1] := 0;
    hr   := 1 / Height;
    for y := 0 to Height - 1 do
     begin
      ScnLn := Scanline[y];
      h     := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
      for x := 0 to Width - 1 do
       begin
        s[1] := 0.97 * s[0] + 0.03 * Random;
        s[0] := s[1];

        ScnLn[x].B := Round($9D - $34 * (s[1] - h));
        ScnLn[x].G := Round($AE - $48 * (s[1] - h));
        ScnLn[x].R := Round($BD - $50 * (s[1] - h));
       end;
     end;
   end;
end;

procedure TFmSetup.BtControlPanelClick(Sender: TObject);
begin
 FmJNDEQT.AsioHost.ControlPanel;
end;

procedure TFmSetup.SbChannelsChange(Sender: TObject);
begin
 FmJNDEQT.OutputChannelOffset := SbChannels.ItemIndex * 2;
end;

procedure TFmSetup.SbDriversChange(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 with FmJNDEQT.ASIOHost do
  if SbDrivers.ItemIndex >= 0 then
   begin
    DriverIndex := SbDrivers.ItemIndex;
    if Assigned(OnReset)
     then OnReset(Self);

    SbChannels.Clear;
    for ChannelIndex := 0 to (FmJNDEQT.ASIOHost.OutputChannelCount div 2) - 1 do
     begin
      SbChannels.Items.Add(string(
        FmJNDEQT.ASIOHost.OutputChannelInfos[2 * ChannelIndex].Name) + ' / ' +
        string(FmJNDEQT.ASIOHost.OutputChannelInfos[2 * ChannelIndex + 1].Name));
     end;
   end;
end;

{$IFDEF FPC}
initialization
  {$i EditorSetup.lrs}
{$ENDIF}

end.

