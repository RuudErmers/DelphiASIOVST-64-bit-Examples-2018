unit DAV_AsioChannelStrip;
{If this file makes troubles, delete the DEFINE ASIOMixer in DASIOHost}

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
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TFrChannelStrip = class(TFrame)
    TrackBar: TTrackBar;
    CBMute: TCheckBox;
    procedure TrackBarChange(Sender: TObject);
    procedure CBMuteClick(Sender: TObject);
  private
    FChannel        : Integer;
    FMute           : Boolean;
    FOnMuteChange   : TNotifyEvent;
    FOnSoloChange   : TNotifyEvent;
    FOnVolumeChange : TNotifyEvent;
    FSolo           : Boolean;
    FVolume         : Single;
  public
    property Channel: Integer read FChannel write FChannel default -1;
    property Mute: Boolean read FMute Write FMute;
    property OnMuteChange: TNotifyEvent read FOnMuteChange write FOnMuteChange;
    property OnSoloChange: TNotifyEvent read FOnSoloChange write FOnSoloChange;
    property OnVolumeChange: TNotifyEvent read FOnVolumeChange write FOnVolumeChange;
    property Solo: Boolean read FSolo Write FSolo;
    property Volume: Single read FVolume Write FVolume;
  end;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

procedure TFrChannelStrip.TrackBarChange(Sender: TObject);
begin
 FVolume := (TrackBar.Max - TrackBar.Position) / TrackBar.Max;
 if Assigned(FOnVolumeChange) then FOnVolumeChange(Self);
 TrackBar.Hint := IntToStr(round(FVolume * 100)) + '%';
end;

procedure TFrChannelStrip.CBMuteClick(Sender: TObject);
begin
 FMute := CBMute.Checked;
 if Assigned(FOnMuteChange) then FOnMuteChange(Self);
 if FMute
  then CBMute.Hint := 'Mute'
  else CBMute.Hint := '';
end;

end.
