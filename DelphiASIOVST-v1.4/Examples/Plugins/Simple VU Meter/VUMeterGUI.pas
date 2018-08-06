unit VUMeterGUI;

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
  Forms, Controls, StdCtrls, ExtCtrls, DAV_Types, DAV_VSTModule;

type
  TVSTVUMeterGUI = class(TForm)
    LbAbout: TLabel;
    LbTrademark: TLabel;
    ShVULeft: TShape;
    ShVURight: TShape;
    LbGainLeft: TLabel;
    LbGainRight: TLabel;
    SBLeft: TScrollBar;
    SBRight: TScrollBar;
    Timer: TTimer;
    procedure ParameterChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  VUMeterModule, Dialogs;

procedure TVSTVUMeterGUI.ParameterChange(Sender: TObject);
begin
 with TVSTVUMeterModule(Owner), (Sender as TScrollbar)
  do Parameter[Tag] := Position;
end;

procedure TVSTVUMeterGUI.TimerTimer(Sender: TObject);
var
  tmp : Integer;
begin
  with (Owner As TVSTVUMeterModule) do
  begin
    tmp := Round(300 + 3 * Amp_to_dB(Peak[0]));

    if tmp > 0 then ShVULeft.Width := tmp else ShVULeft.Width := 0;
    tmp := Round(300 + 3 * Amp_to_dB(Peak[1]));

    if tmp>0 then ShVURight.Width := tmp else ShVURight.Width := 0;
    LbGainLeft.Caption := 'left gain: ' + IntToStr(Round(Parameter[0])) + ' db(fs)';
    LbGainRight.Caption := 'right gain: ' + IntToStr(Round(Parameter[1])) + ' db(fs)';
  end;
end;

end.
