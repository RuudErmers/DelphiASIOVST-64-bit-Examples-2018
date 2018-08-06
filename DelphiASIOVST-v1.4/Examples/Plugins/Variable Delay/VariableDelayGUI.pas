unit VariableDelayGUI;

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
  Windows, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_VSTModule,
  DAV_Types;

type
  TVSTGUI = class(TForm)
    SampleBar: TScrollBar;
    LbSamples: TLabel;
    LbDryMixValue: TLabel;
    SBDryMix: TScrollBar;
    LbWetMixValue: TLabel;
    SBWetMix: TScrollBar;
    procedure FormShow(Sender: TObject);
    procedure SampleBarChange(Sender: TObject);
    procedure SBDryMixChange(Sender: TObject);
    procedure SBWetMixChange(Sender: TObject);
  private
  public
    procedure UpdateDelayLength;
    procedure UpdateDryMix;
    procedure UpdateWetMix;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  VariableDelayModule;

procedure TVSTGUI.FormShow(Sender: TObject);
begin
 UpdateDelayLength;
end;

procedure TVSTGUI.SampleBarChange(Sender: TObject);
begin
 with TVariableDelayVST(Owner) do
  begin
   if Parameter[0] <> 0.01 * SampleBar.Position
    then Parameter[0] := 0.01 * SampleBar.Position;
  end;
end;

procedure TVSTGUI.SBDryMixChange(Sender: TObject);
begin
 with TVariableDelayVST(Owner) do
  begin
   if Parameter[1] <> 0.1 * SBDryMix.Position
    then Parameter[1] := 0.1 * SBDryMix.Position;
  end;
end;

procedure TVSTGUI.SBWetMixChange(Sender: TObject);
begin
 with TVariableDelayVST(Owner) do
  begin
   if Parameter[2] <> 0.1 * SBWetMix.Position
    then Parameter[2] := 0.1 * SBWetMix.Position;
  end;
end;

procedure TVSTGUI.UpdateDelayLength;
begin
 with TVariableDelayVST(Owner) do
  begin
   if Round(100 * Parameter[0]) <> SampleBar.Position
    then SampleBar.Position := Round(100 * Parameter[0]);
   LbSamples.Caption := 'Delay: ' + IntToStr(Round(Parameter[0])) + ' ms';
  end;
end;

procedure TVSTGUI.UpdateDryMix;
begin
 with TVariableDelayVST(Owner) do
  begin
   if Round(10 * Parameter[1]) <> SBDryMix.Position
    then SBDryMix.Position := Round(10 * Parameter[1]);
   LbDryMixValue.Caption := 'Dry Mix: ' + FloatToStrF(Parameter[1], ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TVSTGUI.UpdateWetMix;
begin
 with TVariableDelayVST(Owner) do
  begin
   if Round(10 * Parameter[2]) <> SBWetMix.Position
    then SBWetMix.Position := Round(10 * Parameter[2]);
   LbWetMixValue.Caption := 'Wet Mix: ' + FloatToStrF(Parameter[2], ffGeneral, 3, 3) + ' %';
  end;
end;

end.
