unit StkEchoGUI;

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
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule;

type
  TFmStkEcho = class(TForm)
    LbDelay: TLabel;
    SBDelay: TScrollBar;
    LbDelayValue: TLabel;
    LbEffectMix: TLabel;
    LbEffectMixValue: TLabel;
    SbEffectMix: TScrollBar;
    procedure SBDelayChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SbEffectMixChange(Sender: TObject);
  public
    procedure UpdateDelay;
    procedure UpdateEffectMix;
  end;

implementation

uses
  Math, DAV_VSTModuleWithPrograms, StkEchoDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmStkEcho.FormShow(Sender: TObject);
begin
 UpdateDelay;
 UpdateEffectMix;
end;

procedure TFmStkEcho.SBDelayChange(Sender: TObject);
begin
 with TStkEchoModule(Owner) do
  begin
   Parameter[0] := 0.1 * SBDelay.Position;
  end;
end;

procedure TFmStkEcho.SbEffectMixChange(Sender: TObject);
begin
 with TStkEchoModule(Owner) do
  begin
   Parameter[1] := 0.1 * SBEffectMix.Position;
  end;
end;

procedure TFmStkEcho.UpdateDelay;
var
  DelayPos : Integer;
begin
 with TStkEchoModule(Owner) do
  begin
   DelayPos := Round(10 * Parameter[0]);
   if SBDelay.Position <> DelayPos
    then SBDelay.Position := DelayPos;
   LbDelayValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TFmStkEcho.UpdateEffectMix;
var
  EffectMixPos: Integer;
begin
 with TStkEchoModule(Owner) do
  begin
   EffectMixPos := Round(10 * Parameter[1]);
   if SBEffectMix.Position <> EffectMixPos
    then SBEffectMix.Position := EffectMixPos;
   LbEffectMixValue.Caption := FloatToStrF(Parameter[1], ffGeneral, 4, 4) + ' %';
  end;
end;

end.
