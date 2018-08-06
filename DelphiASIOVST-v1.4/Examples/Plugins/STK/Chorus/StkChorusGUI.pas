unit StkChorusGUI;

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
  TFmStkChorus = class(TForm)
    LbModDepth: TLabel;
    SBModDepth: TScrollBar;
    LbModDepthValue: TLabel;
    LbModFreq: TLabel;
    LbModFreqValue: TLabel;
    SBModFreq: TScrollBar;
    LbEffectMix: TLabel;
    LbEffectMixValue: TLabel;
    SbEffectMix: TScrollBar;
    procedure SBModDepthChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBModFreqChange(Sender: TObject);
    procedure SbEffectMixChange(Sender: TObject);
  public
    procedure UpdateModDepth;
    procedure UpdateModFreq;
    procedure UpdateEffectMix;
  end;

implementation

uses
  Math, DAV_VSTModuleWithPrograms, StkChorusDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmStkChorus.FormShow(Sender: TObject);
begin
 UpdateModDepth;
 UpdateModFreq;
 UpdateEffectMix;
end;

procedure TFmStkChorus.SBModDepthChange(Sender: TObject);
begin
 with TStkChorusModule(Owner) do
  begin
   Parameter[0] := 0.1 * SBModDepth.Position;
  end;
end;

procedure TFmStkChorus.SBModFreqChange(Sender: TObject);
begin
 with TStkChorusModule(Owner) do
  begin
   Parameter[1] := 0.01 * SBModFreq.Position;
  end;
end;

procedure TFmStkChorus.SbEffectMixChange(Sender: TObject);
begin
 with TStkChorusModule(Owner) do
  begin
   Parameter[2] := 0.1 * SBEffectMix.Position;
  end;
end;

procedure TFmStkChorus.UpdateModDepth;
var
  ModDepthPos : Integer;
begin
 with TStkChorusModule(Owner) do
  begin
   ModDepthPos := Round(10 * Parameter[0]);
   if SBModDepth.Position <> ModDepthPos
    then SBModDepth.Position := ModDepthPos;
   LbModDepthValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TFmStkChorus.UpdateModFreq;
var
  ModFreqPos : Integer;
begin
 with TStkChorusModule(Owner) do
  begin
   ModFreqPos := Round(100 * Parameter[1]);
   if SBModFreq.Position <> ModFreqPos
    then SBModFreq.Position := ModFreqPos;
   LbModFreqValue.Caption := FloatToStrF(Parameter[1], ffGeneral, 4, 4) + ' Hz';
  end;
end;

procedure TFmStkChorus.UpdateEffectMix;
var
  EffectMixPos: Integer;
begin
 with TStkChorusModule(Owner) do
  begin
   EffectMixPos := Round(10 * Parameter[2]);
   if SBEffectMix.Position <> EffectMixPos
    then SBEffectMix.Position := EffectMixPos;
   LbEffectMixValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 4) + ' %';
  end;
end;

end.
