unit BarberpoleFlangerGUI;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, Controls, DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel,
  DAV_GuiSelectBox, DAV_GuiStitchedControls, DAV_GuiStitchedPngList,
  DAV_GuiStitchedDial, DAV_GuiImageControl, DAV_GuiCustomControl,
  DAV_GuiGraphicControl;

type
  TFmBarberpoleFlanger = class(TForm)
    DialDepth: TGuiStitchedDial;
    DialMix: TGuiStitchedDial;
    DialSpeed: TGuiStitchedDial;
    DialStages: TGuiStitchedDial;
    DSIL: TGuiStitchedPNGList;
    LbAlgorithm: TGuiLabel;
    LbDepth: TGuiLabel;
    LbDepthValue: TGuiLabel;
    LbMix: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbSpeed: TGuiLabel;
    LbSpeedValue: TGuiLabel;
    LbStages: TGuiLabel;
    LbStagesValue: TGuiLabel;
    SBAlgorithm: TGuiSelectBox;
    procedure FormShow(Sender: TObject);
    procedure DialSpeedChange(Sender: TObject);
    procedure DialStagesChange(Sender: TObject);
    procedure DialDepthChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure SBAlgorithmChange(Sender: TObject);
  public
    procedure UpdateAlgorithm;
    procedure UpdateDepth;
    procedure UpdateMix;
    procedure UpdateSpeed;
    procedure UpdateStages;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  Math, DAV_VSTModuleWithPrograms, BarberpoleFlangerDM;

procedure TFmBarberpoleFlanger.FormShow(Sender: TObject);
begin
 UpdateAlgorithm;
 UpdateDepth;
 UpdateMix;
 UpdateSpeed;
 UpdateStages;
end;

procedure TFmBarberpoleFlanger.SBAlgorithmChange(Sender: TObject);
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   if Round(Parameter[4]) <> SBAlgorithm.ItemIndex
    then Parameter[4] := SBAlgorithm.ItemIndex;
  end;
end;

procedure TFmBarberpoleFlanger.DialStagesChange(Sender: TObject);
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   if Parameter[0] <> DialStages.Value
    then Parameter[0] := DialStages.Value;
  end;
end;

procedure TFmBarberpoleFlanger.DialSpeedChange(Sender: TObject);
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   if Parameter[1] <> DialSpeed.Value
    then Parameter[1] := DialSpeed.Value;
  end;
end;

procedure TFmBarberpoleFlanger.DialDepthChange(Sender: TObject);
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   if Parameter[2] <> DialDepth.Value
    then Parameter[2] := DialDepth.Value;
  end;
end;

procedure TFmBarberpoleFlanger.DialMixChange(Sender: TObject);
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   if Parameter[3] <> DialMix.Value
    then Parameter[3] := DialMix.Value;
  end;
end;

procedure TFmBarberpoleFlanger.UpdateDepth;
var
  Depth : Single;
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   Depth := Parameter[2];
   if DialDepth.Value <> Depth
    then DialDepth.Value := Depth;
   LbDepthValue.Caption := FloatToStrF(RoundTo(Depth, -1), ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmBarberpoleFlanger.UpdateMix;
var
  Mix : Single;
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   Mix := Parameter[3];
   if DialMix.Value <> Mix
    then DialMix.Value := Mix;
   LbMixValue.Caption := FloatToStrF(RoundTo(Mix, -1), ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmBarberpoleFlanger.UpdateSpeed;
var
  Speed : Single;
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   Speed := Parameter[1];
   if DialSpeed.Value <> Speed
    then DialSpeed.Value := Speed;
   LbSpeedValue.Caption := FloatToStrF(RoundTo(Speed, -2), ffGeneral, 2, 2) + ' Hz';
  end;
end;

procedure TFmBarberpoleFlanger.UpdateStages;
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   if DialStages.Value <> Parameter[0]
    then DialStages.Value := Parameter[0];
   LbStagesValue.Caption := IntToStr(Round(Parameter[0]));
  end;
end;

procedure TFmBarberpoleFlanger.UpdateAlgorithm;
begin
 with TBarberpoleFlangerModule(Owner) do
  begin
   if SBAlgorithm.ItemIndex <> Round(Parameter[4])
    then SBAlgorithm.ItemIndex := Round(Parameter[4]);
  end;
end;

end.
