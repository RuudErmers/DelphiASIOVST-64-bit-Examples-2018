unit LinearPhaseLinkwitzRileyGUI;

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
  Forms, Controls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, 
  DAV_GuiStitchedControls, DAV_GuiStitchedDial, DAV_GuiStitchedPngList;

type
  TFmLinearPhaseLinkwitzRiley = class(TForm)
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    GSPL: TGuiStitchedPNGList;
    DialFrequency: TGuiStitchedDial;
    DialOrder: TGuiStitchedDial;
    procedure FormShow(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
  public
    procedure UpdateFrequency;
    procedure UpdateOrder;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_VSTModuleWithPrograms, LinearPhaseLinkwitzRileyDM;

procedure TFmLinearPhaseLinkwitzRiley.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateOrder;
end;

procedure TFmLinearPhaseLinkwitzRiley.DialFrequencyChange(Sender: TObject);
begin
 with TLinearPhaseLinkwitzRileyDataModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Value
    then Parameter[0] := DialFrequency.Value;
  end;
end;

procedure TFmLinearPhaseLinkwitzRiley.DialOrderChange(Sender: TObject);
begin
 with TLinearPhaseLinkwitzRileyDataModule(Owner) do
  begin
   if Parameter[1] <> DialOrder.Value
    then Parameter[1] := DialOrder.Value;
  end;
end;

procedure TFmLinearPhaseLinkwitzRiley.UpdateFrequency;
var
  Freq : Single;
begin
 with TLinearPhaseLinkwitzRileyDataModule(Owner) do
  begin
   Freq := Parameter[0];
   if DialFrequency.Value <> Freq
    then DialFrequency.Value := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + ' kHz';
  end;
end;

procedure TFmLinearPhaseLinkwitzRiley.UpdateOrder;
var
  Order : Single;
begin
 with TLinearPhaseLinkwitzRileyDataModule(Owner) do
  begin
   Order := Parameter[1];
   if DialOrder.Value <> Order
    then DialOrder.Value := Order;
   LbOrderValue.Caption := IntToStr(24 * Round(0.5 * Order)) + 'dB/Oct';
  end;
end;

end.
