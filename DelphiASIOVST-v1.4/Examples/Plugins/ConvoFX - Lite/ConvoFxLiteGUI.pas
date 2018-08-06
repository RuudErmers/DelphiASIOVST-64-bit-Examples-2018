unit ConvoFXLiteGUI;

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
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, 
  DAV_GuiStitchedControls, DAV_GuiStitchedDial, DAV_GuiStitchedPngList,
  DAV_GuiImageControl, DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TFmConvoFXLite = class(TForm)
    DialDamp: TGuiStitchedDial;
    DialGain: TGuiStitchedDial;
    DialIR: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbDamp: TGuiLabel;
    LbDampValue: TGuiLabel;
    LbGain: TGuiLabel;
    LbGainValue: TGuiLabel;
    LbIR: TGuiLabel;
    LbIRSelected: TGuiLabel;
    procedure FormShow(Sender: TObject);
    procedure DialIRChange(Sender: TObject);
    procedure DialGainChange(Sender: TObject);
    procedure DialDampChange(Sender: TObject);
    procedure LbIRSelectedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    procedure UpdateIRSelect;
    procedure UpdateGain;
    procedure UpdateDamping;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Dialogs, Math, DAV_VSTModuleWithPrograms, ConvoFXLiteDM;

procedure TFmConvoFXLite.FormShow(Sender: TObject);
begin
 UpdateIRSelect;
 UpdateGain;
 UpdateDamping;
end;

procedure TFmConvoFXLite.DialDampChange(Sender: TObject);
begin
 with TConvoFXLiteDataModule(Owner) do
  begin
   Parameter[4] := DialDamp.Value;
  end;
end;

procedure TFmConvoFXLite.DialGainChange(Sender: TObject);
begin
 with TConvoFXLiteDataModule(Owner) do
  begin
   Parameter[3] := DialGain.Value;
  end;
end;

procedure TFmConvoFXLite.DialIRChange(Sender: TObject);
begin
 with TConvoFXLiteDataModule(Owner) do
  begin
   if Round(Parameter[2]) <> Round(DialIR.Value)
    then Parameter[2] := Round(DialIR.Value);
  end;
end;

procedure TFmConvoFXLite.LbIRSelectedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 case Button of
  mbLeft  : DialIR.Value := Round(DialIR.Value) + 1;
  mbRight : DialIR.Value := Round(DialIR.Value) - 1;
 end;
end;

procedure TFmConvoFXLite.UpdateGain;
begin
 with TConvoFXLiteDataModule(Owner) do
  begin
   if DialGain.Value <> Parameter[3]
    then DialGain.Value := Parameter[3];
   LbGainValue.Caption := FloatToStrF(RoundTo(Parameter[3], -1), ffGeneral, 2, 2) + ' dB';
  end;
end;

procedure TFmConvoFXLite.UpdateDamping;
begin
 with TConvoFXLiteDataModule(Owner) do
  begin
   if DialDamp.Value <> Parameter[4]
    then DialDamp.Value := Parameter[4];
   if Parameter[4] < 1000
    then LbDampValue.Caption := FloatToStrF(RoundTo(Parameter[4], -1), ffGeneral, 3, 3) + ' Hz'
    else LbDampValue.Caption := FloatToStrF(RoundTo(1E-3 * Parameter[4], -1), ffGeneral, 3, 3) + ' kHz';
  end;
end;

procedure TFmConvoFXLite.UpdateIRSelect;
begin
 with TConvoFXLiteDataModule(Owner) do
  begin
   if Round(DialIR.Value) <> Round(Parameter[2])
    then DialIR.Value := Round(Parameter[2]);
   LbIRSelected.Caption := IntToStr(Round(Parameter[2]));
  end;
end;

end.
