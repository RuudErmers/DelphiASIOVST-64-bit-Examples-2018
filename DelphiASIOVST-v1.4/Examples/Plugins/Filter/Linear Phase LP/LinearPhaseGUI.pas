unit LinearPhaseGUI;

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
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, 
  DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiStitchedControls,
  DAV_GuiStitchedPngList, DAV_GuiStitchedDial, DAV_GuiImageControl,
  DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TFmLinearPhase = class(TForm)
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    DialFrequency: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    procedure FormShow(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialFrequencyDblClick(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
  private
    FEdValue : TEdit;
  public
    procedure UpdateFrequency;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF}
  DAV_VSTModuleWithPrograms, LinearPhaseDM;

procedure TFmLinearPhase.FormShow(Sender: TObject);
begin
 UpdateFrequency;
end;

procedure TFmLinearPhase.DialFrequencyDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbFrequencyValue.Left;
   Top := LbFrequencyValue.Top;
   Width := LbFrequencyValue.Width;
   Height := LbFrequencyValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbFrequencyValue.Caption;
   Tag := 0;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmLinearPhase.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 with TLinearPhaseDataModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, AnsiString(FEdValue.Text));
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmLinearPhase.DialFrequencyChange(Sender: TObject);
begin
 with TLinearPhaseDataModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Value
    then Parameter[0] := DialFrequency.Value;
  end;
end;

procedure TFmLinearPhase.UpdateFrequency;
var
  Freq : Single;
begin
 with TLinearPhaseDataModule(Owner) do
  begin
   Freq := Parameter[0];
   if DialFrequency.Value <> Freq
    then DialFrequency.Value := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + ' kHz';
  end;
end;

end.
