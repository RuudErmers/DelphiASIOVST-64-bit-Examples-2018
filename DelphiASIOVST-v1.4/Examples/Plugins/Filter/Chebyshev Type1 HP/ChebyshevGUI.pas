unit ChebyshevGUI;

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
  Forms, ExtCtrls, Controls, StdCtrls, DAV_Types, DAV_VSTModule, 
  DAV_GuiCustomControl, DAV_GuiGraphicControl, DAV_GuiLabel, DAV_GuiPanel, 
  DAV_GuiStitchedControls, DAV_GuiStitchedDial, DAV_GuiStitchedPngList,
  DAV_GuiImageControl;

type
  TFmChebyshev = class(TForm)
    DialFrequency: TGuiStitchedDial;
    DialOrder: TGuiStitchedDial;
    DialRipple: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbChebyshevFilterDemo: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbRipple: TGuiLabel;
    LbRippleValue: TGuiLabel;
    PnControls: TGuiPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialRippleChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure DialFrequencyDblClick(Sender: TObject);
    procedure DialRippleDblClick(Sender: TObject);
    procedure DialOrderDblClick(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure PnControlsClick(Sender: TObject);
  private
    FEdValue: TEdit;
  public
    procedure UpdateFrequency;
    procedure UpdateRipple;
    procedure UpdateOrder;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF}
  DAV_VSTModuleWithPrograms, ChebyshevDM;

procedure TFmChebyshev.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmChebyshev.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateRipple;
 UpdateOrder;
(*
 with TChebyshevHPModule(Owner) do
  begin
   Resizer.SetEditorHwnd(Self.Handle);
  end;
*)
end;

procedure TFmChebyshev.PnControlsClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmChebyshev.FormClose(Sender: TObject; var Action: TCloseAction);
begin
(*
 with TChebyshevHPModule(Owner) do
  begin
   Resizer.SetEditorHwnd(0);
  end;
*)
end;

procedure TFmChebyshev.UpdateFrequency;
var
  Freq : Single;
begin
 with TChebyshevHPModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialFrequency.Value <> Freq
    then DialFrequency.Value := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(Freq * 1E-3, ffGeneral, 3, 3) + ' kHz'
  end;
end;

procedure TFmChebyshev.DialFrequencyChange(Sender: TObject);
begin
 with TChebyshevHPModule(Owner) do
  begin
   if ParameterByName['Frequency'] <> DialFrequency.Value
    then ParameterByName['Frequency'] := DialFrequency.Value;
  end;
end;

procedure TFmChebyshev.DialFrequencyDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbFrequencyValue.Left;
   Top := LbFrequencyValue.Top;
   Width := LbFrequencyValue.Width;
   Height := LbFrequencyValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbFrequencyValue.Caption;
   Tag := 0;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmChebyshev.DialOrderChange(Sender: TObject);
begin
 with TChebyshevHPModule(Owner) do
  begin
   if ParameterByName['Order'] <> DialOrder.Value
    then ParameterByName['Order'] := DialOrder.Value;
  end;
end;

procedure TFmChebyshev.DialOrderDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbOrderValue.Left;
   Top := LbOrderValue.Top;
   Width := LbOrderValue.Width;
   Height := LbOrderValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbOrderValue.Caption;
   Tag := 2;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmChebyshev.DialRippleChange(Sender: TObject);
begin
 with TChebyshevHPModule(Owner) do
  begin
   if ParameterByName['Ripple'] <> DialRipple.Value
    then ParameterByName['Ripple'] := DialRipple.Value;
  end;
end;

procedure TFmChebyshev.DialRippleDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbRippleValue.Left;
   Top := LbRippleValue.Top;
   Width := LbRippleValue.Width;
   Height := LbRippleValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbRippleValue.Caption;
   Tag := 1;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmChebyshev.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 with TChebyshevHPModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, AnsiString(FEdValue.Text));
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmChebyshev.UpdateOrder;
var
  Order : Single;
begin
 with TChebyshevHPModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialOrder.Value <> Order
    then DialOrder.Value := Order;
   LbOrderValue.Caption := IntToStr(Round(Order));
  end;
end;

procedure TFmChebyshev.UpdateRipple;
var
  Ripple : Single;
begin
 with TChebyshevHPModule(Owner) do
  begin
   Ripple := ParameterByName['Ripple'];
   if DialRipple.Value <> Ripple
    then DialRipple.Value := Ripple;
   LbRippleValue.Caption := FloatToStrF(Ripple, ffGeneral, 3, 3) + ' dB';
  end;
end;

end.
