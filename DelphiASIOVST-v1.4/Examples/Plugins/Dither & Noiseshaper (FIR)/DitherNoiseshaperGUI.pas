unit DitherNoiseshaperGUI;

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

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Classes, Forms,
  Controls, StdCtrls, Spin, DAV_GuiDial;

type
  TFmDitherNoiseshaper = class(TForm)
    CbDitherType: TComboBox;
    CbLimit: TCheckBox;
    CbNoiseshaperType: TComboBox;
    DialAmplitude: TGuiDial;
    LbBit: TLabel;
    LbDitherAmp: TLabel;
    LbDitherType: TLabel;
    LbFinalBitDepth: TLabel;
    LbNoiseshaperType: TLabel;
    SeBitDepth: TSpinEdit;
    procedure FormShow(Sender: TObject);
    procedure CbDitherTypeChange(Sender: TObject);
    procedure CbLimitClick(Sender: TObject);
    procedure CbNoiseshaperTypeChange(Sender: TObject);
    procedure DialAmplitudeChange(Sender: TObject);
    procedure SeBitDepthChange(Sender: TObject);
  public
    procedure UpdateBitDepth;
    procedure UpdateLimit;
    procedure UpdateDitherType;
    procedure UpdateDitherAmplitude;
    procedure UpdateNoiseShaper;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DitherNoiseshaperDM;

procedure TFmDitherNoiseshaper.FormShow(Sender: TObject);
begin
 UpdateBitDepth;
 UpdateLimit;
 UpdateDitherType;
 UpdateDitherAmplitude;
 UpdateNoiseShaper;
end;

procedure TFmDitherNoiseshaper.SeBitDepthChange(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[0] <> SeBitDepth.Value
    then Parameter[0] := SeBitDepth.Value;
  end;
end;

procedure TFmDitherNoiseshaper.CbLimitClick(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[1] <> Integer(CbLimit.Checked)
    then Parameter[1] := Integer(CbLimit.Checked);
  end;
end;

procedure TFmDitherNoiseshaper.CbDitherTypeChange(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[2] <> CbDitherType.ItemIndex
    then Parameter[2] := CbDitherType.ItemIndex;
  end;
end;

procedure TFmDitherNoiseshaper.DialAmplitudeChange(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[3] <> DialAmplitude.Position
    then Parameter[3] := DialAmplitude.Position;
  end;
end;

procedure TFmDitherNoiseshaper.CbNoiseshaperTypeChange(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[4] <> CbNoiseshaperType.ItemIndex
    then Parameter[4] := CbNoiseshaperType.ItemIndex;
  end;
end;

procedure TFmDitherNoiseshaper.UpdateBitDepth;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if SeBitDepth.Value <> Round(Parameter[0])
    then SeBitDepth.Value := Round(Parameter[0])
  end;
end;

procedure TFmDitherNoiseshaper.UpdateLimit;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if CbLimit.Checked <> Boolean(Round(Parameter[1]))
    then CbLimit.Checked := Boolean(Round(Parameter[1]))
  end;
end;

procedure TFmDitherNoiseshaper.UpdateDitherType;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if CbDitherType.ItemIndex <> Round(Parameter[2])
    then CbDitherType.ItemIndex := Round(Parameter[2])
  end;
end;

procedure TFmDitherNoiseshaper.UpdateDitherAmplitude;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if DialAmplitude.Position <> Parameter[3]
    then DialAmplitude.Position := Parameter[3]
  end;
end;

procedure TFmDitherNoiseshaper.UpdateNoiseShaper;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if CbNoiseshaperType.ItemIndex <> Round(Parameter[4])
    then CbNoiseshaperType.ItemIndex := Round(Parameter[4])
  end;
end;

end.
