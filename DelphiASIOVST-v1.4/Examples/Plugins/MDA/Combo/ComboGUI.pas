unit ComboGUI;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFmCombo = class(TForm)
    CBModel: TComboBox;
    LbBias: TLabel;
    LbBiasValue: TLabel;
    LbDrive: TLabel;
    LbDriveValue: TLabel;
    LbFreq: TLabel;
    LbFrequencyValue: TLabel;
    LbModel: TLabel;
    LbOutput: TLabel;
    LbOutputValue: TLabel;
    LbReso: TLabel;
    LbResonanceValue: TLabel;
    RBMono: TRadioButton;
    RBStereo: TRadioButton;
    SBBias: TScrollBar;
    SBDrive: TScrollBar;
    SBFreq: TScrollBar;
    SBOutput: TScrollBar;
    SBReso: TScrollBar;
    procedure SBDriveChange(Sender: TObject);
    procedure SBBiasChange(Sender: TObject);
    procedure SBOutputChange(Sender: TObject);
    procedure SBFreqChange(Sender: TObject);
    procedure SBResoChange(Sender: TObject);
    procedure RBMonoClick(Sender: TObject);
    procedure RBStereoClick(Sender: TObject);
    procedure CBModelChange(Sender: TObject);
  end;

var
  FmCombo: TFmCombo;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  ComboDM;

procedure TFmCombo.CBModelChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[0] := CBModel.ItemIndex;
end;

procedure TFmCombo.RBMonoClick(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[4] := 0;
end;

procedure TFmCombo.RBStereoClick(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[4] := 1;
end;

procedure TFmCombo.SBBiasChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[2] := 0.1 * SBBias.Position;
end;

procedure TFmCombo.SBDriveChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[1] := 0.1 * SBDrive.Position;
end;

procedure TFmCombo.SBOutputChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[3] := 0.1 * SBOutput.Position;
end;

procedure TFmCombo.SBFreqChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[5] := 0.1 * SBFreq.Position;
end;

procedure TFmCombo.SBResoChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[6] := 0.1 * SBReso.Position;
end;

end.
