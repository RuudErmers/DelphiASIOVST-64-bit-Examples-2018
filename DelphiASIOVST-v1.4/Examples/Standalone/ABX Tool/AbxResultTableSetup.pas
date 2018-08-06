unit AbxResultTableSetup;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, 
  Forms, Controls, StdCtrls, Buttons, ExtCtrls, Mask;

type
  TFmResultTableSetup = class(TForm)
    BtOK: TButton;
    BtCancel: TButton;
    Bevel: TBevel;
    LbColumnColor: TLabel;
    RBColorEverySecond: TRadioButton;
    RBColorAboveThreshold: TRadioButton;
    RBColorBelowThreshold: TRadioButton;
    LbRatingThreshold: TLabel;
    MERatingThreshold: TMaskEdit;
    procedure FormCreate(Sender: TObject);
    procedure MERatingThresholdChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FRatingThreshold: Single;
  public
    property RatingThreshold: Single read FRatingThreshold;
  end;

var
  FmResultTableSetup: TFmResultTableSetup;

implementation

uses
  IniFiles, AbxMain, AbxProject;

{$R *.dfm}

procedure TFmResultTableSetup.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i : Integer;
begin
 for i := 0 to FmAbxMain.MDIChildCount - 1
  do TFmProject(FmAbxMain.MDIChildren[i]).DBGridPro.Invalidate;
end;

procedure TFmResultTableSetup.FormCreate(Sender: TObject);
var
  str : string;
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   FRatingThreshold := ReadFloat('Table Setup', 'Rating Threshold', 0.25);
   str := ReadString('Table Setup', 'Color Columns', 'Every Second');
   RBColorEverySecond.Checked := str = 'Every Second';
   RBColorAboveThreshold.Checked := str = 'Above Rating Threshold';
   RBColorBelowThreshold.Checked := str = 'Below Rating Threshold';
  finally
   Free;
  end;
end;

procedure TFmResultTableSetup.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   WriteFloat('Table Setup', 'Rating Threshold', FRatingThreshold);
   if RBColorEverySecond.Checked
    then WriteString('Table Setup', 'Color Columns', 'Every Second');
   if RBColorAboveThreshold.Checked
    then WriteString('Table Setup', 'Color Columns', 'Above Rating Threshold');
   if RBColorBelowThreshold.Checked
    then WriteString('Table Setup', 'Color Columns', 'Below Rating Threshold');
  finally
   Free;
  end;
end;

procedure TFmResultTableSetup.MERatingThresholdChange(Sender: TObject);
begin
 try
  FRatingThreshold := StrToFloat(MERatingThreshold.Text);
  MERatingThreshold.Text := FloatToStr(FRatingThreshold);
 except
  MERatingThreshold.Text := '0,25';
 end;
end;

end.
