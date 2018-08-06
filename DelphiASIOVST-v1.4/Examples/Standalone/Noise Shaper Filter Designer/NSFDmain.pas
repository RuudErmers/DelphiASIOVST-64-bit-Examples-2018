unit NSFDmain;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF}SysUtils, Classes,
  Graphics, Controls, Forms, Menus, StdCtrls, Dialogs, DAV_Types,
  DAV_DspNoiseShapingFilterDesigner;

type
  TFmNoiseshapingFilterDesigner = class(TForm)
    Memo: TMemo;
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MISaveAs: TMenuItem;
    MICalculation: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MICalculationClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
  private
    procedure CoefficientUpdate(Sender: TObject; Coefficients: PDAVDoubleFixedArray; Best: Double);
  end;

var
  FmNoiseshapingFilterDesigner: TFmNoiseshapingFilterDesigner;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFmNoiseshapingFilterDesigner.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmNoiseshapingFilterDesigner.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := 'txt';
   Filter := 'Text (*.txt)|*.txt';
   if Execute then
    begin
     Memo.Lines.SaveToFile(Filename);
    end;
  finally
   Free
  end;
end;

procedure TFmNoiseshapingFilterDesigner.CoefficientUpdate(Sender: TObject; Coefficients: PDAVDoubleFixedArray; Best: Double);
var
  Sample : Integer;
begin
 with Sender as TNoiseShapingFilterDesigner do
  begin
   for Sample := 0 to SampleFrames - 1
    do Memo.Lines.Add('Coefficient[' + IntToStr(Sample) + '] = ' + FloatToStr(Coefficients[Sample]));
   Memo.Lines.Add('Best = ' + FloatToStr(Best));
  end;
 Application.ProcessMessages;
end;

procedure TFmNoiseshapingFilterDesigner.MICalculationClick(Sender: TObject);
begin
 with TNoiseShapingFilterDesigner.Create do
  try
   OnCoefficientUpdate := CoefficientUpdate;
   Calculate;
  finally
   Free;
  end;
end;

end.
