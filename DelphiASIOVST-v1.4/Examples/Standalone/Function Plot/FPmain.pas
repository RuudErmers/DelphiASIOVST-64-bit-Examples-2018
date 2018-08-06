unit FPmain;

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

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, DAV_GuiBaseControl, DAV_GuiGraphXY;

type
  TFmFunctionPlot = class(TForm)
    GuiGraphXY: TGuiGraphXY;
    function FunctionPlotEvaluate(Sender: TObject; X: Double): Double;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  FmFunctionPlot: TFmFunctionPlot;

implementation

uses
  Math;

{$R *.dfm}

procedure TFmFunctionPlot.FormCreate(Sender: TObject);
begin
 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
   begin
    OnEvaluate := FunctionPlotEvaluate;
   end;
end;

function TFmFunctionPlot.FunctionPlotEvaluate(Sender: TObject; X: Double): Double;
begin
 result := tanh(x);
end;

end.
