unit NonlinearDSP;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows,{$ENDIF}
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule, DAV_VSTCustomModule,
  DAV_VSTParameters;

type

  { TVSTOpAmp }

  TVSTOpAmp = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: THandle);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FGain : Double;
  public
    property Gain: Double read FGain;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Controls, DAV_Common, DAV_Approximations, NonlinearGUI;

{ TVSTOpAmp }

procedure TVSTOpAmp.VSTModuleOpen(Sender: TObject);
begin
 FGain := 1;
 Parameter[0] := 1;
end;

procedure TVSTOpAmp.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: THandle);
begin
 GUI := TVSTGUI.Create(Self);

 with TVSTGUI(GUI) do
  begin
   LbGain.Caption  := 'OpAmp Gain';
   SbGain.Max      := 1000;
   SbGain.Min      := 100;
   SbGain.Position := 100;
  end;
end;

procedure TVSTOpAmp.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i, j : Integer;
begin
 for j := 0 to min(numOutputs, numInputs) - 1 do
  for i := 0 to SampleFrames - 1
   do Outputs[j, i] := FastTanhOpt5Term(FGain * Inputs[j, i]);
end;

procedure TVSTOpAmp.VSTModuleProcessDoubleReplacing(const inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  i, j : Integer;
begin
 for j := 0 to min(numOutputs, numInputs) - 1 do
  for i := 0 to SampleFrames - 1
   do Outputs[j, i] := FastTanhOpt5Term(FGain * Inputs[j, i]);
end;

procedure TVSTOpAmp.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FGain := 2 * dB_to_Amp(Value);

 // eventually update GUI
 if FEditorForm is TVSTGUI
  then TVSTGUI(FEditorForm).UpdateGain;
end;

end.
