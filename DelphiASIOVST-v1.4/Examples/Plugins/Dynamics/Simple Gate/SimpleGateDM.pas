unit SimpleGateDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

type
  TSimpleGateDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure SGDMThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
  private
    FSimpleGates : array [0..1] of TClassicGate;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  EditorFrm;

procedure TSimpleGateDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSimpleGates[0] := TClassicGate.Create;
 FSimpleGates[1] := TClassicGate.Create;

 Parameter[0] := -10;
end;

procedure TSimpleGateDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleGates[0]);
 FreeAndNil(FSimpleGates[1]);
end;

procedure TSimpleGateDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TEditorForm.Create(Self);
end;

procedure TSimpleGateDataModule.SGDMThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSimpleGates[0]) then FSimpleGates[0].Threshold_dB := Value;
 if Assigned(FSimpleGates[1]) then FSimpleGates[1].Threshold_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateScrollBar;
end;

procedure TSimpleGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSimpleGates[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSimpleGates[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure TSimpleGateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(FSimpleGates[0]) then FSimpleGates[0].SampleRate := SampleRate;
 if Assigned(FSimpleGates[1]) then FSimpleGates[1].SampleRate := SampleRate;
end;

end.
