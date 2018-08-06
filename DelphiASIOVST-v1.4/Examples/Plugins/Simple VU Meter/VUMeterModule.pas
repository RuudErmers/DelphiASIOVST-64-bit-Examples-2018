unit VUMeterModule;

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
  Windows, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;

type
  TVSTVUMeterModule = class(TVSTModule)
    procedure VSTModuleEditIdle(Sender: TObject);
    procedure VSTModuleProcess(const inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
    FPeak   : TDAV2DoubleArray;
    FVolume : TDAV2DoubleArray;
    function GetPeak(index: Integer): Double;
  public
    property Peak[index : Integer] : Double read GetPeak;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses VUMeterGUI;

function TVSTVUMeterModule.GetPeak(index: Integer): Double;
begin
 result := FPeak[index];
end;

procedure TVSTVUMeterModule.VSTModuleEditIdle(Sender: TObject);
begin
 if Assigned(EditorForm) then
  with (EditorForm As TVSTVUMeterGUI)
   do TimerTimer(Sender);
end;

procedure TVSTVUMeterModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTVUMeterGUI.Create(Self);
end;

////////////////////////////////////////////////////////////////////////////////
// 32 Bit Processing
////////////////////////////////////////////////////////////////////////////////
procedure TVSTVUMeterModule.VSTModuleProcess(const inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var i: integer;
begin
 // This is usually the most important part of your plugin:
 // Here the samples for each input and output channel can be processed
 // individually. In this example, the volume of the left and right
 // channel is set by variables determined by the parameters 0 and 1,
 // that were pre-calculated and stored in the variables vol_l and vol_r
 // in the parameterChanged procedure.
 // There is also a simple VU meter code here
for i := 0 to SampleFrames - 1 do
  begin
   outputs[0, i] := inputs[0, i] * FVolume[0];
   outputs[1, i] := inputs[1, i] * FVolume[1];

   // simple (but not very efficient) VU meter code:
   FPeak[0] := FPeak[0] * 0.9999;
   FPeak[1] := FPeak[1] * 0.9999;
   if abs(outputs[0, i]) > FPeak[0] then FPeak[0] := abs(outputs[0, i]);
   if abs(outputs[1, i]) > FPeak[1] then FPeak[1] := abs(outputs[1, i]);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTVUMeterModule.VSTModuleProcessDoubleReplacing(const inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var i: integer;
begin
 // Same as above, but (internally) 64Bit...
 for i := 0 to SampleFrames - 1 do
  begin
   outputs[0, i] := inputs[0, i] * FVolume[0];
   outputs[1, i] := inputs[1, i] * FVolume[1];

   // simple (but not very efficient) VU meter code:
   FPeak[0] := FPeak[0] * 0.9999;
   FPeak[1] := FPeak[1] * 0.9999;
   if abs(outputs[0, i]) > FPeak[0] then FPeak[0] := abs(outputs[0, i]);
   if abs(outputs[1, i]) > FPeak[1] then FPeak[1] := abs(outputs[1, i]);
  end;
end;

procedure TVSTVUMeterModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FVolume[Index] := dB_to_Amp(Value);
 if Amp_to_dB(FVolume[Index])<>Value then
  with (EditorForm As TVSTVUMeterGUI) do
   case Index of
    0 : SBLeft.Position := Round(Amp_to_dB(FVolume[Index]));
    1 : SBRight.Position := Round(Amp_to_dB(FVolume[Index]));
   end;
end;

end.
