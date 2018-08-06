unit HRTF3DModule;

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
  Forms, DAV_Types, DAV_Complex, DAV_VSTModule, DAV_DspConvolution, DAV_DspHRTF;

type
  TVSTHRTF3DModule = class(TVSTModule)
    procedure VST2ModuleOpen(Sender: TObject);
    procedure VST2ModuleClose(Sender: TObject);
    procedure VST2ModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VST2ModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamAzimuthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInterpolationDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterInterpolationChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDisplayHRTFsDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FIR           : array [0..1] of PDAVSingleFixedArray;
    FHRTFs        : THrtfs;
    FLength       : Cardinal;
    FConvolution  : array [0..1] of TLowLatencyConvolution32;
  public
    property HRTFs: THrtfs read FHRTFs;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{$R Default.RES}

uses
  Math, HRTF3DGUI;

procedure TVSTHRTF3DModule.VST2ModuleOpen(Sender: TObject);
var
  Channel : Integer;
  RS      : TResourceStream;
begin
 FHRTFs := THRTFs.Create;

 RS := TResourceStream.Create(hInstance, 'Default', 'HRTF');
 try
  FHRTFs.LoadFromStream(RS);
 finally
  RS.Free;
 end;

 FLength := FHRTFs.MinimumHrirSize;

 GetMem(FIR[0], FLength * SizeOf(Single));
 GetMem(FIR[1], FLength * SizeOf(Single));

 for Channel := 0 to 1 do
  begin
   FConvolution[Channel] := TLowLatencyConvolution32.Create;
   FConvolution[Channel].MinimumIRBlockOrder :=  6;
   FConvolution[Channel].MaximumIRBlockOrder := 13;
  end;
 // initial parameters
 Parameter[0] := 0;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 2;

 // Preset 1
 with Programs[1] do
  begin
   Parameter[0] := 90;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 2;
   Parameter[4] := 2;
  end;

 // set editor form class
 EditorFormClass := TVSTGUI;
end;

procedure TVSTHRTF3DModule.VST2ModuleClose(Sender: TObject);
begin
 Dispose(FIR[0]);
 Dispose(FIR[1]);
 FreeAndNil(FConvolution);
 FreeAndNil(FHRTFs);
end;

procedure TVSTHRTF3DModule.VST2ModuleProcess(
  const Inputs, Outputs: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 for Channel := 0 to 1
  do FConvolution[Channel].ProcessBlock(@Inputs[Channel, 0], @Outputs[Channel, 0], min(BlockSize, SampleFrames));
end;

procedure TVSTHRTF3DModule.ParameterDisplayHRTFsDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TVSTHRTF3DModule.ParameterInterpolationChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FHRTFs) then
  case Round(Parameter[Index]) of
   1 : FHRTFs.InterpolationType := itNearest;
   2 : FHRTFs.InterpolationType := itLinear;
   3 : FHRTFs.InterpolationType := itLinear3;
  end;
end;

procedure TVSTHRTF3DModule.ParameterInterpolationDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  1 : PreDefined := 'Nearest';
  2 : PreDefined := 'Linear (2 Points)';
  3 : PreDefined := 'Linear (3 Points)';
 end;
end;

procedure TVSTHRTF3DModule.ParamAzimuthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateAzimuth;
end;

procedure TVSTHRTF3DModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 // yet todo!
end;

procedure TVSTHRTF3DModule.VST2ModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
const
  CDeg2Rad = 2 * Pi / 360;
begin
 if Assigned(FHRTFs) then
  begin
   FHRTFs.InterpolateHrir(Parameter[0] * CDeg2Rad,
                          Parameter[1] * CDeg2Rad, FLength, FIR[0], FIR[1]);
   FConvolution[0].LoadImpulseResponse(FIR[0], FLength);
   FConvolution[1].LoadImpulseResponse(FIR[1], FLength);
  end;
end;

end.
