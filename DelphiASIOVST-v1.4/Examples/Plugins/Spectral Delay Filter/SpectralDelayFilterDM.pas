unit SpectralDelayFilterDM;

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
  Forms, SyncObjs, DAV_Types, DAV_DspFilterSpectralDelay, DAV_VSTModule;

type
  TSpectralDelayFilterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleClose(Sender: TObject);
    procedure ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
  private
    FCriticalSection      : TCriticalSection;
    FSpectralDelayFilters : array of TSpectralDelayFilter;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  SpectralDelayFilterGUI;

procedure TSpectralDelayFilterModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(numInputs = numOutputs);
 SetLength(FSpectralDelayFilters, numOutputs);

 for Channel := 0 to Length(FSpectralDelayFilters) - 1
  do FSpectralDelayFilters[Channel] := TSpectralDelayFilter.Create;

 // initialize parameters
 Parameter[0] := 0.9;
 Parameter[1] := 16;

 // set editor form class
 EditorFormClass := TFmSpectralDelayFilter;
end;

procedure TSpectralDelayFilterModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
  if Assigned(FSpectralDelayFilters[Channel])
   then FreeAndNil(FSpectralDelayFilters[Channel]);
end;

procedure TSpectralDelayFilterModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSpectralDelayFilterModule.ParameterTuneChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
  if Assigned(FSpectralDelayFilters[Channel])
   then FSpectralDelayFilters[Channel].Frequency := Sqrt(Value); // * 0.5 * SampleRate;
end;

procedure TSpectralDelayFilterModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
   if Assigned(FSpectralDelayFilters[Channel])
    then FSpectralDelayFilters[Channel].FilterCount := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpectralDelayFilterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FSpectralDelayFilters[Channel].ProcessSample64(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpectralDelayFilterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
  if Assigned(FSpectralDelayFilters[Channel])
   then FSpectralDelayFilters[Channel].SampleRate := SampleRate;
end;

end.
