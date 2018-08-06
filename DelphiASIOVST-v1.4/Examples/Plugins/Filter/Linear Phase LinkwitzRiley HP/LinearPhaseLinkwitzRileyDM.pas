unit LinearPhaseLinkwitzRileyDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_DspFilterButterworth, DAV_DspConvolution, 
  DAV_DspPolyphaseDownsampler, DAV_VSTModule;

type
  TLinearPhaseLinkwitzRileyDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOversampleChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection     : TCriticalSection;
    FConvolution         : TLowLatencyConvolutionStereo32;
    FLinkwitzRileyFilter : TButterworthHighPassFilter;
    FDownsampler         : TPolyphaseDownsampler64;
    FOversampled         : Boolean;
    procedure CalculateFilterKernel;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, LinearPhaseLinkwitzRileyGUI, DAV_VSTParameters;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleOpen(Sender: TObject);
begin
 FConvolution := TLowLatencyConvolutionStereo32.Create;
 with FConvolution do
  begin
   MinimumIRBlockOrder := 5;
   MaximumIRBlockOrder := 18;
  end;
 FLinkwitzRileyFilter := TButterworthHighPassFilter.Create(2);

 FDownsampler := TPolyphaseDownsampler64.Create;
 FOversampled := False;

 Parameter[0] := 20;
 Parameter[1] := 4;

 CalculateFilterKernel;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FConvolution);
 FreeAndNil(FDownsampler);
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmLinearPhaseLinkwitzRiley.Create(Self);
end;

procedure TLinearPhaseLinkwitzRileyDataModule.ParameterFilterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(2 * Round(0.5 * Parameter[Index])));
end;

procedure TLinearPhaseLinkwitzRileyDataModule.ParameterOversampleChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLinkwitzRileyFilter) then
  begin
   FOversampled := Value > 0.5;
   CalculateFilterKernel;
  end;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.ParameterFilterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLinkwitzRileyFilter) then
  begin
   FLinkwitzRileyFilter.Order := Round(0.5 * Parameter[Index]);
   CalculateFilterKernel;
  end;

 // update GUI
 if EditorForm is TFmLinearPhaseLinkwitzRiley then
  with TFmLinearPhaseLinkwitzRiley(EditorForm)
   do UpdateOrder;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLinkwitzRileyFilter) then
  begin
   if FOversampled
    then FLinkwitzRileyFilter.Frequency := 0.5 * Value
    else FLinkwitzRileyFilter.Frequency := Value;
   CalculateFilterKernel;
  end;

 // update GUI
 if EditorForm is TFmLinearPhaseLinkwitzRiley then
  with TFmLinearPhaseLinkwitzRiley(EditorForm)
   do UpdateFrequency;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.CalculateFilterKernel;
var
  Sample       : Integer;
  FilterKernel : PDAVSingleFixedArray;
  KernelSize   : Integer;
  Peak         : Double;
  Data         : TDAV2DoubleArray;
const
  CStepSize : Integer = 1 shl 8;
begin
 KernelSize := CStepSize;
 GetMem(FilterKernel, KernelSize * SizeOf(Single));
 try
  // forward processing
  FLinkwitzRileyFilter.ResetStates;
  FilterKernel[0] := FLinkwitzRileyFilter.ProcessSample64(1.0);
  Peak := abs(FilterKernel[0]);
  for Sample := 1 to KernelSize - 1 do
   begin
    FilterKernel[Sample] := FLinkwitzRileyFilter.ProcessSample64(0.0);
    Peak := 0.9 * Peak;
    if abs(FilterKernel[Sample]) > Peak
     then Peak := abs(FilterKernel[Sample]);
   end;

  while (Peak > 1E-5) and (KernelSize < 1 shl 16) do
   begin
    ReallocMem(FilterKernel, (KernelSize + CStepSize) * SizeOf(Single));
    for Sample := KernelSize to KernelSize + CStepSize - 1 do
     begin
      FilterKernel[Sample] := FLinkwitzRileyFilter.ProcessSample64(0.0);
      Peak := 0.9 * Peak;
      if abs(FilterKernel[Sample]) > Peak
       then Peak := abs(FilterKernel[Sample]);
     end;
    Inc(KernelSize, CStepSize);
   end;

  // backward processing
  FLinkwitzRileyFilter.ResetStates;
  ReallocMem(FilterKernel, 2 * KernelSize * SizeOf(Single));
  Move(FilterKernel^[0], FilterKernel^[KernelSize], KernelSize * SizeOf(Single));
  for Sample := 0 to KernelSize - 1
   do FilterKernel[Sample] := FLinkwitzRileyFilter.ProcessSample64(FilterKernel[2 * KernelSize - 1 - Sample]);
  for Sample := KernelSize to 2 * KernelSize - 1
   do FilterKernel[Sample] := FLinkwitzRileyFilter.ProcessSample64(0.0);

  if FOversampled then
   for Sample := 0 to KernelSize - 1 do
    begin
     Data[0] := FilterKernel[2 * Sample];
     Data[1] := FilterKernel[2 * Sample + 1];
     FilterKernel[Sample] := 2 * FDownsampler.ProcessSample64(Data)
    end;

  FCriticalSection.Enter;
  try
   if FOversampled
    then FConvolution.LoadImpulseResponse(FilterKernel, KernelSize)
    else FConvolution.LoadImpulseResponse(FilterKernel, 2 * KernelSize);
   InitialDelay := FConvolution.Latency + KernelSize;
  finally
   FCriticalSection.Leave;
  end;
 finally
  Dispose(FilterKernel);
 end;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if abs(SampleRate) > 0 then
  if Assigned(FLinkwitzRileyFilter) then
   begin
    FLinkwitzRileyFilter.SampleRate := SampleRate;
    CalculateFilterKernel;
   end;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
 FCriticalSection.Enter;
 try
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
  FConvolution.ProcessBlock(@Outputs[0, 0], @Outputs[1, 0], SampleFrames);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
