unit HrtfConvolverDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_AudioData, DAV_DspConvolution,
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_DspHrtf;

type
  THrtfConvolverDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs,
      Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterAzimuthChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure ParameterElevationChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure ParameterRadiusChange(Sender: TObject; const Index: Integer;
      var Value: Single);
  private
    FCriticalSection: TCriticalSection;
    FConvolution: array [0 .. 1] of TLowLatencyConvolution32;
    FAdcHrtf: TAudioDataCollection32;
    FAdcIR: TAudioDataCollection32;
    FHRTFs: THrtfs;
    function GetConvolution(Index: Integer): TLowLatencyConvolution32;
  public
    procedure HRTFChanged;

    property AudioDataCollectionHRTF: TAudioDataCollection32 read FAdcHrtf;
    property Convolution[Index: Integer]: TLowLatencyConvolution32
      read GetConvolution;
    property HRTFs: THrtfs read FHRTFs;
    property CriticalSection: TCriticalSection read FCriticalSection
      write FCriticalSection;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Math, HrtfConvolverGui;

resourcestring
  RCStrIndexOutOfBouds = 'Index out of bouds(%d)';

procedure THrtfConvolverDataModule.VSTModuleCreate(Sender: TObject);
begin
  FCriticalSection := TCriticalSection.Create;
end;

procedure THrtfConvolverDataModule.VSTModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FCriticalSection);
end;

procedure THrtfConvolverDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
  RS: TResourceStream;
begin
  for Channel := 0 to Length(FConvolution) - 1 do
  begin
    FConvolution[Channel] := TLowLatencyConvolution32.Create;
    FConvolution[Channel].MinimumIRBlockOrder := CeilLog2(InitialDelay);
    FConvolution[Channel].MaximumIRBlockOrder := 18;
  end;

  // create and load HRTF data
  FHRTFs := THrtfs.Create;
  RS := TResourceStream.Create(hInstance, 'Default', 'HRTF');
  try
    FHRTFs.LoadFromStream(RS);
  finally
    RS.Free;
  end;

  // create HRTF audio data collection
  FAdcHrtf := TAudioDataCollection32.Create(Self);
  with FAdcHrtf do
  begin
    SampleFrames := FHRTFs.MinimumHrirSize;
    ChannelCount := 2;
    SampleRate := Self.SampleRate;
  end;

  FAdcIR := TAudioDataCollection32.Create(Self);
  with FAdcIR do
  begin
    SampleFrames := 1024;
    ChannelCount := 2;
    SampleRate := Self.SampleRate;
  end;

  EditorFormClass := TFmHrtfConvolver;
end;

procedure THrtfConvolverDataModule.VSTModuleClose(Sender: TObject);
begin
  FreeAndNil(FAdcHrtf);
  FreeAndNil(FAdcIR);
  FreeAndNil(FConvolution[0]);
  FreeAndNil(FConvolution[1]);
end;

function THrtfConvolverDataModule.GetConvolution(Index: Integer)
  : TLowLatencyConvolution32;
begin
  if Index in [0 .. 1] then
    result := FConvolution[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBouds, [Index]);
end;

procedure THrtfConvolverDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
  // lock processing
  FCriticalSection.Enter;
  try
    // FPreDelay[0].ProcessBlock
    FConvolution[0].ProcessBlock(@Inputs[0, 0], @Outputs[0, 0], SampleFrames);
    FConvolution[1].ProcessBlock(@Inputs[1, 0], @Outputs[1, 0], SampleFrames);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure THrtfConvolverDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
  HRTFChanged;
end;

procedure THrtfConvolverDataModule.ParameterAzimuthChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  HRTFChanged;
  if EditorForm is TFmHrtfConvolver then
    TFmHrtfConvolver(EditorForm).AzimuthChanged;
end;

procedure THrtfConvolverDataModule.ParameterElevationChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  HRTFChanged;
  if EditorForm is TFmHrtfConvolver then
    TFmHrtfConvolver(EditorForm).ElevationChanged;
end;

procedure THrtfConvolverDataModule.ParameterRadiusChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  HRTFChanged;
  if EditorForm is TFmHrtfConvolver then
    TFmHrtfConvolver(EditorForm).RadiusChanged;
end;

procedure THrtfConvolverDataModule.HRTFChanged;
const
  CDeg2Rad = 2 * Pi / 360;
begin
  if Assigned(FHRTFs) then
    with FAdcHrtf do
    begin
      FHRTFs.InterpolateHrir(Parameter[0] * CDeg2Rad, Parameter[1] * CDeg2Rad,
        SampleFrames, ChannelDataPointer[0], ChannelDataPointer[1]);

      // eventually resample data here

      FConvolution[0].LoadImpulseResponse(ChannelDataPointer[0], SampleFrames);
      FConvolution[1].LoadImpulseResponse(ChannelDataPointer[1], SampleFrames);
    end;
end;

end.
