unit ThirdOctaveAnalyserDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspFilterChebyshevType1,
  DAV_CustomDataContainer, DAV_DataContainerThirdOctave,
  DAV_DspThirdOctaveAnalyser, DAV_DspThirdOctaveAnalyserFilter;

type
  TThirdOctaveAnalyserModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterSmoothChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFullscaleGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDownsamplingDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterDownsamplingChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FDownsampling : Boolean;
    FFSGain       : Single;
    function GetBandRMS(Index: Integer): Single;
    procedure SetDownsampling(const Value: Boolean);
  protected
    FFilterAnalysis  : TCustomThirdOctaveAnalyserFilter;
    FCriticalSection : TCriticalSection;
    procedure DownsamplingChanged;
  public
    property BandRMS[Index: Integer]: Single read GetBandRMS;
    property Downsampling: Boolean read FDownsampling write SetDownsampling;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_VSTCustomModule, ThirdOctaveAnalyserGUI;

procedure TThirdOctaveAnalyserModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TThirdOctaveAnalyserModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TThirdOctaveAnalyserModule.VSTModuleOpen(Sender: TObject);
begin
 FFilterAnalysis := TThirdOctaveAnalyserFilter.Create;
 FFSGain := 0;

 // initialize parameters
 Parameter[0] := 0.99;
 Parameter[1] := 90;
 Parameter[2] := 1;

 with Programs[0] do
  begin
   Parameter[0] := 0.99;
   Parameter[1] := 90;
   Parameter[2] := 1;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 0.999;
   Parameter[1] := 90;
   Parameter[2] := 1;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 0.9999;
   Parameter[1] := 90;
   Parameter[2] := 1;
  end;

 // set editor form class
 EditorFormClass := TFmThirdOctaveAnalyser;
end;

procedure TThirdOctaveAnalyserModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FFilterAnalysis);
end;

function TThirdOctaveAnalyserModule.GetBandRMS(Index: Integer): Single;
begin
 Result := FFilterAnalysis.BandData[Index];
end;

procedure TThirdOctaveAnalyserModule.ParameterDownsamplingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FFilterAnalysis)
   then Downsampling := Value > 0.5;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TThirdOctaveAnalyserModule.ParameterDownsamplingDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'Off'
  else PreDefined := 'On';
end;

procedure TThirdOctaveAnalyserModule.ParameterFullscaleGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFSGain := Value;

 // update GUI
 if EditorForm is TFmThirdOctaveAnalyser
  then TFmThirdOctaveAnalyser(EditorForm).UpdateFullscaleGain;
end;

procedure TThirdOctaveAnalyserModule.ParameterSmoothChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FFilterAnalysis)
   then FFilterAnalysis.SmoothingFactor := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TThirdOctaveAnalyserModule.SetDownsampling(const Value: Boolean);
begin
 if FDownsampling <> Value then
  begin
   FDownsampling := Value;
   DownsamplingChanged;
  end;
end;

procedure TThirdOctaveAnalyserModule.DownsamplingChanged;
begin
 FCriticalSection.Enter;
 try
  FreeAndNil(FFilterAnalysis);

  if FDownsampling
   then FFilterAnalysis := TThirdOctaveAnalyserFilterDownSampling.Create
   else FFilterAnalysis := TThirdOctaveAnalyserFilter.Create;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TThirdOctaveAnalyserModule.VSTModuleProcessReplacing(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1
   do FFilterAnalysis.ProcessSample32(Inputs[0, SampleIndex]);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
