unit DAV_VSTHostAudioData;

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

{$I ..\DAV_Compiler.inc}

{$IFNDEF FPC}{$DEFINE MemDLL}{$ENDIF}

uses
  {$IFDEF FPC} LCLIntf, LResources, Dynlibs, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, DAV_Types, DAV_VstHost, DAV_AudioData;

type
  {$IFDEF DELPHI10_UP} {$region 'TVstPlugIn'} {$ENDIF}
  TCustomVstPlugInAudioData = class(TCustomVstPlugIn)
  public
    procedure ProcessAudioDataCollection(Inputs, Outputs: TAudioDataCollection32); overload; virtual;
    procedure ProcessAudioDataCollection(Inputs, Outputs: TAudioDataCollection64); overload; virtual;
    procedure ProcessAudioDataCollectionInplace(AudioData: TAudioDataCollection32); overload; virtual;
    procedure ProcessAudioDataCollectionInplace(AudioData: TAudioDataCollection64); overload; virtual;
  end;

  TVstPlugInAudioData = class(TCustomVstPlugInAudioData)
  published
    property Active;
    property AutomationState;
    property CurrentProcessLevel;
    property DisplayName;
    property DLLFileName;
    property EditVisible;
    property EffectOptions;
    property InitialDelay;
    property numInputs;
    property numOutputs;
    property numParams;
    property numPrograms;
    property PlugCategory;
    property PluginVstVersion;
    property ProductString;
    property ProgramName;
    property CurrentProgram;
    property ReplaceOrAccumulate;
    property UniqueID;
    property VendorString;
    property VendorVersion;
    property Version;
    property VstOfflineTasks;
    property OnAfterLoad;
    property OnAudioMasterAutomate;
    property OnAudioMasterBeginEdit;
    property OnAudioMasterEndEdit;
    property OnAudioMasterIdle;
    property OnAudioMasterIOChanged;
    property OnAudioMasterNeedIdle;
    property OnAudioMasterOfflineGetCurrentMetaPass;
    property OnAudioMasterOfflineGetCurrentPass;
    property OnAudioMasterOfflineRead;
    property OnAudioMasterOfflineStart;
    property OnAudioMasterOfflineWrite;
    property OnAudioMasterPinConnected;
    property OnAudioMasterSetOutputsampleRate;
    property OnAudioMasterUpdateDisplay;
    property OnAudioMasterWantMidi;
    property OnProcessEvents;
    property OnVendorSpecific;
    {$IFDEF VstHostGUI}
    property GUIStyle;
    property OnCloseEdit;
    property OnShowEdit;
    {$ENDIF}
  end;

  TVstPlugInsAudioData = class(TVstPlugIns)
  private
    FOwner: TComponent;
    function GetVSTHost: TCustomVstHost;
    function GetItem(Index: Integer): TVstPlugInAudioData;
    procedure SetItem(Index: Integer; const Value: TVstPlugInAudioData);
  protected
    property Items[Index: Integer]: TVstPlugInAudioData read GetItem write SetItem; default;
    property VstHost: TCustomVstHost read GetVSTHost;
  public
    constructor Create(AOwner: TComponent);
    function Add: TVstPlugInAudioData;
    function CloneAdd(Source: TVstPlugInAudioData): TVstPlugInAudioData;
    function Insert(Index: Integer): TVstPlugInAudioData;
    procedure Delete(Index: Integer);
    property Count;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TVstPlugIn'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TVstHost'} {$ENDIF}
  TCustomVstHostAudioData = class(TCustomVstHost)
  protected
    procedure CreateVstPluginList; override;
  end;

  TVstHostAudioData = class(TCustomVstHost)
  published
    property BlockSize;
    property CanDos;
    property Language;
    property LatencyInput;
    property LatencyOutput;
    property ManageIdleAutomaticly;
    property NumAutomatableParameters;
    property OnCreate;
    property OnDestroy;
    property ParameterQuantization;
    property PlugInDir;
    property ProductString;
    property Tempo;
    property VendorString;
    property VendorVersion;
    property VstPlugIns;
    property VstTimeInfo;
    property VstVersion;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TVstHost'} {$ENDIF}

implementation

uses
  SysUtils, DAV_Common, DAV_VSTEffect;

procedure TCustomVstPlugInAudioData.ProcessAudioDataCollection(Inputs,
  Outputs: TAudioDataCollection32);
var
  Channel : Integer;
  InList,
  OutList : array of PDAVSingleFixedArray;
begin
 Assert(Inputs.SampleFrames = Outputs.SampleFrames);
 Assert(Inputs.ChannelCount >= numInputs);
 Assert(Outputs.ChannelCount >= numOutputs);

 SetLength(InList, Inputs.ChannelCount);
 for Channel := 0 to Inputs.ChannelCount - 1
  do InList[Channel] := Inputs[Channel].ChannelDataPointer;

 SetLength(OutList, Outputs.ChannelCount);
 for Channel := 0 to Outputs.ChannelCount - 1
  do OutList[Channel] := Outputs[Channel].ChannelDataPointer;
 ProcessAudio(PPSingle(InList), PPSingle(OutList), Inputs.SampleFrames);
end;

procedure TCustomVstPlugInAudioData.ProcessAudioDataCollection(Inputs,
  Outputs: TAudioDataCollection64);
var
  Channel : Integer;
  InList,
  OutList : array of PDAVDoubleFixedArray;
begin
 Assert(Inputs.SampleFrames = Outputs.SampleFrames);
 Assert(Inputs.ChannelCount >= numInputs);
 Assert(Outputs.ChannelCount >= numOutputs);

 SetLength(InList, Inputs.ChannelCount);
 for Channel := 0 to Inputs.ChannelCount - 1
  do InList[Channel] := Inputs[Channel].ChannelDataPointer;

 SetLength(OutList, Outputs.ChannelCount);
 for Channel := 0 to Outputs.ChannelCount - 1
  do OutList[Channel] := Outputs[Channel].ChannelDataPointer;
 Process64Replacing(PPDouble(InList), PPDouble(OutList), Inputs.SampleFrames);
end;

procedure TCustomVstPlugInAudioData.ProcessAudioDataCollectionInplace(
  AudioData: TAudioDataCollection32);
var
  Channel  : Integer;
  DataList : array of PDAVSingleFixedArray;
begin
 Assert(AudioData.ChannelCount >= numInputs);
 Assert(AudioData.ChannelCount >= numOutputs);

 SetLength(DataList, AudioData.ChannelCount);
 for Channel := 0 to AudioData.ChannelCount - 1
  do DataList[Channel] := AudioData[Channel].ChannelDataPointer;

 ProcessAudio(PPSingle(DataList), PPSingle(DataList), AudioData.SampleFrames);
end;

procedure TCustomVstPlugInAudioData.ProcessAudioDataCollectionInplace(
  AudioData: TAudioDataCollection64);
var
  Channel  : Integer;
  DataList : array of PDAVDoubleFixedArray;
begin
 Assert(AudioData.ChannelCount >= numInputs);
 Assert(AudioData.ChannelCount >= numOutputs);

 SetLength(DataList, AudioData.ChannelCount);
 for Channel := 0 to AudioData.ChannelCount - 1
  do DataList[Channel] := AudioData[Channel].ChannelDataPointer;

 Process64Replacing(PPDouble(DataList), PPDouble(DataList), AudioData.SampleFrames);
end;


////////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TVstPlugInAudioData implementation'} {$ENDIF}

{ TVstPlugInsAudioData }

function TVstPlugInsAudioData.Add: TVstPlugInAudioData;
begin
  Result := TVstPlugInAudioData(inherited Add);
end;

function TVstPlugInsAudioData.CloneAdd(Source: TVstPlugInAudioData): TVstPlugInAudioData;
begin
 Result := TVstPlugInAudioData(inherited Add);
 Source.AssignTo(Result);
end;

constructor TVstPlugInsAudioData.Create(AOwner: TComponent);
begin
// inherited Create(AOwner, TVstPlugInAudioData);
 raise Exception.Create('Problem yet to solve! - not implemented');
 FOwner := AOwner;
end;

function TVstPlugInsAudioData.GetItem(Index: Integer): TVstPlugInAudioData;
begin
 Result := TVstPlugInAudioData(inherited GetItem(Index));
end;

function TVstPlugInsAudioData.GetVSTHost: TCustomVstHost;
begin
 Result := TCustomVstHost(FOwner);
end;

function TVstPlugInsAudioData.Insert(Index: Integer): TVstPlugInAudioData;
begin
 Result := TVstPlugInAudioData(inherited Insert(Index));
end;

procedure TVstPlugInsAudioData.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

procedure TVstPlugInsAudioData.SetItem(Index: Integer; const Value: TVstPlugInAudioData);
begin
 inherited SetItem(Index, Value);
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////


{ TCustomVstHostAudioData }

procedure TCustomVstHostAudioData.CreateVstPluginList;
begin
 FVstPlugIns := TVstPlugInsAudioData.Create(Self);
end;

end.
