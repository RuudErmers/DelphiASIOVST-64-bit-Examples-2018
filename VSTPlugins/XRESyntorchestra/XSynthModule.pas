unit XSynthModule;

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

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTEffect,   DAV_VSTModule, UIXPlugin;

type
  TVSTSSModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTModuleProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
      ParentWindow: NativeUInt);
    procedure VSTModuleEditClose(Sender: TObject; var DestroyForm: Boolean);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
    procedure VSTSSModulePrograms0StoreChunk(Sender: TObject;
      const Index: Integer; const isPreset: Boolean);
    procedure VSTSSModulePrograms0LoadChunk(Sender: TObject;
      const Index: Integer; const isPreset: Boolean);
    function VSTModuleGetChunkParameter(Sender: TObject;
      const Index: Integer): Single;
  private
    FCurTempo:double;
    FModel  : IXPlugin;
    function HostCallGetProductString (const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; override;
    function GetUniqueID: AnsiString; override;
    procedure CheckTempo;
  public

  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Approximations, DAV_VSTParameters,  Dialogs,DAV_VSTPrograms,  XPluginFactory;

procedure TVSTSSModule.VSTModuleOpen(Sender: TObject);
begin
  About:='VST Plugin by Christian Budde, Tobybear & Ruud Ermers';
  FModel:= XPluginFactory.CreateObject(self,SampleRate);
 // set editor form class
  EditorFormClass := FModel.GetFormClass;
end;

procedure TVSTSSModule.VSTModuleClose(Sender: TObject);
begin
  FModel.Close;
end;

procedure TVSTSSModule.VSTModuleEditClose(Sender: TObject;  var DestroyForm: Boolean);
begin
  FModel.SetEditor(NIL);
end;

procedure TVSTSSModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;  ParentWindow: NativeUInt);
begin
  FModel.SetEditor(GUI);
end;

function TVSTSSModule.VSTModuleGetChunkParameter(Sender: TObject;
  const Index: Integer): Single;
VAR k:integer;
begin
  k:=index;
  result:=(getTickCount MOD 100) / 100;
end;

procedure TVSTSSModule.CheckTempo;
var
  TimeInfo : PVstTimeInfo;
  Tempo:double;
begin
  TimeInfo := GetTimeInfo(32767);
  if VtiTempoValid in TimeInfo^.flags then
    begin
      Tempo := TimeInfo^.Tempo;
      if FCurTempo<>Tempo then
      begin
        FCurTempo:=Tempo;
        FModel.SetTempo(Tempo);
      end;
    end;
end;

procedure TVSTSSModule.VSTModuleProcess32Replacing(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
  CheckTempo;
  FModel.Process(Inputs,Outputs,SampleFrames);
end;

procedure TVSTSSModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
begin
  FModel.ProcessMidi(Sender,MidiEvent);
end;

procedure TVSTSSModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
  if FModel<>NIL then FModel.SetSampleRate(SampleRate);
end;


procedure TVSTSSModule.VSTSSModulePrograms0LoadChunk(Sender: TObject;
  const Index: Integer; const isPreset: Boolean);
begin
  if FModel<>NIL then
  begin
    (Sender as TVstProgram).Chunk.Position:=0;
    FModel.LoadFromStream((Sender as TVstProgram).Chunk,Index,IsPreset);
  end;
end;

procedure TVSTSSModule.VSTSSModulePrograms0StoreChunk(Sender: TObject;
  const Index: Integer; const isPreset: Boolean);
begin
  if FModel<>NIL then
    FModel.SaveToStream((Sender as TVstProgram).Chunk,Index,IsPreset);
end;

function TVSTSSModule.GetUniqueID: AnsiString;
begin
  SetUniqueID(XPluginFactory.UniqueID);
  result:=inherited;
end;

function TVSTSSModule.HostCallGetProductString(const Index: Integer;
  const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin
  SetProductName(XPluginFactory.ProductName);
  result:=inherited;
end;

end.

