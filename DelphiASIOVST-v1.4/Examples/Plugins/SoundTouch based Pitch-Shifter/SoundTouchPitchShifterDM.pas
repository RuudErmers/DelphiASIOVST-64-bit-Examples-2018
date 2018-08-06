unit SoundTouchPitchShifterDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspSoundTouch;

type
  TSoundTouchPitchShifterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPitchFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FSoundTouch      : array [0..1] of TDspSoundTouch;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Dialogs, SoundTouchPitchShifterGUI, DAV_VSTCustomModule;

procedure TSoundTouchPitchShifterModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSoundTouchPitchShifterModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to NumInputs - 1 do
  begin
   FSoundTouch[Channel] := TDspSoundTouch.Create;
   with FSoundTouch[Channel] do
    begin
     Samplerate := Self.Samplerate;
     Channels := 1;
    end;
  end;
 Parameter[0] := 1;
end;

procedure TSoundTouchPitchShifterModule.ParameterPitchFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to NumInputs - 1 do
   if Assigned(FSoundTouch[Channel])
    then FSoundTouch[Channel].Pitch := Power(2, Value / 12);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmSoundTouchPitchShifter
  then TFmSoundTouchPitchShifter(EditorForm).UpdateSemitones;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to NumInputs - 1
  do FreeAndNil(FSoundTouch[Channel]);
end;

procedure TSoundTouchPitchShifterModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSoundTouchPitchShifter.Create(Self);
end;

procedure TSoundTouchPitchShifterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for Channel := 0 to NumInputs - 1 do
    if Assigned(FSoundTouch[Channel])
     then FSoundTouch[Channel].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to NumInputs - 1 do
   begin
    FSoundTouch[Channel].WriteSamples(@Inputs[Channel, 0], SampleFrames);
    FSoundTouch[Channel].ReadSamples(@Outputs[Channel, 0], SampleFrames);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
