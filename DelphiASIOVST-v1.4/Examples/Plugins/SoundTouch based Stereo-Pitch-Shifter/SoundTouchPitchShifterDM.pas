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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTModule, DAV_SoundTouchDLL, DAV_ChannelDataCoder; //DAV_SoundTouch;

type
  TSoundTouchPitchShifterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPitchFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleBlockSizeChange(Sender: TObject;
      const BlockSize: Integer);
  private
    FSoundTouch      : TSoundTouch;
    FDataCoder       : TChannel32DataCoderFloat32;
    FInterleavedData : PDAVSingleFixedArray;
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

procedure TSoundTouchPitchShifterModule.VSTModuleOpen(Sender: TObject);
begin
 FSoundTouch := TSoundTouch.Create;
 with FSoundTouch do
  begin
   SampleRate := Self.SampleRate;
   Channels := 2;
  end;
 FDataCoder := TChannel32DataCoderFloat32.Create;

 // initialize parameter
 Parameter[0] := 1;

 // set editor form class
 EditorFormClass := TFmSoundTouchPitchShifter;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleBlockSizeChange(
  Sender: TObject; const BlockSize: Integer);
begin
 ReallocMem(FInterleavedData, 2 * BlockSize * SizeOf(Single));
 FDataCoder.BlockSize := 2 * BlockSize * SizeOf(Single);
end;

procedure TSoundTouchPitchShifterModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSoundTouch);
 FreeAndNil(FDataCoder);
end;

procedure TSoundTouchPitchShifterModule.ParameterPitchFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoundTouch)
  then FSoundTouch.Pitch := Power(2, Value / 12);

 // update GUI
 if EditorForm is TFmSoundTouchPitchShifter
  then TFmSoundTouchPitchShifter(EditorForm).UpdateSemitones;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(FSoundTouch)
  then FSoundTouch.SampleRate := SampleRate;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
 FDataCoder.BlockSize := 2 * SampleFrames * SizeOf(Single);
 Move(Inputs[0, 0], FDataCoder.ChannelPointer[0]^, SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], FDataCoder.ChannelPointer[1]^, SampleFrames * SizeOf(Single));
 FDataCoder.SaveToPointer(FInterleavedData);
 FSoundTouch.PutSamples(@FInterleavedData^[0], SampleFrames);
 FSoundTouch.ReceiveSamples(@FInterleavedData^[0], SampleFrames);
 FDataCoder.LoadFromPointer(FInterleavedData);
 Move(FDataCoder.ChannelPointer[0]^, Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(FDataCoder.ChannelPointer[1]^, Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

end.
