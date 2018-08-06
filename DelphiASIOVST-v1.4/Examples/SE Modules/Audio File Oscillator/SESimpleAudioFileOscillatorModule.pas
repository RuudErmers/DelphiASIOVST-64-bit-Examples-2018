unit SESimpleAudioFileOscillatorModule;

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
//  SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseEmbedding}Windows, Classes, {$ENDIF} SysUtils, SyncObjs,
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_AudioData,
  DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  // define some constants to make referencing in/outs clearer
  TSEAudioFileOscillatorPins = (pinFileName, pinReset, pinOutput);

  TSESimpleAudioFileOscillatorModule = class(TSEModuleBase)
  private
    FOutputBuffer    : PDAVSingleFixedArray;
    FAudioData       : TAudioDataCollection32;
    FFileName        : TFileName;
    FPosition        : Integer;
    FReset           : Boolean;
    FCriticalSection : TCriticalSection;
    {$IFDEF UseEmbedding}
    FContainedData   : TStringList;
    procedure LoadFromResource(ID: Integer);
    {$ENDIF}
  protected
    procedure Open; override;
    procedure ChooseProcess; virtual;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessBypass(const BufferOffset, SampleFrames: Integer);
  end;

implementation

{$IFDEF UseEmbedding}
function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;
{$ENDIF}

constructor TSESimpleAudioFileOscillatorModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FAudioData := TAudioDataCollection32.Create(nil);
 FCriticalSection := TCriticalSection.Create;

 {$IFDEF UseEmbedding}
 FContainedData := TStringList.Create;
 EnumResourceNames(HInstance, 'WAVETABLE', @EnumNamesFunc, LongWord(FContainedData));

 if FContainedData.Count > 0
  then Integer(FFileName) := 0
  else FFileName := '';
 {$ENDIF}
end;

destructor TSESimpleAudioFileOscillatorModule.Destroy;
begin
 {$IFDEF UseEmbedding}
 FreeAndNil(FContainedData);
 {$ENDIF}
 FreeAndNil(FCriticalSection);
 FreeAndNil(FAudioData);
 inherited;
end;

procedure TSESimpleAudioFileOscillatorModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 ChooseProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSESimpleAudioFileOscillatorModule.ChooseProcess;
begin
 if {$IFDEF UseEmbedding}(FContainedData.Count = 0) and {$ENDIF} (not FileExists(FFileName))
  then OnProcess := SubProcessBypass
  else OnProcess := SubProcess
end;

procedure TSESimpleAudioFileOscillatorModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 inherited;
 case TSEAudioFileOscillatorPins(CurrentPin.PinID) of
  pinFileName : begin
                 FCriticalSection.Enter;
                 try
                  {$IFDEF UseEmbedding}
                  if FContainedData.Count > 0
                   then LoadFromResource(Integer(FFileName))
                   else
                  {$ENDIF}
                    if FileExists(FFileName) then
                     try
                      FAudioData.LoadFromFile(FFileName);
                      FPosition := 0;
                     except
                     end;

                  ChooseProcess;
                 finally
                  FCriticalSection.Leave
                 end;
                end;
     pinReset : if FReset then
                 begin
                  FPosition := 0;
                  FReset := False;
                  Pin[Integer(pinReset)].TransmitStatusChange(SampleClock, stOneOff);
                 end;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSESimpleAudioFileOscillatorModule.LoadFromResource(ID: Integer);
var
  RS  : TResourceStream;
begin
 if (ID >= 0) and (ID < FContainedData.Count) then
  begin
   RS := TResourceStream.Create(HInstance, FContainedData[ID], 'WAVETABLE');
   try
    FAudioData.LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;
  end;
end;
{$ENDIF}

// The most important part, processing the audio
procedure TSESimpleAudioFileOscillatorModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     FOutputBuffer[BufferOffset + Sample] := FAudioData[0].ChannelDataPointer^[FPosition];
     inc(FPosition);
     if FPosition >= FAudioData[0].SampleCount
      then FPosition := 0;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSESimpleAudioFileOscillatorModule.SubProcessBypass(
  const BufferOffset, SampleFrames: Integer);
begin
 FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
end;

// describe your module
class procedure TSESimpleAudioFileOscillatorModule.getModuleProperties(Properties : PSEModuleProperties);
{$IFDEF UseEmbedding}
var
  ContainedData : TStringList;
  i             : Integer;
  str           : string;
{$ENDIF}
begin
 {$IFDEF UseEmbedding}
 ContainedData := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'WAVETABLE', @EnumNamesFunc, LongWord(ContainedData));
  {$ENDIF}
  with Properties^ do
   begin
    {$IFDEF UseEmbedding}
    if ContainedData.Count > 0 then
     begin
      Name := 'Embedded Simple Audio File Osc.';
      str  := 'DAV ESAFO';
      for i := 0 to ContainedData.Count - 1
       do str := str + ContainedData[i];
      ID := PAnsiChar(str);
     end
    else
    {$ENDIF}
     begin
      Name := 'Simple Audio File Oscillator';
      ID := 'DAV Simple Audio File Oscillator';
     end;

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';
    SDKVersion := CSeSdkVersion;
   end;
 {$IFDEF UseEmbedding}
 finally
  FreeAndNil(ContainedData);
 end;
 {$ENDIF}
end;

// describe the pins (plugs)
function TSESimpleAudioFileOscillatorModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
{$IFDEF UseEmbedding}
var
  str : AnsiString;
{$ENDIF}
begin
 Result := True;
 case TSEAudioFileOscillatorPins(index) of
  pinFileName : with Properties^ do
                 {$IFDEF UseEmbedding}
                 if FContainedData.Count > 0 then
                  begin
                   Name            := 'Wavetable ID';
                   VariableAddress := @FFileName;
                   Direction       := drIn;
                   DataType        := dtEnum;
                   DefaultValue    := '0';
                   str             := 'range 0,' + IntToStr(FContainedData.Count - 1);
                   DatatypeExtra   := PAnsiChar(str);
                  end
                 else
                 {$ENDIF}
                  begin
                   Name            := 'FileName';
                   VariableAddress := @FFileName;
                   Flags           := [iofFilename];
                   Direction       := drIn;
                   DataType        := dtText;
                  end;
     pinReset : with Properties^ do
                 begin
                  Name            := 'Reset';
                  VariableAddress := @FReset;
                  Direction       := drIn;
                  Datatype        := dtBoolean;
                 end;
    pinOutput : with Properties^ do
                 begin
                  Name            := 'Output';
                  VariableAddress := @FOutputBuffer;
                  Direction       := drOut;
                  Datatype        := dtFSample;
                 end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
