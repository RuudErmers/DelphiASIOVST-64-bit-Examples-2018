unit SESimpleMp3PlayerModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspBufferedMP3Player;

type
  // define some constants to make referencing in/outs clearer
  TSEMp3PlayerPins = (pinFileName, pinBufferSize, pinReset, 
    pinOutputLeft, pinOutputRight);

  TSESimpleMp3PlayerModule = class(TSEModuleBase)
  private
    FOutLeftBuffer   : PDAVSingleFixedArray;
    FOutRightBuffer  : PDAVSingleFixedArray;
    FFileName        : PAnsiChar;
    FPosition        : Integer;
    FReset           : Boolean;
    FBufferSize      : Integer;
    FCriticalSection : TCriticalSection;
    {$IFDEF UseEmbedding}
    FBufferedPlayer  : TBufferedMP3StreamPlayer;
    FResourceStream  : TResourceStream;
    FContainedData   : TStringList;
    procedure LoadFromResource(ID: Integer);
    {$ELSE}
    FBufferedPlayer  : TBufferedMP3FilePlayer;
    {$ENDIF}
  protected
    procedure Open; override;
    procedure Close; override;
    procedure ChooseProcess; virtual;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
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

constructor TSESimpleMp3PlayerModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FCriticalSection := TCriticalSection.Create;

 {$IFDEF UseEmbedding}
 FBufferedPlayer := TBufferedMP3StreamPlayer.Create;
 FContainedData := TStringList.Create;
 EnumResourceNames(HInstance, 'MP3', @EnumNamesFunc, LongWord(FContainedData));

 if FContainedData.Count > 0
  then Integer(FFileName) := 0
  else FFileName := '';
 {$ELSE}
 FBufferedPlayer := TBufferedMP3FilePlayer.Create;
 {$ENDIF}
end;

destructor TSESimpleMp3PlayerModule.Destroy;
begin
 {$IFDEF UseEmbedding}
 FreeAndNil(FContainedData);
 {$ENDIF}
 FreeAndNil(FCriticalSection);
 if Assigned(FBufferedPlayer)
  then FreeAndNil(FBufferedPlayer);
 inherited;
end;

procedure TSESimpleMp3PlayerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 ChooseProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutputLeft)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputRight)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSESimpleMp3PlayerModule.Close;
begin
 OnProcess := SubProcessBypass;
 inherited;
end;

procedure TSESimpleMp3PlayerModule.ChooseProcess;
begin
 if {$IFDEF UseEmbedding}(FContainedData.Count = 0) {$ELSE} (not FileExists(FFileName)) {$ENDIF}
  then OnProcess := SubProcessBypass
  else OnProcess := SubProcess
end;

procedure TSESimpleMp3PlayerModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 inherited;
 case TSEMp3PlayerPins(CurrentPin.PinID) of
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
                        FBufferedPlayer.Filename := FFileName;
                        FPosition := 0;
                       except
                       end;

                    ChooseProcess;
                   finally
                    FCriticalSection.Leave;
                   end;
                  end;
       pinReset : if FReset then
                   begin
                    FPosition := 0;
                    FReset := False;
                    if Assigned(FBufferedPlayer)
                     then FBufferedPlayer.Reset;
                    Pin[Integer(pinReset)].TransmitStatusChange(SampleClock, stOneOff);
                   end;
  pinBufferSize : begin
                   if FBufferSize < 1024 then FBufferSize := 1024 else
                   if FBufferSize > 65536 then FBufferSize := 65536;
                   FBufferedPlayer.BufferSize := FBufferSize;
                   FBufferedPlayer.BlockSize := FBufferSize div 4;
                  end;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSESimpleMp3PlayerModule.LoadFromResource(ID: Integer);
begin
 if (ID >= 0) and (ID < FContainedData.Count) then
  begin
   if Assigned(FResourceStream) then FreeAndNil(FResourceStream)
   FResourceStream := TResourceStream.Create(HInstance, FContainedData[ID], 'MP3');
   FBufferedPlayer.Stream := FResourceStream;
  end;
end;
{$ENDIF}

// The most important part, processing the audio
procedure TSESimpleMp3PlayerModule.SampleRateChanged;
begin
 inherited;
 FBufferedPlayer.SampleRate := SampleRate;
end;

procedure TSESimpleMp3PlayerModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 FCriticalSection.Enter;
 try
  FBufferedPlayer.GetSamples(@FOutLeftBuffer^[BufferOffset], @FOutRightBuffer^[BufferOffset], SampleFrames)
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSESimpleMp3PlayerModule.SubProcessBypass(
  const BufferOffset, SampleFrames: Integer);
begin
 FillChar(FOutLeftBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 FillChar(FOutRightBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
end;

// describe your module
class procedure TSESimpleMp3PlayerModule.GetModuleProperties(Properties : PSEModuleProperties);
{$IFDEF UseEmbedding}
var
  ContainedData : TStringList;
  i             : Integer;
  str           : AnsiString;
{$ENDIF}
begin
 {$IFDEF UseEmbedding}
 ContainedData := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'MP3', @EnumNamesFunc, LongWord(ContainedData));
  {$ENDIF}
  with Properties^ do
   begin
    {$IFDEF UseEmbedding}
    if ContainedData.Count > 0 then
     begin
      Name := 'Embedded Simple MP3 Player';
      str  := 'DAV ESMP3';
      for i := 0 to ContainedData.Count - 1
       do str := str + ContainedData[i];
      ID := PAnsiChar(str);
     end
    else
    {$ENDIF}
     begin
      Name := 'Simple MP3 Player';
      ID := 'DAV Simple MP3 Player';
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
function TSESimpleMp3PlayerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
{$IFDEF UseEmbedding}
var
  str : AnsiString;
{$ENDIF}
begin
 Result := True;
 case TSEMp3PlayerPins(index) of
       pinFileName : with Properties^ do
                      {$IFDEF UseEmbedding}
                      if FContainedData.Count > 0 then
                       begin
                        Name            := 'MP3 ID';
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
     pinBufferSize : with Properties^ do
                      begin
                       Name            := 'BufferSize';
                       VariableAddress := @FBufferSize;
                       Direction       := drIn;
                       Datatype        := dtInteger;
                      end;
          pinReset : with Properties^ do
                      begin
                       Name            := 'Reset';
                       VariableAddress := @FReset;
                       Direction       := drIn;
                       Datatype        := dtBoolean;
                      end;
     pinOutputLeft : with Properties^ do
                      begin
                       Name            := 'Left';
                       VariableAddress := @FOutLeftBuffer;
                       Direction       := drOut;
                       Datatype        := dtFSample;
                      end;
    pinOutputRight : with Properties^ do
                      begin
                       Name            := 'Right';
                       VariableAddress := @FOutRightBuffer;
                       Direction       := drOut;
                       Datatype        := dtFSample;
                      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
