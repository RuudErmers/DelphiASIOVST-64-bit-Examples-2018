unit SEBufferedMp3PlayerModule;

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
  Classes, SysUtils, SyncObjs, DAV_Types, DAV_SECommon, DAV_SEModule,
  DAV_DspBufferedMP3Player;

type
  // define some constants to make referencing in/outs clearer
  TSEBufferedMp3PlayerPins = (pinFileName, pinBufferSize, pinSemitones, pinInterpolation,
    pinReset, pinOutputLeft, pinOutputRight, pinTitle, pinArtist, pinAlbum,
    pinYear, pinComment);

  TSEBufferedMp3PlayerModule = class(TSEModuleBase)
  private
    FOutLeftBuffer   : PDAVSingleFixedArray;
    FOutRightBuffer  : PDAVSingleFixedArray;
    FFileName        : PAnsiChar;
    FPosition        : Integer;
    FReset           : Boolean;
    FBufferSize      : Integer;
    FSemitones       : Single;
    FInterpolation   : TBufferInterpolation;
    FTitle           : PAnsiChar;
    FArtist          : PAnsiChar;
    FAlbum           : PAnsiChar;
    FComment         : PAnsiChar;
    FYear            : PAnsiChar;
    FMemoryStream    : TMemoryStream;
    FCriticalSection : TCriticalSection;
    FBufferedPlayer  : TBufferedMP3StreamPlayer;
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

constructor TSEBufferedMp3PlayerModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FCriticalSection := TCriticalSection.Create;

 FBufferedPlayer := TBufferedMP3StreamPlayer.Create;
 FMemoryStream := TMemoryStream.Create;
end;

destructor TSEBufferedMp3PlayerModule.Destroy;
begin
 FreeAndNil(FCriticalSection);
 if Assigned(FBufferedPlayer)
  then FreeAndNil(FBufferedPlayer);
 if Assigned(FMemoryStream)
  then FreeAndNil(FMemoryStream);
 inherited;
end;

procedure TSEBufferedMp3PlayerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 ChooseProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutputLeft)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputRight)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEBufferedMp3PlayerModule.Close;
begin
 OnProcess := SubProcessBypass;
 inherited;
end;

procedure TSEBufferedMp3PlayerModule.ChooseProcess;
begin
 if not FileExists(FFileName)
  then OnProcess := SubProcessBypass
  else OnProcess := SubProcess
end;

procedure TSEBufferedMp3PlayerModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 inherited;
 case TSEBufferedMp3PlayerPins(CurrentPin.PinID) of
       pinFileName : begin
                      FCriticalSection.Enter;
                      try
                       if FileExists(FFileName) then
                        try
                         FMemoryStream.Clear;
                         FMemoryStream.Position := 0; 
                         FMemoryStream.LoadFromFile(FFilename);
                         FBufferedPlayer.Stream := FMemoryStream;
                         FPosition := 0;
                        except
                        end;

                       if Assigned(FBufferedPlayer.MpegAudio) then
                        with FBufferedPlayer.MpegAudio do
                         begin
                          if Assigned(FTitle)   then StrPCopy(FTitle, Id3Title);
                          if Assigned(FArtist)  then StrPCopy(FArtist, Id3Artist);
                          if Assigned(FAlbum)   then StrPCopy(FAlbum, Id3Album);
                          if Assigned(FYear)    then StrPCopy(FYear, Id3Year);
                          if Assigned(FComment) then StrPCopy(FComment, Id3Comment);
                         end;

                       ChooseProcess;
                      finally
                       FCriticalSection.Leave;
                      end;
                     end;
     pinBufferSize : begin
                      if FBufferSize < 1024 then FBufferSize := 1024 else
                      if FBufferSize > 65536 then FBufferSize := 65536;
                      FBufferedPlayer.BufferSize := FBufferSize;
                      FBufferedPlayer.BlockSize := FBufferSize div 4;
                     end;
      pinSemitones : begin
                      FBufferedPlayer.Pitch := FSemitones;
                     end;
  pinInterpolation : begin
                      FBufferedPlayer.Interpolation := FInterpolation;
                     end;
          pinReset : if FReset then
                      begin
                       FPosition := 0;
                       FReset := False;
                       if Assigned(FBufferedPlayer)
                        then FBufferedPlayer.Reset;
                       Pin[Integer(pinReset)].TransmitStatusChange(SampleClock, stOneOff);
                      end;
 end;
end;

// The most important part, processing the audio
procedure TSEBufferedMp3PlayerModule.SampleRateChanged;
begin
 inherited;
 FBufferedPlayer.SampleRate := SampleRate;
end;

procedure TSEBufferedMp3PlayerModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 FCriticalSection.Enter;
 try
  FBufferedPlayer.GetSamples(@FOutLeftBuffer^[BufferOffset], @FOutRightBuffer^[BufferOffset], SampleFrames)
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSEBufferedMp3PlayerModule.SubProcessBypass(
  const BufferOffset, SampleFrames: Integer);
begin
 FillChar(FOutLeftBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 FillChar(FOutRightBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
end;

// describe your module
class procedure TSEBufferedMp3PlayerModule.GetModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   Name := 'Buffered MP3 Player';
   ID := 'DAV Buffered MP3 Player';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEBufferedMp3PlayerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEBufferedMp3PlayerPins(index) of
       pinFileName : with Properties^ do
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
      pinSemitones : with Properties^ do
                      begin
                       Name            := 'Semitones';
                       VariableAddress := @FSemitones;
                       Direction       := drIn;
                       Datatype        := dtSingle;
                      end;
  pinInterpolation : with Properties^ do
                      begin
                       Name            := 'Interpolation';
                       VariableAddress := @FInterpolation;
                       Direction       := drIn;
                       Datatype        := dtEnum;
                       DatatypeExtra   := 'none, linear, hermite, bspline';
                       DefaultValue    := 'none';
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
          pinTitle : with Properties^ do
                      begin
                       Name            := 'Title';
                       VariableAddress := @FTitle;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
          pinArtist : with Properties^ do
                      begin
                       Name            := 'Artist';
                       VariableAddress := @FArtist;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
          pinAlbum : with Properties^ do
                      begin
                       Name            := 'Album';
                       VariableAddress := @FAlbum;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
           pinYear : with Properties^ do
                      begin
                       Name            := 'Year';
                       VariableAddress := @FYear;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
        pinComment : with Properties^ do
                      begin
                       Name            := 'Comment';
                       VariableAddress := @FComment;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
