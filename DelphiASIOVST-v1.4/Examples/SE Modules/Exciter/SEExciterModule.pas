unit SEExciterModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspExciter;

type
  // define some constants to make referencing in/outs clearer
  TSEExciterPins = (pinInput, pinOutput, pinInputLevel, pinLowFrequencyLevel,
    pinHighFrequencyLevel, pinHarmonicsLevel, pinFrequency);

  TCustomSEExciterModule = class(TSEModuleBase)
  private
    FInputBuffer        : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer       : PDAVSingleFixedArray;
    FStaticCount        : Integer;
    FInputLevel         : PDAVSingleFixedArray;
    FLowFrequencyLevel  : PDAVSingleFixedArray;
    FHighFrequencyLevel : PDAVSingleFixedArray;
    FHarmonicsLevel     : PDAVSingleFixedArray;
    FFrequency          : PDAVSingleFixedArray;

    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FExciter : TExciter;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEExciterModule = class(TCustomSEExciterModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEExciterAutomatedModule = class(TCustomSEExciterModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEExciterModule }

constructor TCustomSEExciterModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FExciter := TExciter.Create
end;

destructor TCustomSEExciterModule.Destroy;
begin
 FreeAndNil(FExciter);
 inherited;
end;

procedure TCustomSEExciterModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TCustomSEExciterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case TSEExciterPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
 end;
end;

// The most important part, processing the audio
procedure TCustomSEExciterModule.SampleRateChanged;
begin
 inherited;
 FExciter.SampleRate := SampleRate;
end;

procedure TCustomSEExciterModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEExciterModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSEExciterModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Exciter';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Exciter';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEExciterModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEExciterPins(index) of
  pinInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;


{ TSEExciterModule }

constructor TSEExciterModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
 inherited;
 GetMem(FInputLevel, SizeOf(Single));
 GetMem(FLowFrequencyLevel, SizeOf(Single));
 GetMem(FHighFrequencyLevel, SizeOf(Single));
 GetMem(FHarmonicsLevel, SizeOf(Single));
 GetMem(FFrequency, SizeOf(Single));
end;

destructor TSEExciterModule.Destroy;
begin
 Dispose(FInputLevel);
 Dispose(FLowFrequencyLevel);
 Dispose(FHighFrequencyLevel);
 Dispose(FHarmonicsLevel);
 Dispose(FFrequency);
 inherited;
end;

class procedure TSEExciterModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Exciter';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Exciter';
  end;
end;

function TSEExciterModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if Result = False then
  case TSEExciterPins(index) of
   pinInputLevel:
    with Properties^ do
     begin
      Name            := 'Input Level';
      VariableAddress := FInputLevel;
      Direction       := drIn;
      Datatype        := dtSingle;
      DefaultValue    := '1';
      Result          := True;
     end;
   pinLowFrequencyLevel:
    with Properties^ do
     begin
      Name            := 'Low Frequency Level';
      VariableAddress := FLowFrequencyLevel;
      Direction       := drIn;
      Datatype        := dtSingle;
      DefaultValue    := '1';
      Result          := True;
     end;
   pinHighFrequencyLevel:
    with Properties^ do
     begin
      Name            := 'High Frequency Level';
      VariableAddress := FHighFrequencyLevel;
      Direction       := drIn;
      Datatype        := dtSingle;
      DefaultValue    := '1';
      Result          := True;
     end;
   pinHarmonicsLevel:
    with Properties^ do
     begin
      Name            := 'HarmonicsLevel';
      VariableAddress := FHarmonicsLevel;
      Direction       := drIn;
      Datatype        := dtSingle;
      DefaultValue    := '1';
      Result          := True;
     end;
   pinFrequency:
    with Properties^ do
     begin
      Name            := 'Frequency [kHz]';
      VariableAddress := FFrequency;
      Direction       := drIn;
      Datatype        := dtSingle;
      DefaultValue    := '1';
      Result          := True;
     end;
  end;
end;

// An input plug has changed value
procedure TSEExciterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case TSEExciterPins(CurrentPin.PinID) of
  pinInputLevel         : FExciter.InputLevel := FInputLevel^[0];
  pinLowFrequencyLevel  : FExciter.LowFrequencyLevel := FLowFrequencyLevel^[0];
  pinHighFrequencyLevel : FExciter.HighFrequencyLevel:= FHighFrequencyLevel^[0];
  pinHarmonicsLevel     : FExciter.HarmonicsLevel := FHarmonicsLevel^[0];
  pinFrequency          : FExciter.Frequency := FFrequency^[0];
 end;
end;

procedure TSEExciterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FExciter.ProcessSample64(Inp^[Sample]);
end;

{ TSEExciterAutomatedModule }

class procedure TSEExciterAutomatedModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Exciter (automated)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Exciter (automated)';
  end;
end;

function TSEExciterAutomatedModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if Result = False then
  case TSEExciterPins(index) of
   pinInputLevel:
    with Properties^ do
     begin
      Name            := 'Input Level';
      VariableAddress := @FInputLevel;
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '1';
      Result          := True;
     end;
   pinLowFrequencyLevel:
    with Properties^ do
     begin
      Name            := 'Low Frequency Level';
      VariableAddress := @FLowFrequencyLevel;
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '1';
      Result          := True;
     end;
   pinHighFrequencyLevel:
    with Properties^ do
     begin
      Name            := 'High Frequency Level';
      VariableAddress := @FHighFrequencyLevel;
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '1';
      Result          := True;
     end;
   pinHarmonicsLevel:
    with Properties^ do
     begin
      Name            := 'HarmonicsLevel';
      VariableAddress := @FHarmonicsLevel;
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '1';
      Result          := True;
     end;
   pinFrequency:
    with Properties^ do
     begin
      Name            := 'Frequency [kHz]';
      VariableAddress := @FFrequency;
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '1';
      Result          := True;
     end;
  end;
end;

procedure TSEExciterAutomatedModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  IL     : PDAVSingleFixedArray;
  LFL    : PDAVSingleFixedArray;
  HFL    : PDAVSingleFixedArray;
  HL     : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 IL   := PDAVSingleFixedArray(@FInputLevel[BufferOffset]);
 LFL  := PDAVSingleFixedArray(@FLowFrequencyLevel[BufferOffset]);
 HFL  := PDAVSingleFixedArray(@FHighFrequencyLevel[BufferOffset]);
 HL   := PDAVSingleFixedArray(@FHarmonicsLevel[BufferOffset]);
 Freq := PDAVSingleFixedArray(@FFrequency[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outp^[Sample] := FExciter.ProcessSample64(Inp^[Sample]);
   FExciter.InputLevel := 10 * IL^[Sample];
   FExciter.LowFrequencyLevel := 10 * LFL^[Sample];
   FExciter.HighFrequencyLevel := 10 * HFL^[Sample];
   FExciter.HarmonicsLevel := 10 * HL^[Sample];
   FExciter.Frequency := 10000 * Freq^[Sample];
  end;
end;

end.
