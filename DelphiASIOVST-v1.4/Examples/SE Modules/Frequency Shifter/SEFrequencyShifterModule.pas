unit SEFrequencyShifterModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspFrequencyShifter;

type
  // define some constants to make referencing in/outs clearer
  TSEFrequencyShifterPins = (pinInput, pinUpshift, pinDownshift,
    pinFrequency, pinCoefficientCount, pinTransitionBandwidth);

  TCustomSEFrequencyShifterModule = class(TSEModuleBase)
  private
    FInputBuffer      : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FUpshiftBuffer    : PDAVSingleFixedArray;
    FDownshiftBuffer  : PDAVSingleFixedArray;
    FStaticCount      : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FFrequencyShifter : TBodeFrequencyShifter32;
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

  TSEFrequencyShifterStaticModule = class(TCustomSEFrequencyShifterModule)
  private
    FCoefficientCount : Integer;
    FFrequency        : Single;
    FTransitionBW     : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEFrequencyShifterControllableModule = class(TSEFrequencyShifterStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEFrequencyShifterModule }

constructor TCustomSEFrequencyShifterModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FFrequencyShifter := TBodeFrequencyShifter32.Create
end;

destructor TCustomSEFrequencyShifterModule.Destroy;
begin
 FreeAndNil(FFrequencyShifter);
 inherited;
end;

procedure TCustomSEFrequencyShifterModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEFrequencyShifterModule.SampleRateChanged;
begin
 inherited;
 FFrequencyShifter.SampleRate := SampleRate;
end;

procedure TCustomSEFrequencyShifterModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEFrequencyShifterModule.ChooseProcess;
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
class procedure TCustomSEFrequencyShifterModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEFrequencyShifterModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEFrequencyShifterPins(index) of
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
  pinUpshift:
   with Properties^ do
    begin
     Name            := 'Upshift';
     VariableAddress := @FUpshiftBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinDownshift:
   with Properties^ do
    begin
     Name            := 'Downshift';
     VariableAddress := @FDownshiftBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEFrequencyShifterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEFrequencyShifterPins(CurrentPin.PinID) of
  pinInput: begin
             ChooseProcess;
             Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
            end;
 end;
end;


{ TSEFrequencyShifterStaticModule }

// describe your module
class procedure TSEFrequencyShifterStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Bode Frequency Shifter (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Bode Frequency Shifter (static)';
  end;
end;

procedure TSEFrequencyShifterStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp      : PDAVSingleFixedArray;
  Up, Down : PDAVSingleFixedArray;
  Sample   : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Up   := PDAVSingleFixedArray(@FUpshiftBuffer[BufferOffset]);
 Down := PDAVSingleFixedArray(@FDownshiftBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do FFrequencyShifter.ProcessSample(Inp^[Sample], Up^[Sample], Down^[Sample]);
end;

// describe the pins (plugs)
function TSEFrequencyShifterStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEFrequencyShifterPins(index) of
  pinFrequency:
   with Properties^ do
    begin
     Name            := 'Frequency [Hz]';
     VariableAddress := @FFrequency;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '100';
     Result          := True;
    end;
  pinCoefficientCount:
   with Properties^ do
    begin
     Name            := 'Coefficient Count';
     VariableAddress := @FCoefficientCount;
     Direction       := drParameter;
     Datatype        := dtInteger;
     DefaultValue    := '12';
     Result          := True;
    end;
  pinTransitionBandwidth:
   with Properties^ do
    begin
     Name            := 'Transition Bandwidth';
     VariableAddress := @FTransitionBW;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0.1';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEFrequencyShifterStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEFrequencyShifterPins(CurrentPin.PinID) of
            pinFrequency : FFrequencyShifter.Frequency := FFrequency;
     pinCoefficientCount : FFrequencyShifter.CoefficientCount := FCoefficientCount;
  pinTransitionBandwidth : FFrequencyShifter.TransitionBandwidth := FTransitionBW;
 end;
end;


{ TSEFrequencyShifterControllableModule }

class procedure TSEFrequencyShifterControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Bode Frequency Shifter';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Bode Frequency Shifter';
  end;
end;

function TSEFrequencyShifterControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEFrequencyShifterPins(index) in [pinFrequency]
  then with Properties^ do Direction := drIn;
end;

end.
