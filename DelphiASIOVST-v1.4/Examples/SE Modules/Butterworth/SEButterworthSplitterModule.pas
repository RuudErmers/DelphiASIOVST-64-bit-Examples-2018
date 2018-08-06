unit SEButterworthSplitterModule;

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
  DAV_Types, DAV_DspFilterButterworth, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEButterworthPins = (pinInput, pinOutputLow, pinOutputHigh, pinFrequency, pinOrder);

  TSEButterworthModule = class(TSEModuleBase)
  protected
    FInputBuffer    : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputLoBuffer : PDAVSingleFixedArray;
    FOutputHiBuffer : PDAVSingleFixedArray;
    FFilter         : TCustomButterworthSplitBandFilter;
    FStaticCount    : Integer;
    FFrequency      : Single;
    FOrder          : Integer;
    procedure ChooseProcess;
    procedure Open; override;
    procedure SampleRateChanged; override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    class function GetFilterClass: TCustomButterworthFilterClass; virtual; abstract;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TSEStaticButterworthSplitterModule = class(TSEButterworthModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEControlableButterworthSplitterModule = class(TSEStaticButterworthSplitterModule)
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEAutomatableButterworthSplitterModule = class(TSEStaticButterworthSplitterModule)
  protected
    FFreqBuffer : PDAVSingleFixedArray;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;


implementation

uses
  SysUtils, DAV_DspFilter;

constructor TSEButterworthModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
 inherited;
 FFrequency := 1000;
 FOrder := 4;

 FFilter := TButterworthSplitBandFilter.Create(4);
 FFilter.SetFilterValues(FFrequency, 0);
 FFilter.Order := FOrder;
end;

destructor TSEButterworthModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilter);
 inherited;
end;

procedure TSEButterworthModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutputLow)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputHigh)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEButterworthModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

// The most important part, processing the audio
procedure TSEButterworthModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  OutLo  : PDAVSingleFixedArray;
  OutHi  : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 OutLo := PDAVSingleFixedArray(@FOutputLoBuffer[BufferOffset]);
 OutHi := PDAVSingleFixedArray(@FOutputHiBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do FFilter.ProcessSample32(Input[Sample], OutLo[Sample], OutHi[Sample]);
end;

procedure TSEButterworthModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEButterworthModule.ChooseProcess;
begin
 if Pin[0].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;


// describe your module
class procedure TSEButterworthModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEButterworthModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEButterworthPins(index) of
  // typical input plug (inputs are listed first)
  pinInput:
    with Properties^ do
     begin
      Name            := 'Input';
      VariableAddress := @FInputBuffer;
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '0';
     end;

  // typical output plug
  pinOutputLow:
    with Properties^ do
     begin
      Name            := 'Output (Low)';
      VariableAddress := @FOutputLoBuffer;
      Direction       := drOut;
      Datatype        := dtFSample;
     end;
  // typical output plug
  pinOutputHigh:
    with Properties^ do
     begin
      Name            := 'Output (High)';
      VariableAddress := @FOutputHiBuffer;
      Direction       := drOut;
      Datatype        := dtFSample;
     end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

{ TSEButterworthModule }

class procedure TSEStaticButterworthSplitterModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Splitter (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Splitter (static)';
  end;
end;

function TSEStaticButterworthSplitterModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEButterworthPins(index) of
  pinFrequency:
    with Properties^ do
     begin
      Name            := 'Frequency';
      VariableAddress := @FFrequency;
      Direction       := drParameter;
      DataType        := dtSingle;
      Flags           := [iofLinearInput];
      DefaultValue    := '1000';
      Result          := True;
     end;
  pinOrder:
    with Properties^ do
     begin
      Name            := 'Order';
      VariableAddress := @FOrder;
      Direction       := drParameter;
      DataType        := dtEnum;
      Flags           := [iofLinearInput];
      DefaultValue    := '4';
      DatatypeExtra   := 'range -0,64';
      Result          := True;
     end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEStaticButterworthSplitterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEButterworthPins(CurrentPin.PinID) of
      pinInput : begin
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                  Pin[2].TransmitStatusChange(SampleClock, Pin[0].Status);
                  ChooseProcess;
                 end;
  pinFrequency : FFilter.Frequency := 1E-5 + abs(FFrequency);
      pinOrder : FFilter.Order     := FOrder;
 end;
 inherited;
end;

{ TSEControlableButterworthSplitterModule }

function TSEControlableButterworthSplitterModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if Index in [2..3] then Properties^.Direction := drIn;
end;

class procedure TSEControlableButterworthSplitterModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Splitter';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Splitter';
  end;
end;

{ TSEAutomatableButterworthSplitterModule }

class procedure TSEAutomatableButterworthSplitterModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Splitter (automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Splitter (automatable)';
  end;
end;

function TSEAutomatableButterworthSplitterModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEButterworthPins(Index) of
  pinFrequency:
    with Properties^ do
     begin
      VariableAddress := @FFreqBuffer;
      Direction       := drIn;
      Datatype        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '5';
      Result          := True;
     end;
  pinOrder:
    with Properties^ do
     begin
      Direction       := drIn;
      DataType        := dtEnum;
      Flags           := [iofLinearInput];
      Result          := True;
     end;
 end;
end;

procedure TSEAutomatableButterworthSplitterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  OutLo  : PDAVSingleFixedArray;
  OutHi  : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 OutLo := PDAVSingleFixedArray(@FOutputLoBuffer[BufferOffset]);
 OutHi := PDAVSingleFixedArray(@FOutputHiBuffer[BufferOffset]);
 Freq  := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
   FFilter.ProcessSample32(Input[Sample], OutLo[Sample], OutHi[Sample]);
  end;
end;

end.
