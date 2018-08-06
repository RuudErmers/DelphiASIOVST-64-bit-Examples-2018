unit SELinkwitzRileyModule;

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
  DAV_Types, DAV_DspFilterLinkwitzRiley, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSELinkwitzRileyPins = (pinInput, pinOutputLow, pinOutputHigh, pinFrequency,
    pinOrder);

  TSELinkwitzRileyModule = class(TSEModuleBase)
  private
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FInputBuffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutLoBuffer : PDAVSingleFixedArray;
    FOutHiBuffer : PDAVSingleFixedArray;
    FOrder       : Integer;
    FHighSign    : Single;
    FFilter      : TLinkwitzRiley;
    FStaticCount : Integer;
    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELinkwitzRileyStaticModule = class(TSELinkwitzRileyModule)
  protected
    FFrequency   : Single;
    procedure Open; override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSELinkwitzRileyControlableModule = class(TSELinkwitzRileyStaticModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELinkwitzRileyAutomatableModule = class(TSELinkwitzRileyModule)
  protected
    FFreqBuffer : PDAVSingleFixedArray;
    procedure Open; override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

implementation

uses
  SysUtils, DAV_Common;

constructor TSELinkwitzRileyModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);

 FFilter := TLinkwitzRiley.Create;
 FFilter.Frequency := 1000;
 FFilter.Order := 2;
 FOrder := 2;
end;

destructor TSELinkwitzRileyModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilter);
 inherited;
end;

procedure TSELinkwitzRileyModule.Open;
begin
 inherited Open;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutputLow)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputHigh)].TransmitStatusChange(SampleClock, stRun);
end;

// set samplerate of the filters
procedure TSELinkwitzRileyModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

procedure TSELinkwitzRileyModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSELinkwitzRileyModule.ChooseProcess;
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
class procedure TSELinkwitzRileyModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSELinkwitzRileyModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSELinkwitzRileyPins(index) of
  // typical input plug (inputs are listed first)
  pinInput: with Properties^ do
             begin
              Name            := 'Input';
              VariableAddress := @FInputBuffer;
              Direction       := drIn;
              Datatype        := dtFSample;
              DefaultValue    := '0';
              Flags           := [iofLinearInput];
             end;

  // typical output plug
  pinOutputLow: with Properties^ do
                 begin
                  Name            := 'Output Low';
                  VariableAddress := @FOutLoBuffer;
                  Direction       := drOut;
                  Datatype        := dtFSample;
                 end;
  pinOutputHigh: with Properties^ do
                  begin
                   Name            := 'Output High';
                   VariableAddress := @FOutHiBuffer;
                   Direction       := drOut;
                   Datatype        := dtFSample;
                  end;
  pinFrequency: with Properties^ do
                 begin
                  Name            := 'Frequency';
                 end;
  pinOrder: with Properties^ do
             begin
              Name            := 'Order';
              VariableAddress := @FOrder;
              Direction       := drParameter;
              DataType        := dtEnum;
              DefaultValue    := '4';
              DatatypeExtra   := 'range 0,64';
             end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSELinkwitzRileyModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered LinkwitzRiley time parameter?
 case TSELinkwitzRileyPins(CurrentPin.PinID) of
  pinInput : begin
              ChooseProcess;
              Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
             end;
  pinOrder : FFilter.Order := FOrder;
 end;
 inherited;
end;

{ TSELinkwitzRileyStaticModule }

constructor TSELinkwitzRileyStaticModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFrequency := 1000;
end;

class procedure TSELinkwitzRileyStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Linkwitz-Riley Static Splitter';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Linkwitz-Riley Static Splitter';
  end;
end;

function TSELinkwitzRileyStaticModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSELinkwitzRileyPins(index) of
  pinFrequency: with Properties^ do
                 begin
                  Name            := 'Frequency';
                  VariableAddress := @FFrequency;
                  Direction       := drParameter;
                  DataType        := dtSingle;
                  DefaultValue    := '1000';
                  Result          := True;
                 end;
  pinOrder: with Properties^ do
             begin
              Name            := 'Order';
              VariableAddress := @FOrder;
              Direction       := drParameter;
              DataType        := dtEnum;
              DefaultValue    := '4';
              DatatypeExtra   := 'range 0,64';
              Result          := True;
             end;
 end;
end;

procedure TSELinkwitzRileyStaticModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TSELinkwitzRileyStaticModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered LinkwitzRiley time parameter?
 case TSELinkwitzRileyPins(CurrentPin.PinID) of
  pinFrequency: FFilter.Frequency := FFrequency;
 end;
 inherited;
end;

procedure TSELinkwitzRileyStaticModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  OutLo  : PDAVSingleFixedArray;
  OutHi  : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 OutLo := PDAVSingleFixedArray(@FOutLoBuffer[BufferOffset]);
 OutHi := PDAVSingleFixedArray(@FOutHiBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do FFilter.ProcessSample32(Input[Sample] + cDenorm32, OutLo^[Sample], OutHi^[Sample]);
end;


{ TSELinkwitzRileyControlableModule }

class procedure TSELinkwitzRileyControlableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Linkwitz-Riley Splitter';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Linkwitz-Riley Splitter';
  end;
end;

function TSELinkwitzRileyControlableModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSELinkwitzRileyPins(index) of
  pinFrequency : with Properties^ do Direction := drIn;
      pinOrder : with Properties^ do Direction := drIn;
 end;
end;


{ TSELinkwitzRileyAutomatableModule }

class procedure TSELinkwitzRileyAutomatableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Linkwitz-Riley Automatable Splitter';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Linkwitz-Riley Automatable Splitter';
  end;
end;

function TSELinkwitzRileyAutomatableModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSELinkwitzRileyPins(index) of
  pinFrequency: with Properties^ do
                 begin
                  Name            := 'Frequency [kHz]';
                  VariableAddress := @FFreqBuffer;
                  Direction       := drIn;
                  DataType        := dtFSample;
                  Result          := True;
                 end;
  pinOrder: with Properties^ do
             begin
              Name            := 'Order';
              VariableAddress := @FOrder;
              Direction       := drIn;
              DataType        := dtEnum;
              DefaultValue    := '4';
              DatatypeExtra   := 'range 0,64';
              Result          := True;
             end;
 end;;
end;

procedure TSELinkwitzRileyAutomatableModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TSELinkwitzRileyAutomatableModule.SubProcess(const BufferOffset,
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
 OutLo := PDAVSingleFixedArray(@FOutLoBuffer[BufferOffset]);
 OutHi := PDAVSingleFixedArray(@FOutHiBuffer[BufferOffset]);
 Freq  := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FFilter.Frequency := 10000 * Freq^[Sample];
   FFilter.ProcessSample32(Input[Sample] + cDenorm32, OutLo^[Sample], OutHi^[Sample]);
  end;
end;

end.
