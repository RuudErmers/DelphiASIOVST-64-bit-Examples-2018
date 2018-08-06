unit SEModDelayModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspModDelay;

type
  // define some constants to make referencing in/outs clearer
  TSEModDelayPins = (pinInput, pinOutput, pinMix, pinDelay, pinDepth,
    pinFeedback, pinLowpassFrequency, pinRate);

  TCustomSEModDelayModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FModDelay     : TModDelay32;
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

  TSEModDelayStaticModule = class(TCustomSEModDelayModule)
  private
    FMix      : Single;
    FDelay    : Single;
    FDepth    : Single;
    FRate     : Single;
    FLowpass  : Single;
    FFeedback : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEModDelayControllableModule = class(TSEModDelayStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEModDelayModule }

constructor TCustomSEModDelayModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FModDelay := TModDelay32.Create
end;

destructor TCustomSEModDelayModule.Destroy;
begin
 FreeAndNil(FModDelay);
 inherited;
end;

procedure TCustomSEModDelayModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEModDelayModule.SampleRateChanged;
begin
 inherited;
 FModDelay.SampleRate := SampleRate;
end;

procedure TCustomSEModDelayModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEModDelayModule.ChooseProcess;
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
class procedure TCustomSEModDelayModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEModDelayModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEModDelayPins(index) of
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

// An input plug has changed value
procedure TCustomSEModDelayModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEModDelayPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
 end;
end;


{ TSEModDelayStaticModule }

// describe your module
class procedure TSEModDelayStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'ModDelay (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV ModDelay (static)';
  end;
end;

procedure TSEModDelayStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FModDelay.ProcessSample32(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSEModDelayStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEModDelayPins(index) of
  pinMix:
   with Properties^ do
    begin
     Name            := 'Mix [%]';
     VariableAddress := @FMix;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '50';
     Result          := True;
    end;
  pinDelay:
   with Properties^ do
    begin
     Name            := 'Delay [ms]';
     VariableAddress := @FDelay;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '20';
     Result          := True;
    end;
  pinDepth:
   with Properties^ do
    begin
     Name            := 'Depth [%]';
     VariableAddress := @FDepth;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '10';
     Result          := True;
    end;
  pinFeedback:
   with Properties^ do
    begin
     Name            := 'Feedback [%]';
     VariableAddress := @FFeedback;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '10';
     Result          := True;
    end;
  pinLowpassFrequency:
   with Properties^ do
    begin
     Name            := 'Lowpass Frequency [Hz]';
     VariableAddress := @FLowpass;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '300';
     Result          := True;
    end;
  pinRate:
   with Properties^ do
    begin
     Name            := 'Rate [Hz]';
     VariableAddress := @FRate;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEModDelayStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEModDelayPins(CurrentPin.PinID) of
               pinMix : FModDelay.Mix              := FMix;
             pinDepth : FModDelay.Depth            := FDepth;
             pinDelay : FModDelay.Delay            := FDelay;
          pinFeedback : FModDelay.Feedback         := FFeedback;
  pinLowpassFrequency : FModDelay.LowpassFrequency := FLowpass;
              pinRate : FModDelay.Rate             := FRate;
 end;
end;


{ TSEModDelayControllableModule }

class procedure TSEModDelayControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'ModDelay';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV ModDelay';
  end;
end;

function TSEModDelayControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEModDelayPins(index) in [pinDepth..pinRate]
  then with Properties^ do Direction := drIn;
end;

end.
