unit SEVoiceSynthModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspVoiceSynth;

type
  // define some constants to make referencing in/outs clearer
  TSEVoiceSynthPins = (pinInput, pinMinimum, pinMaximum, pinSmooth,
    pinOneCrossingOnly, pinDSFilterOrder, pinDSBandwidth, pinAttack,
    pinRelease, pinThreshold, pinQuantize, pinOutput);

  TCustomSEVoiceSynthModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FVoiceSynth  : TVoiceSynth;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TSEVoiceSynthStaticModule = class(TCustomSEVoiceSynthModule)
  private
    FMinimum         : Single;
    FMaximum         : Single;
    FSmoothFactor    : Single;
    FOneCrossingOnly : Boolean;
    FDSBandwidth     : Single;
    FDSFilterOrder   : Integer;
    FAttack          : Single;
    FRelease         : Single;
    FThreshold       : Single;
    FQuantize        : Boolean;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEVoiceSynthControllableModule = class(TSEVoiceSynthStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEVoiceSynthModule }

constructor TCustomSEVoiceSynthModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FVoiceSynth := TVoiceSynth.Create;
end;

destructor TCustomSEVoiceSynthModule.Destroy;
begin
 FreeAndNil(FVoiceSynth);
 inherited;
end;

procedure TCustomSEVoiceSynthModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEVoiceSynthModule.SampleRateChanged;
begin
 inherited;
 if SampleRate > 0
  then FVoiceSynth.SampleRate := SampleRate;
end;

procedure TCustomSEVoiceSynthModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Inp, Outp : PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FVoiceSynth.ProcessSample32(Inp^[Sample]);
end;

procedure TCustomSEVoiceSynthModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEVoiceSynthModule.ChooseProcess;
begin
 if (Pin[Integer(pinInput)].Status = stRun)
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSEVoiceSynthModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEVoiceSynthModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEVoiceSynthPins(index) of
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
     DefaultValue    := '0';
     Result          := True;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEVoiceSynthModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEVoiceSynthPins(CurrentPin.PinID) of
  pinInput : begin
              Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, Pin[0].Status);
              ChooseProcess;
             end;
 end;
end;


{ TSEVoiceSynthStaticModule }

// describe your module
class procedure TSEVoiceSynthStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'VoiceSynth (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV VoiceSynth (static)';
  end;
end;

// describe the pins (plugs)
function TSEVoiceSynthStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEVoiceSynthPins(index) of
  pinMinimum:
   with Properties^ do
    begin
     Name            := 'Minimum [Hz]';
     VariableAddress := @FMinimum;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '100';
     Result          := True;
    end;
  pinMaximum:
   with Properties^ do
    begin
     Name            := 'Maximum [Hz]';
     VariableAddress := @FMaximum;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '4000';
     Result          := True;
    end;
  pinSmooth:
   with Properties^ do
    begin
     Name            := 'Smooth [0..1]';
     VariableAddress := @FSmoothFactor;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0.99';
     Result          := True;
    end;
  pinOneCrossingOnly:
   with Properties^ do
    begin
     Name            := 'One Crossing Only';
     VariableAddress := @FOneCrossingOnly;
     Direction       := drParameter;
     Datatype        := dtBoolean;
     DefaultValue    := 'True';
     Result          := True;
    end;
  pinDSFilterOrder:
   with Properties^ do
    begin
     Name            := 'Downsampling Filter Order';
     VariableAddress := @FDSFilterOrder;
     Direction       := drParameter;
     Datatype        := dtEnum;
     DefaultValue    := '4';
     DatatypeExtra   := 'range 2,32';
     Result          := True;
    end;
  pinDSBandwidth:
   with Properties^ do
    begin
     Name            := 'Downsampling Bandwidth [0..1]';
     VariableAddress := @FDSBandwidth;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := 'True';
     Result          := True;
    end;
  pinAttack:
   with Properties^ do
    begin
     Name            := 'Attack [ms]';
     VariableAddress := @FAttack;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
  pinRelease:
   with Properties^ do
    begin
     Name            := 'Release [ms]';
     VariableAddress := @FRelease;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '10';
     Result          := True;
    end;
  pinThreshold:
   with Properties^ do
    begin
     Name            := 'Threshold [-1..1]';
     VariableAddress := @FThreshold;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0';
     Result          := True;
    end;
  pinQuantize:
   with Properties^ do
    begin
     Name            := 'Quantize To Notes';
     VariableAddress := @FQuantize;
     Direction       := drParameter;
     Datatype        := dtBoolean;
     DefaultValue    := 'False';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEVoiceSynthStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEVoiceSynthPins(CurrentPin.PinID) of
  pinMinimum         : FVoiceSynth.MinimumFrequency := FMinimum;
  pinMaximum         : FVoiceSynth.MaximumFrequency := FMaximum;
  pinSmooth          : FVoiceSynth.SmoothFactor := FSmoothFactor;
  pinDSFilterOrder   : FVoiceSynth.DownSampleFilterOrder := FDSFilterOrder;
  pinDSBandwidth     : FVoiceSynth.DownSampleBandwidth := FDSBandwidth;
  pinOneCrossingOnly : FVoiceSynth.OneCrossingOnly := FOneCrossingOnly;
  pinAttack          : FVoiceSynth.Attack := FAttack;
  pinRelease         : FVoiceSynth.Release := FRelease;
  pinThreshold       : FVoiceSynth.Threshold := FThreshold;
  pinQuantize        : FVoiceSynth.QuantizeToNotes := FQuantize;
 end;
end;


{ TSEVoiceSynthControllableModule }

class procedure TSEVoiceSynthControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'VoiceSynth';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV VoiceSynth';
  end;
end;

function TSEVoiceSynthControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEVoiceSynthPins(index) in [pinMinimum..pinQuantize]
  then with Properties^ do Direction := drIn;
end;

end.
