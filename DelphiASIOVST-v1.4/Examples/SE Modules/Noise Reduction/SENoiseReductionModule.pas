unit SENoiseReductionModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspSpectralNoiseReduction,
  DAV_DspWindowFunctions;

type
  // define some constants to make referencing in/outs clearer
  TSENoiseReductionPins = (pinInput, pinOutput, pinFftOrder, pinWindowType,
    pinThreshold, pinRatio, pinKnee, pinAttack, pinRelease, pinCaptureNoise);

  TCustomSENoiseReductionModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    FFftOrder     : Integer;
    FWinFuncIndex : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FNoiseReduction : TNoiseReduction32;
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

  TSENoiseReductionStaticModule = class(TCustomSENoiseReductionModule)
  private
    FThreshold : Single;
    FRatio     : Single;
    FKnee      : Single;
    FAttack    : Single;
    FRelease   : Single;
    FCapture   : Boolean;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSENoiseReductionControllableModule = class(TSENoiseReductionStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSENoiseReductionModule }

constructor TCustomSENoiseReductionModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FNoiseReduction := TNoiseReduction32.Create
end;

destructor TCustomSENoiseReductionModule.Destroy;
begin
 FreeAndNil(FNoiseReduction);
 inherited;
end;

procedure TCustomSENoiseReductionModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSENoiseReductionModule.SampleRateChanged;
begin
 inherited;
 FNoiseReduction.SampleRate := SampleRate;
end;

procedure TCustomSENoiseReductionModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSENoiseReductionModule.ChooseProcess;
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
class procedure TCustomSENoiseReductionModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSENoiseReductionModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSENoiseReductionPins(Index) of
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
  pinFftOrder:
   with Properties^ do
    begin
     Name            := 'FFT Order';
     VariableAddress := @FFftOrder;
     Direction       := drParameter;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range 6,16';
     DefaultValue    := '10';
    end;
  pinWindowType:
   with Properties^ do
    begin
     Name            := 'Window Function';
     VariableAddress := @FWinFuncIndex;
     Direction       := drParameter;
     Datatype        := dtEnum;
     DatatypeExtra   := 'Rectangle, Triangle, Hanning, Hamming, Blackman, Lanczos, Welch';
     DefaultValue    := 'Blackman';
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSENoiseReductionModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSENoiseReductionPins(CurrentPin.PinID) of
         pinInput: begin
                    ChooseProcess;
                    Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                   end;
      pinFftOrder: FNoiseReduction.FFTOrder := FFftOrder;
    pinWindowType: FNoiseReduction.WindowFunctionClass := GWindowFunctions[FWinFuncIndex];
 end;
end;


{ TSENoiseReductionStaticModule }

// describe your module
class procedure TSENoiseReductionStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Noise Reduction (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Noise Reduction (static)';
  end;
end;

procedure TSENoiseReductionStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FNoiseReduction.ProcessSample32(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSENoiseReductionStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSENoiseReductionPins(Index) of
  pinThreshold:
   with Properties^ do
    begin
     Name            := 'Threshold Offset [dB]';
     VariableAddress := @FThreshold;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '8';
     Result          := True;
    end;
  pinRatio:
   with Properties^ do
    begin
     Name            := 'Ratio';
     VariableAddress := @FRatio;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0.1';
     Result          := True;
    end;
  pinKnee:
   with Properties^ do
    begin
     Name            := 'Knee [dB]';
     VariableAddress := @FKnee;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
  pinAttack:
   with Properties^ do
    begin
     Name            := 'Attack [ms]';
     VariableAddress := @FAttack;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0.6';
     Result          := True;
    end;
  pinRelease:
   with Properties^ do
    begin
     Name            := 'Release [ms]';
     VariableAddress := @FRelease;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '8';
     Result          := True;
    end;
  pinCaptureNoise:
   with Properties^ do
    begin
     Name            := 'Capture Noise Profile';
     VariableAddress := @FCapture;
     Direction       := drParameter;
     Datatype        := dtBoolean;
     DefaultValue    := '0';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSENoiseReductionStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSENoiseReductionPins(CurrentPin.PinID) of
     pinThreshold: FNoiseReduction.ThresholdOffset := FThreshold;
         pinRatio: FNoiseReduction.Ratio := FRatio;
          pinKnee: FNoiseReduction.Knee := FKnee;
        pinAttack: FNoiseReduction.Attack := FAttack;
       pinRelease: FNoiseReduction.Release := FRelease;
  pinCaptureNoise: FNoiseReduction.Match := FCapture;
 end;
end;


{ TSENoiseReductionControllableModule }

class procedure TSENoiseReductionControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Noise Reduction';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Noise Reduction';
  end;
end;

function TSENoiseReductionControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSENoiseReductionPins(Index) in [pinThreshold..pinCaptureNoise]
  then with Properties^ do Direction := drIn;
end;

end.
