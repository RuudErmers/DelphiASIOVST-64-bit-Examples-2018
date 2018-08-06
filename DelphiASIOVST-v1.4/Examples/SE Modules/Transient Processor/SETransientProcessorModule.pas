unit SETransientProcessorModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspTransientProcessor;

type
  // define some constants to make referencing in/outs clearer
  TSETransientProcessorPins = (pinInput, pinOutput, pinAttack,
    pinRelease, pinGain, pinFilter, pinAttackHold, pinReleaseHold);

  TCustomSETransientProcessorModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FTransientProcessor : TMonoTransientProcessor;
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

  TSETransientProcessorStaticModule = class(TCustomSETransientProcessorModule)
  private
    FAttack      : Single;
    FAttackHold  : Single;
    FFilter      : Single;
    FOutputGain  : Single;
    FRelease     : Single;
    FReleaseHold : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSETransientProcessorControllableModule = class(TSETransientProcessorStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSETransientProcessorModule }

constructor TCustomSETransientProcessorModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FTransientProcessor := TMonoTransientProcessor.Create
end;

destructor TCustomSETransientProcessorModule.Destroy;
begin
 FreeAndNil(FTransientProcessor);
 inherited;
end;

procedure TCustomSETransientProcessorModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSETransientProcessorModule.SampleRateChanged;
begin
 inherited;
 FTransientProcessor.SampleRate := SampleRate;
end;

procedure TCustomSETransientProcessorModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSETransientProcessorModule.ChooseProcess;
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
class procedure TCustomSETransientProcessorModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSETransientProcessorModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSETransientProcessorPins(index) of
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
procedure TCustomSETransientProcessorModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSETransientProcessorPins(CurrentPin.PinID) of
  pinInput: begin
             ChooseProcess;
             Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
            end;
 end;
end;


{ TSETransientProcessorStaticModule }

// describe your module
class procedure TSETransientProcessorStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Transient Processor (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Transient Processor (static)';
  end;
end;

procedure TSETransientProcessorStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FTransientProcessor.ProcessSample32(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSETransientProcessorStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSETransientProcessorPins(index) of
  pinAttack:
   with Properties^ do
    begin
     Name            := 'Attack [%]';
     VariableAddress := @FAttack;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0';
     Result          := True;
    end;
  pinRelease:
   with Properties^ do
    begin
     Name            := 'Release [%]';
     VariableAddress := @FRelease;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0';
     Result          := True;
    end;
  pinGain:
   with Properties^ do
    begin
     Name            := 'Output Gain [dB]';
     VariableAddress := @FOutputGain;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0';
     Result          := True;
    end;
  pinFilter:
   with Properties^ do
    begin
     Name            := 'Filter [%]';
     VariableAddress := @FFilter;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0';
     Result          := True;
    end;
  pinAttackHold:
   with Properties^ do
    begin
     Name            := 'Attack Hold [%]';
     VariableAddress := @FReleaseHold;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '35';
     Result          := True;
    end;
  pinReleaseHold:
   with Properties^ do
    begin
     Name            := 'Release Hold [%]';
     VariableAddress := @FReleaseHold;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '35';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSETransientProcessorStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSETransientProcessorPins(CurrentPin.PinID) of
       pinAttack : FTransientProcessor.Attack := FAttack;
      pinRelease : FTransientProcessor.Release := FRelease;
         pinGain : FTransientProcessor.Output := FOutputGain;
       pinFilter : FTransientProcessor.Filter := FFilter;
   pinAttackHold : FTransientProcessor.AttackHold := FAttackHold;
  pinReleaseHold : FTransientProcessor.ReleaseHold := FReleaseHold;
 end;
end;


{ TSETransientProcessorControllableModule }

class procedure TSETransientProcessorControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Transient Processor';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Transient Processor';
  end;
end;

function TSETransientProcessorControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSETransientProcessorPins(index) in [pinAttack..pinReleaseHold]
  then with Properties^ do Direction := drIn;
end;

end.
