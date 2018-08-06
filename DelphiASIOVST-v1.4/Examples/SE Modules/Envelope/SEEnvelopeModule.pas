unit SEEnvelopeModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspPolyphaseHilbert;

type
  // define some constants to make referencing in/outs clearer
  TSEEnvelopePins = (pinInput, pinOutput, pinCoefficients, pinTransition);

  TSEEnvelopeModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    FCoefficients : Integer;
    FTransition   : Single;
  protected
    FHilbert      : TPhaseHalfPi32;
    procedure Open; override;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSEHilbertModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : array [0..1] of PDAVSingleFixedArray;
    FStaticCount  : Integer;
    FCoefficients : Integer;
    FTransition   : Single;
  protected
    FHilbert      : TPhaseHalfPi32;
    procedure Open; override;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils, DAV_Common;

{ TSEEnvelopeModule }

constructor TSEEnvelopeModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FHilbert := TPhaseHalfPi32.Create;
end;

destructor TSEEnvelopeModule.Destroy;
begin
 FreeAndNil(FHilbert);
 inherited;
end;

procedure TSEEnvelopeModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEEnvelopeModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   // do the actual processing (multiplying the two input samples together)
   Outp^[Sample] := FHilbert.ProcessEnvelopeSample(Inp^[Sample]);
  end;
end;

procedure TSEEnvelopeModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEEnvelopeModule.ChooseProcess;
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
class procedure TSEEnvelopeModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Polyphase Envelope';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Polyphase Envelope';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEEnvelopeModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEEnvelopePins(index) of
  pinInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
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
  pinCoefficients:
   with Properties^ do
    begin
     Name            := 'Coefficients';
     VariableAddress := @FCoefficients;
     Direction       := drIn;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range -0,64';
     DefaultValue    := '8';
    end;
  pinTransition:
   with Properties^ do
    begin
     Name            := 'Transition';
     VariableAddress := @FTransition;
     Direction       := drIn;
     Datatype        := dtSingle;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEEnvelopeModule.PlugStateChange(const CurrentPin: TSEPin);
var
  OutState : TSEStateType;
begin
 inherited;

 case TSEEnvelopePins(CurrentPin.PinID) of
         pinInput : begin
                     OutState := Pin[0].Status;
                     if (OutState < stRun) and (Pin[Integer(pinInput)].Value = 0)
                      then OutState := stStatic;
                     ChooseProcess; 
                     Pin[1].TransmitStatusChange(SampleClock, OutState);
                    end;
  pinCoefficients : FHilbert.NumberOfCoefficients := FCoefficients;
    pinTransition : FHilbert.Transition := Limit(FTransition, 0.01, 0.499);
 end;
end;


{ TSEHilbertModule }

constructor TSEHilbertModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FHilbert := TPhaseHalfPi32.Create;
end;

destructor TSEHilbertModule.Destroy;
begin
 FreeAndNil(FHilbert);
 inherited;
end;

procedure TSEHilbertModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEHilbertModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp, OutA, OutB : PDAVSingleFixedArray;
  Sample          : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 OutA := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 OutB := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do FHilbert.ProcessHilbertSample(Inp^[Sample], OutA^[Sample], OutB^[Sample]);
end;

procedure TSEHilbertModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEHilbertModule.ChooseProcess;
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
class procedure TSEHilbertModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Hilbert Split';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit Polyphase Hilbert Split';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEHilbertModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  0:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  1:
   with Properties^ do
    begin
     Name            := 'Output (sin)';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  2:
   with Properties^ do
    begin
     Name            := 'Output (cos)';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  3:
   with Properties^ do
    begin
     Name            := 'Coefficients';
     VariableAddress := @FCoefficients;
     Direction       := drIn;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range -0,64';
     DefaultValue    := '8';
    end;
  4:
   with Properties^ do
    begin
     Name            := 'Transition';
     VariableAddress := @FTransition;
     Direction       := drIn;
     Datatype        := dtSingle;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEHilbertModule.PlugStateChange(const CurrentPin: TSEPin);
var
  OutState : TSEStateType;
begin
 inherited;

 case TSEEnvelopePins(CurrentPin.PinID) of
         pinInput : begin
                     OutState := Pin[Integer(pinInput)].Status;
                     if (OutState < stRun) and (Pin[0].Value = 0)
                      then OutState := stStatic;

                     ChooseProcess; 
                     Pin[1].TransmitStatusChange(SampleClock, OutState);
                     Pin[2].TransmitStatusChange(SampleClock, OutState);
                    end;
  pinCoefficients : FHilbert.NumberOfCoefficients := FCoefficients;
    pinTransition : FHilbert.Transition := Limit(FTransition, 0.01, 0.499);
 end;
end;

end.
