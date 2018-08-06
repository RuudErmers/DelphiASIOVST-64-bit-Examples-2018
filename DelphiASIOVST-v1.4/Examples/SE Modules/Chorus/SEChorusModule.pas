unit SEChorusModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspChorus, DAV_StkChorus;

type
  // define some constants to make referencing in/outs clearer
  TSEChorusPins = (pinInput, pinOutput, pinStages, pinDepth, pinSpeed, pinDrift,
    pinMix);

  // define some constants to make referencing in/outs clearer
  TSEStkChorusPins = (pinStkInput, pinStkOutputMono, pinStkOutputL,
    pinStkOutputR, pinStkModDepth, pinStkModFreq, pinStkMix);

  TSEChorusModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStages       : Integer;
    FStaticCount  : Integer;
    FDepth        : Single;
    FSpeed        : Single;
    FDrift        : Single;
    FMix          : Single;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FChorus       : TDspChorus32;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSEStkChorusModule = class(TSEModuleBase)
  private
    FInputBuffer   : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputMBuffer : PDAVSingleFixedArray;
    FOutputLBuffer : PDAVSingleFixedArray;
    FOutputRBuffer : PDAVSingleFixedArray;
    FStaticCount   : Integer;
    FModDepth      : Single;
    FModFreq       : Single;
    FEffectMix     : Single;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FStkChorus     : TStkChorus;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils;

{ TSEChorusModule }

constructor TSEChorusModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FChorus := TDspChorus32.Create
end;

destructor TSEChorusModule.Destroy;
begin
 FreeAndNil(FChorus);
 inherited;
end;

procedure TSEChorusModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEChorusModule.SampleRateChanged;
begin
 inherited;
 FChorus.SampleRate := SampleRate;
end;

procedure TSEChorusModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FChorus.ProcessSample32(Inp^[Sample]);
end;

procedure TSEChorusModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEChorusModule.ChooseProcess;
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
class procedure TSEChorusModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Chorus';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Chorus';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEChorusModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEChorusPins(index) of
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
  pinStages:
   with Properties^ do
    begin
     Name            := 'Stages';
     VariableAddress := @FStages;
     Direction       := drIn;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range 1,16';
     DefaultValue    := '2';
    end;
  pinDepth:
   with Properties^ do
    begin
     Name            := 'Depth';
     VariableAddress := @FDepth;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
    end;
  pinSpeed:
   with Properties^ do
    begin
     Name            := 'Speed';
     VariableAddress := @FSpeed;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
    end;
  pinDrift:
   with Properties^ do
    begin
     Name            := 'Drift';
     VariableAddress := @FDrift;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
    end;
  pinMix:
   with Properties^ do
    begin
     Name            := 'Mix';
     VariableAddress := @FMix;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.5';
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEChorusModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case TSEChorusPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
      pinStages: FChorus.Stages := FStages;
       pinDepth: FChorus.Depth := FDepth;
       pinSpeed: FChorus.Speed := FSpeed;
       pinDrift: FChorus.Drift := FDrift;
         pinMix: FChorus.Mix := FMix;
 end;
end;

{ TSEStkChorusModule }

constructor TSEStkChorusModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FStkChorus := TStkChorus.Create
end;

destructor TSEStkChorusModule.Destroy;
begin
 FreeAndNil(FStkChorus);
 inherited;
end;

procedure TSEStkChorusModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEStkChorusModule.SampleRateChanged;
begin
 inherited;
 FStkChorus.SampleRate := SampleRate;
end;

procedure TSEStkChorusModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : array [0..2] of PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp     := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp[0] := PDAVSingleFixedArray(@FOutputMBuffer[BufferOffset]);
 Outp[1] := PDAVSingleFixedArray(@FOutputLBuffer[BufferOffset]);
 Outp[2] := PDAVSingleFixedArray(@FOutputRBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outp[0]^[Sample] := FStkChorus.Tick(Inp^[Sample]);
   Outp[1]^[Sample] := FStkChorus.LastOutputLeft;
   Outp[2]^[Sample] := FStkChorus.LastOutputRight;
  end;
end;

procedure TSEStkChorusModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEStkChorusModule.ChooseProcess;
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
class procedure TSEStkChorusModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Chorus (STK based)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Chorus (STK based)';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEStkChorusModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEStkChorusPins(index) of
  pinStkInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinStkOutputMono:
   with Properties^ do
    begin
     Name            := 'Output (mono)';
     VariableAddress := @FOutputMBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinStkOutputL:
   with Properties^ do
    begin
     Name            := 'Output (left)';
     VariableAddress := @FOutputLBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinStkOutputR:
   with Properties^ do
    begin
     Name            := 'Output (right)';
     VariableAddress := @FOutputRBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinStkModDepth:
   with Properties^ do
    begin
     Name            := 'ModDepth';
     VariableAddress := @FModDepth;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
    end;
  pinStkModFreq:
   with Properties^ do
    begin
     Name            := 'ModFreq';
     VariableAddress := @FModFreq;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
    end;
  pinStkMix:
   with Properties^ do
    begin
     Name            := 'Mix';
     VariableAddress := @FEffectMix;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.5';
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEStkChorusModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case TSEStkChorusPins(CurrentPin.PinID) of
     pinStkInput: begin
                   ChooseProcess;
                   Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                   Pin[2].TransmitStatusChange(SampleClock, Pin[0].Status);
                   Pin[3].TransmitStatusChange(SampleClock, Pin[0].Status);
                  end;
  pinStkModDepth: FStkChorus.ModDepth := FModDepth;
   pinStkModFreq: FStkChorus.ModFrequency := FModFreq;
       pinStkMix: FStkChorus.EffectMix := FEffectMix;
 end;
end;

end.
