unit SELookaheadLimiterModule;

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
  DAV_Types, DAV_DspDynamicLookaheadLimiter, DAV_SECommon, DAV_SEModule;

type
  TCustomLookaheadLimiterSEModuleClass = class of TCustomLookaheadLimiterSEModule;

  TCustomLookaheadLimiterSEModule = class(TSEModuleBase)
  protected
    FInputBuffer      : PDAVSingleFixedArray;
    FOutputBuffer     : PDAVSingleFixedArray;
    FGainBuffer       : PDAVSingleFixedArray;
    FStaticCount      : Integer;
    FLookaheadSamples : Integer;
    FLookaheadLimiter : TDspFeedforwardLookaheadLimiter32;
    procedure Open; override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TLookaheadLimiterStaticSEModule = class(TCustomLookaheadLimiterSEModule)
  protected
    FInputGain  : Single;
    FOutputGain : Single;
    FRelease    : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TLookaheadLimiterParamStaticSEModule = class(TLookaheadLimiterStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TLookaheadLimiterAutomatableSEModule = class(TCustomLookaheadLimiterSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FInputGain  : PDAVSingleFixedArray;
    FOutputGain : PDAVSingleFixedArray;
    FRelease    : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


implementation

uses
  SysUtils;

constructor TCustomLookaheadLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FLookaheadLimiter := TDspFeedforwardLookaheadLimiter32.Create;
end;

destructor TCustomLookaheadLimiterSEModule.Destroy;
begin
 FreeAndNil(FLookaheadLimiter);
 inherited;
end;

procedure TCustomLookaheadLimiterSEModule.Open;
begin
 inherited Open;
 ChooseProcess;
end;

procedure TCustomLookaheadLimiterSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 // has user altered LookaheadLimiter parameter?
 case CurrentPin.PinID of
  0: begin
      ChooseProcess;
      Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
     end;
  3: FLookaheadLimiter.LookAhead := 2 * Round(0.5 * FLookaheadSamples);
 end;
end;

// describe your module
class procedure TCustomLookaheadLimiterSEModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TCustomLookaheadLimiterSEModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomLookaheadLimiterSEModule.ChooseProcess;
begin
 if Pin[0].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe the pins (plugs)
function TCustomLookaheadLimiterSEModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  0: with Properties^ do
      begin
       Name            := 'Input';
       VariableAddress := @FInputBuffer;
       Direction       := drIn;
       Datatype        := dtFSample;
       DefaultValue    := '0';
       Flags           := [iofLinearInput];
      end;
  1: with Properties^ do
      begin
       Name            := 'Output';
       VariableAddress := @FOutputBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  2: with Properties^ do
      begin
       Name            := 'Gain (direct!)';
       VariableAddress := @FGainBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  3: with Properties^ do
      begin
       Name            := 'Lookahead [samples]';
       VariableAddress := @FLookaheadSamples;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '64';
       DatatypeExtra   := 'range 2,2048'; 
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;


{ TLookaheadLimiterStaticSEModule }

constructor TLookaheadLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FInputGain        :=   1.00;
 FOutputGain       :=  -0.01;
 FRelease          := 100.00;
 FLookaheadSamples := 64;
end;

class procedure TLookaheadLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lookahead Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lookahead Limiter (Static)';
  end;
end;

function TLookaheadLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  4: with Properties^ do
      begin
       Name            := 'Input Gain [dB]';
       VariableAddress := @FInputGain;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Output Gain [dB]';
       VariableAddress := @FOutputGain;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-0.01';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TLookaheadLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered LookaheadLimiter parameter?
 case CurrentPin.PinID of
  4: FLookaheadLimiter.Input_dB  := FInputGain;
  5: FLookaheadLimiter.Output_dB := FOutputGain;
  6: FLookaheadLimiter.Release   := FRelease;
 end;
 inherited;
end;

procedure TLookaheadLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Gain   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Gain   := PDAVSingleFixedArray(@FGainBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   Output^[Sample] := FLookaheadLimiter.ProcessSample32(Input^[Sample]);
   Gain^[Sample] := FLookaheadLimiter.GainReductionFactor;
  end;
end;

{ TLookaheadLimiterParamStaticSEModule }

class procedure TLookaheadLimiterParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lookahead Limiter';
   ID := 'DAV Lookahead Limiter';
  end;
end;

function TLookaheadLimiterParamStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3..6: with Properties^ do Direction := drIn;
 end;
end;

{ TLookaheadLimiterAutomatableSEModule }

class procedure TLookaheadLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lookahead Limiter (Automatable)';
   ID := 'DAV Lookahead Limiter (Automatable)';
  end;
end;

function TLookaheadLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);

 case Index of
  3: with Properties^ do Direction := drIn;
  4: with Properties^ do
      begin
       Name            := 'Input Gain [dB]';
       VariableAddress := @FInputGain;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Output Gain [dB]';
       VariableAddress := @FOutputGain;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-0.01';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TLookaheadLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  4..6: if (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;

 inherited;
end;

procedure TLookaheadLimiterAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input     : PDAVSingleFixedArray;
  Output    : PDAVSingleFixedArray;
  InpGain   : PDAVSingleFixedArray;
  OutpGain  : PDAVSingleFixedArray;
  Gain      : PDAVSingleFixedArray;
  Relse     : PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input    := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output   := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 InpGain  := PDAVSingleFixedArray(@FInputGain[BufferOffset]);
 OutpGain := PDAVSingleFixedArray(@FOutputGain[BufferOffset]);
 Gain     := PDAVSingleFixedArray(@FGainBuffer[BufferOffset]);
 Relse    := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  with FLookaheadLimiter do
   begin
    Input_dB := 10 * InpGain[Sample];
    Output_dB := 10 * OutpGain[Sample];
    Release := 10 * Relse[Sample];
    Output^[Sample] := ProcessSample32(Input^[Sample]);
    Gain^[Sample] := GainReductionFactor;
   end;
end;

end.
