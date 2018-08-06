unit SELightweightDynamicsModule;

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
//  SynthEdit is witten by Jeff McClintock (see http://www.synthedit.com/     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_DspDynamics, DAV_DspLightweightDynamics, DAV_SECommon,
  DAV_SEModule;

type
  TCustomLightweightDynamicsSEModuleClass = class of TCustomLightweightDynamicsSEModule;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomLightweightDynamicsSEModule                                      //
  //  -----------------------                                                 //
  //                                                                          //
  //  Base class for all lightweight dynamics, simple features a sidechain    //
  //  input two regular inputs and two outputs.                               //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomLightweightDynamicsSEModule = class(TSEModuleBase)
  protected
    FSideChainBuffer : PDAVSingleFixedArray;
    FInputBuffer     : array [0..1] of PDAVSingleFixedArray;
    FOutputBuffer    : array [0..1] of PDAVSingleFixedArray;
    FStaticCount     : Integer;
    procedure Open; override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftClassicGateSEModule                                           //
  //  -----------------------------                                           //
  //                                                                          //
  //  Base class for the soft direct gate.                                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomLightweightGateSEModule = class(TCustomLightweightDynamicsSEModule)
  protected
    FDynamicProcesor: TLightweightSoftKneeFeedbackCompressor;
    procedure Open; override;
    procedure SampleRateChanged; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TLightweightGateStaticSEModule = class(TCustomLightweightGateSEModule)
  protected
    FThreshold : Single;
    FRatio     : Single;
    FKnee_dB   : Single;
    FAttack    : Single;
    FRelease   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TLightweightGateParamStaticSEModule = class(TLightweightGateStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TLightweightGateAutomatableSEModule = class(TCustomLightweightGateSEModule)
  protected
    FThreshold : PDAVSingleFixedArray;
    FRatio     : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomLimiterSEModule                                                  //
  //  ----------------------                                                  //
  //                                                                          //
  //  Base class for all time constant limiters.                              //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomLightweightLimiterSEModule = class(TCustomLightweightDynamicsSEModule)
  protected
    FDynamicProcesor : TLightweightSoftKneeLimiter;
    FAutoMakeUp      : Boolean;
    procedure Open; override;
    procedure SampleRateChanged; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TLightweightLimiterStaticSEModule = class(TCustomLightweightLimiterSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TLightweightLimiterParamStaticSEModule = class(TLightweightLimiterStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TLightweightLimiterAutomatableSEModule = class(TCustomLightweightLimiterSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomCompressorSEModule                                               //
  //  -------------------------                                               //
  //                                                                          //
  //  Base class for all time constant compressors.                           //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomLightweightCompressorSEModule = class(TCustomLightweightDynamicsSEModule)
  protected
    FDynamicProcesor : TCustomCompressor;
    FAutoMakeUp      : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure Open; override;
    procedure SampleRateChanged; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomLightweightCompressorSEModule                                    //
  //  ------------------------------------                                    //
  //                                                                          //
  //  Base class for the lightweight soft knee compressor.                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TLightweightCompressorStaticSEModule = class(TCustomLightweightCompressorSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FRatio     : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TLightweightCompressorParamStaticSEModule = class(TLightweightCompressorStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TLightweightCompressorAutomatableSEModule = class(TCustomLightweightCompressorSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FRatio     : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomLightweightFeedbackCompressorSEModule                            //
  //  --------------------------------------------                            //
  //                                                                          //
  //  Base class for the lightweight soft knee feedback compressor.           //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomLightweightFeedbackCompressorSEModule = class(TCustomLightweightCompressorSEModule)
  protected
    FDynamicProcesor : TLightweightSoftKneeFeedbackCompressor;
    FAutoMakeUp      : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure Open; override;
    procedure SampleRateChanged; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TLightweightFeedbackCompressorStaticSEModule = class(TCustomLightweightCompressorSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FRatio     : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TLightweightFeedbackCompressorParamStaticSEModule = class(TLightweightFeedbackCompressorStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TLightweightFeedbackCompressorAutomatableSEModule = class(TCustomLightweightFeedbackCompressorSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FRatio     : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

implementation

uses
  SysUtils;

procedure TCustomLightweightDynamicsSEModule.Open;
begin
 inherited Open;
 ChooseProcess;
end;

procedure TCustomLightweightDynamicsSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 if CurrentPin.PinID in [0..2] then
   ChooseProcess;

 if CurrentPin.PinID in [1..2] then
   Pin[CurrentPin.PinID + 2].TransmitStatusChange(SampleClock,
     Pin[CurrentPin.PinID].Status);
end;

// describe your module
class procedure TCustomLightweightDynamicsSEModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TCustomLightweightDynamicsSEModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomLightweightDynamicsSEModule.ChooseProcess;
begin
 if (Pin[0].Status = stRun) and
    ((Pin[1].Status = stRun) or
     (Pin[2].Status = stRun))
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe the pins (plugs)
function TCustomLightweightDynamicsSEModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  0: with Properties^ do
      begin
       Name            := 'Sidechain';
       VariableAddress := @FSideChainBuffer;
       Direction       := drIn;
       Datatype        := dtFSample;
       DefaultValue    := '0';
       Flags           := [iofLinearInput];
      end;
  1: with Properties^ do
      begin
       Name            := 'Input A';
       VariableAddress := @FInputBuffer[0];
       Direction       := drIn;
       Datatype        := dtFSample;
       DefaultValue    := '0';
       Flags           := [iofLinearInput];
      end;
  2: with Properties^ do
      begin
       Name            := 'Input B';
       VariableAddress := @FInputBuffer[1];
       Direction       := drIn;
       Datatype        := dtFSample;
       DefaultValue    := '0';
       Flags           := [iofLinearInput];
      end;
  3: with Properties^ do
      begin
       Name            := 'Output A';
       VariableAddress := @FOutputBuffer[0];
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  4: with Properties^ do
      begin
       Name            := 'Output B';
       VariableAddress := @FOutputBuffer[1];
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TCustomLightweightGateSEModule }

constructor TCustomLightweightGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TLightweightSoftKneeFeedbackCompressor.Create;
end;

destructor TCustomLightweightGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomLightweightGateSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

procedure TCustomLightweightGateSEModule.SampleRateChanged;
begin
  inherited;
  FDynamicProcesor.SampleRate := SampleRate;
end;

procedure TCustomLightweightGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.InputSample(SideChain[Sample]);
   Output[0]^[Sample] := FDynamicProcesor.GainSample(Input[0, Sample]);
   Output[1]^[Sample] := FDynamicProcesor.GainSample(Input[1, Sample]);
  end;
end;

{ TLightweightGateStaticSEModule }

constructor TLightweightGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FRatio     :=   1;
 FKnee_dB   :=   1;
 FAttack    :=  10;
 FRelease   := 100;
end;

class procedure TLightweightGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Gate (Static)';
  end;
end;

function TLightweightGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Ratio [1:x]';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  8: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  9: with Properties^ do
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
procedure TLightweightGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered LightweightDynamics parameter?
 case CurrentPin.PinID of
  5: FDynamicProcesor.Threshold_dB := FThreshold;
  6: FDynamicProcesor.Ratio        := FRatio;
  7: FDynamicProcesor.Knee_dB      := FKnee_dB;
  8: FDynamicProcesor.Attack       := FRelease;
  9: FDynamicProcesor.Release      := FAttack;
 end;
 inherited;
end;

procedure TLightweightGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.InputSample(SideChain[Sample]);
   Output[0]^[Sample] := FDynamicProcesor.GainSample(Input[0, Sample]);
   Output[1]^[Sample] := FDynamicProcesor.GainSample(Input[1, Sample]);
  end;
end;

{ TLightweightGateParamStaticSEModule }

class procedure TLightweightGateParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lightweight Gate';
   ID := 'DAV Lightweight Gate';
  end;
end;

function TLightweightGateParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5..9: with Properties^ do Direction := drIn;
 end;
end;

{ TLightweightGateAutomatableSEModule }

class procedure TLightweightGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lightweight Gate (Automatable)';
   ID := 'DAV Lightweight Gate (Automatable)';
  end;
end;

function TLightweightGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Ratio [1:x]';
       VariableAddress := @FRatio;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  8: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  9: with Properties^ do
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

procedure TLightweightGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered LightweightDynamics parameter?
 case CurrentPin.PinID of
  5..9: if (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun) and
           (Pin[7].Status <> stRun) and
           (Pin[8].Status <> stRun) and
           (Pin[9].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TLightweightGateAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Thres     : PDAVSingleFixedArray;
  Knee      : PDAVSingleFixedArray;
  Rtio      : PDAVSingleFixedArray;
  Attck     : PDAVSingleFixedArray;
  Relse     : PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);
 Knee      := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Rtio      := PDAVSingleFixedArray(@FRatio[BufferOffset]);
 Thres     := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attck     := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Relse     := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  with FDynamicProcesor do
   begin
    Threshold_dB := 10 * Thres[Sample];
    Knee_dB := 10 * Knee[Sample];
    Ratio := 10 * Rtio[Sample];
    Attack := 10 * Attck[Sample];
    Release := 10 * Relse[Sample];
    InputSample(SideChain[Sample]);
    Output[0]^[Sample] := GainSample(Input[0, Sample]);
    Output[1]^[Sample] := GainSample(Input[1, Sample]);
   end;
end;

{ TCustomLightweightLimiterSEModule }

constructor TCustomLightweightLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TLightweightSoftKneeLimiter.Create;
end;

destructor TCustomLightweightLimiterSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomLightweightLimiterSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

procedure TCustomLightweightLimiterSEModule.SampleRateChanged;
begin
 inherited;
 FDynamicProcesor.SampleRate := SampleRate;
end;

procedure TCustomLightweightLimiterSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.InputSample(SideChain[Sample]);
   Output[0]^[Sample] := FDynamicProcesor.GainSample(Input[0, Sample]);
   Output[1]^[Sample] := FDynamicProcesor.GainSample(Input[1, Sample]);
  end;
end;

{ TLightweightLimiterStaticSEModule }

constructor TLightweightLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold  := -10;
 FKnee_dB    :=   1;
 FAttack     :=  10;
 FRelease    := 100;
 FAutoMakeUp := False;
end;

class procedure TLightweightLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Limiter (Static)';
  end;
end;

function TLightweightLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  8: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  9: with Properties^ do
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
procedure TLightweightLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered LightweightDynamics parameter?
 case CurrentPin.PinID of
  5: FDynamicProcesor.Threshold_dB := FThreshold;
  6: FDynamicProcesor.AutoMakeUp   := FAutoMakeUp;
  7: FDynamicProcesor.Knee_dB      := FKnee_dB;
  8: FDynamicProcesor.Attack       := FAttack;
  9: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TLightweightLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.InputSample(SideChain[Sample]);
   Output[0]^[Sample] := FDynamicProcesor.GainSample(Input[0, Sample]);
   Output[1]^[Sample] := FDynamicProcesor.GainSample(Input[1, Sample]);
  end;
end;

{ TLightweightLimiterParamStaticSEModule }

class procedure TLightweightLimiterParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lightweight Limiter';
   ID := 'DAV Lightweight Limiter';
  end;
end;

function TLightweightLimiterParamStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5..9: with Properties^ do Direction := drIn;
 end;
end;

{ TLightweightLimiterAutomatableSEModule }

class procedure TLightweightLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lightweight Limiter (Automatable)';
   ID := 'DAV Lightweight Limiter (Automatable)';
  end;
end;

function TLightweightLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  8: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  9: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TLightweightLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  5, 7..9: if (Pin[5].Status <> stRun) and
              (Pin[7].Status <> stRun) and
              (Pin[8].Status <> stRun) and
              (Pin[9].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
  6: FDynamicProcesor.AutoMakeUp := FAutoMakeUp;
 end;
 inherited;
end;

procedure TLightweightLimiterAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Thres     : PDAVSingleFixedArray;
  Knee      : PDAVSingleFixedArray;
  Attck     : PDAVSingleFixedArray;
  Relse     : PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);
 Knee      := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Thres     := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attck     := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Relse     := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  with FDynamicProcesor do
   begin
    Threshold_dB := 10 * Thres[Sample];
    Knee_dB := 10 * Knee[Sample];
    Attack := 10 * Attck[Sample];
    Release := 10 * Relse[Sample];
    InputSample(SideChain[Sample]);
    Output[0]^[Sample] := GainSample(Input[0, Sample]);
    Output[1]^[Sample] := GainSample(Input[1, Sample]);
   end;
end;


{ TCustomLightweightCompressorSEModule }

constructor TCustomLightweightCompressorSEModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FDynamicProcesor := TLightweightSoftKneeCompressor.Create; 
end;

destructor TCustomLightweightCompressorSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomLightweightCompressorSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

function TCustomLightweightCompressorSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  6: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TCustomLightweightCompressorSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered LightweightDynamics parameter?
 case CurrentPin.PinID of
  6: FDynamicProcesor.AutoMakeUp := FAutoMakeUp;
 end;
 inherited;
end;

procedure TCustomLightweightCompressorSEModule.SampleRateChanged;
begin
 inherited;
 FDynamicProcesor.SampleRate := SampleRate;
end;

procedure TCustomLightweightCompressorSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.InputSample(SideChain[Sample]);
   Output[0]^[Sample] := FDynamicProcesor.GainSample(Input[0, Sample]);
   Output[1]^[Sample] := FDynamicProcesor.GainSample(Input[1, Sample]);
  end;
end;

{ TLightweightCompressorStaticSEModule }

constructor TLightweightCompressorStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB   :=   1;
 FRatio     :=   1;
 FAttack    :=  10;
 FRatio     :=   8;
 FRelease   := 100;
end;

class procedure TLightweightCompressorStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Compressor (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Compressor (Static)';
  end;
end;

function TLightweightCompressorStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Ratio [1:x]';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  8: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  9: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
 10: with Properties^ do
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
procedure TLightweightCompressorStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered LightweightDynamics parameter?
 case CurrentPin.PinID of
   5: FDynamicProcesor.Threshold_dB := FThreshold;
   7: FDynamicProcesor.Ratio        := FRatio;
   8: FDynamicProcesor.Knee_dB      := FKnee_dB;
   9: FDynamicProcesor.Attack       := FAttack;
  10: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TLightweightCompressorStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.InputSample(SideChain[Sample]);
   Output[0]^[Sample] := FDynamicProcesor.GainSample(Input[0, Sample]);
   Output[1]^[Sample] := FDynamicProcesor.GainSample(Input[1, Sample]);
  end;
end;

{ TLightweightCompressorParamStaticSEModule }

class procedure TLightweightCompressorParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lightweight Compressor';
   ID := 'DAV Lightweight Compressor';
  end;
end;

function TLightweightCompressorParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5..10: with Properties^ do Direction := drIn;
 end;
end;

{ TLightweightCompressorAutomatableSEModule }

class procedure TLightweightCompressorAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lightweight Compressor (Automatable)';
   ID := 'DAV Lightweight Compressor (Automatable)';
  end;
end;

function TLightweightCompressorAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Ratio [1:x]';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  8: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  9: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
 10: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TLightweightCompressorAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  5..10: if (Pin[ 5].Status <> stRun) and
            (Pin[ 6].Status <> stRun) and
            (Pin[ 7].Status <> stRun) and
            (Pin[ 8].Status <> stRun) and
            (Pin[ 9].Status <> stRun) and
            (Pin[10].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TLightweightCompressorAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Thres     : PDAVSingleFixedArray;
  Rtio      : PDAVSingleFixedArray;
  Knee      : PDAVSingleFixedArray;
  Attck     : PDAVSingleFixedArray;
  Relse     : PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);
 Knee      := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Thres     := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Rtio      := PDAVSingleFixedArray(@FRatio[BufferOffset]);
 Attck     := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Relse     := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  with FDynamicProcesor do
   begin
    Threshold_dB := 10 * Thres[Sample];
    Knee_dB := 10 * Knee[Sample];
    Ratio := 10 * Rtio[Sample];
    Attack := 10 * Attck[Sample];
    Release := 10 * Relse[Sample];
    InputSample(SideChain[Sample]);
    Output[0]^[Sample] := GainSample(Input[0, Sample]);
    Output[1]^[Sample] := GainSample(Input[1, Sample]);
   end;
end;

{ TCustomLightweightFeedbackCompressorSEModule }

constructor TCustomLightweightFeedbackCompressorSEModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FDynamicProcesor := TLightweightSoftKneeFeedbackCompressor.Create; 
end;

destructor TCustomLightweightFeedbackCompressorSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomLightweightFeedbackCompressorSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

function TCustomLightweightFeedbackCompressorSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  6: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TCustomLightweightFeedbackCompressorSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered LightweightDynamics parameter?
 case CurrentPin.PinID of
  6: FDynamicProcesor.AutoMakeUp := FAutoMakeUp;
 end;
 inherited;
end;

procedure TCustomLightweightFeedbackCompressorSEModule.SampleRateChanged;
begin
 inherited;
 FDynamicProcesor.SampleRate := SampleRate;
end;

procedure TCustomLightweightFeedbackCompressorSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.InputSample(SideChain[Sample]);
   Output[0]^[Sample] := FDynamicProcesor.GainSample(Input[0, Sample]);
   Output[1]^[Sample] := FDynamicProcesor.GainSample(Input[1, Sample]);
  end;
end;

{ TLightweightFeedbackCompressorStaticSEModule }

constructor TLightweightFeedbackCompressorStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB   :=   1;
 FRatio     :=   1;
 FAttack    :=  10;
 FRatio     :=   8;
 FRelease   := 100;
end;

class procedure TLightweightFeedbackCompressorStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Compressor (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Compressor (Static)';
  end;
end;

function TLightweightFeedbackCompressorStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Ratio [1:x]';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  8: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  9: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
 10: with Properties^ do
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
procedure TLightweightFeedbackCompressorStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered LightweightDynamics parameter?
 case CurrentPin.PinID of
   5: FDynamicProcesor.Threshold_dB := FThreshold;
   7: FDynamicProcesor.Ratio        := FRatio;
   8: FDynamicProcesor.Knee_dB      := FKnee_dB;
   9: FDynamicProcesor.Attack       := FAttack;
  10: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TLightweightFeedbackCompressorStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.InputSample(SideChain[Sample]);
   Output[0]^[Sample] := FDynamicProcesor.GainSample(Input[0, Sample]);
   Output[1]^[Sample] := FDynamicProcesor.GainSample(Input[1, Sample]);
  end;
end;

{ TLightweightFeedbackCompressorParamStaticSEModule }

class procedure TLightweightFeedbackCompressorParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lightweight Feedback Compressor';
   ID := 'DAV Lightweight Feedback Compressor';
  end;
end;

function TLightweightFeedbackCompressorParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5..10: with Properties^ do Direction := drIn;
 end;
end;

{ TLightweightFeedbackCompressorAutomatableSEModule }

class procedure TLightweightFeedbackCompressorAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Lightweight Feedback Compressor (Automatable)';
   ID := 'DAV Lightweight Feedback Compressor (Automatable)';
  end;
end;

function TLightweightFeedbackCompressorAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  5: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Ratio [1:x]';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  8: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  9: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
 10: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TLightweightFeedbackCompressorAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  5..10: if (Pin[ 5].Status <> stRun) and
            (Pin[ 6].Status <> stRun) and
            (Pin[ 7].Status <> stRun) and
            (Pin[ 8].Status <> stRun) and
            (Pin[ 9].Status <> stRun) and
            (Pin[10].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TLightweightFeedbackCompressorAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  SideChain : PDAVSingleFixedArray;
  Input     : array [0..1] of PDAVSingleFixedArray;
  Output    : array [0..1] of PDAVSingleFixedArray;
  Thres     : PDAVSingleFixedArray;
  Rtio      : PDAVSingleFixedArray;
  Knee      : PDAVSingleFixedArray;
  Attck     : PDAVSingleFixedArray;
  Relse     : PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 SideChain := PDAVSingleFixedArray(@FSideChainBuffer[BufferOffset]);
 Input[0]  := PDAVSingleFixedArray(@FInputBuffer[0, BufferOffset]);
 Input[1]  := PDAVSingleFixedArray(@FInputBuffer[1, BufferOffset]);
 Output[0] := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 Output[1] := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);
 Knee      := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Thres     := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Rtio      := PDAVSingleFixedArray(@FRatio[BufferOffset]);
 Attck     := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Relse     := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  with FDynamicProcesor do
   begin
    Threshold_dB := 10 * Thres[Sample];
    Knee_dB := 10 * Knee[Sample];
    Ratio := 10 * Rtio[Sample];
    Attack := 10 * Attck[Sample];
    Release := 10 * Relse[Sample];
    InputSample(SideChain[Sample]);
    Output[0]^[Sample] := GainSample(Input[0, Sample]);
    Output[1]^[Sample] := GainSample(Input[1, Sample]);
   end;
end;

end.
