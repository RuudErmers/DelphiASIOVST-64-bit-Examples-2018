unit SERealverbModule;

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
  SysUtils, SyncObjs, DAV_Types, DAV_SECommon, DAV_SEModule, DAV_Complex,
  DAV_HalfFloat, DAV_DspConvolution;

type
  // define some constants to make referencing in/outs clearer
  TSERealverbPins = (pinInput, pinOutput, pinMaxFFTOrder, pinDesiredLatency,
    pinRealLatency);

  TSERealverbModule = class(TSEModuleBase)
  private
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FInputBuffer         : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer        : PDAVSingleFixedArray;

    FConvolver           : TLowLatencyConvolution32;

    FCriticalSection     : TCriticalSection;
    FStaticCount         : Integer;
    FMaxIRBlockOrder     : Integer;
    FRealLatency         : Integer;
    FDesiredLatencyIndex : Integer;
    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

implementation

uses
  Classes;

resourcestring
  RCStrSynthEditOnly = 'This module is not allowed to be embedded into a VST Plugin';

constructor TSERealverbModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FCriticalSection     := TCriticalSection.Create;
 FConvolver           := TLowLatencyConvolution32.Create;
 FMaxIRBlockOrder     := FConvolver.MaximumIRBlockOrder;
 FRealLatency         := FConvolver.Latency;
 FDesiredLatencyIndex := 5;
end;

destructor TSERealverbModule.Destroy;
begin
 FreeAndNil(FCriticalSection);
 FreeAndNil(FConvolver);
 inherited;
end;

procedure TSERealverbModule.Open;
var
  i       : Integer;
  HFData  : array of THalfFloat;
  IRData  : TDAVSingleDynArray;
begin
 inherited Open;
 with TResourceStream.Create(HInstance, 'Left', 'RAW') do
  try
   SetLength(HFData, Size div SizeOf(THalfFloat));
   SetLength(IRData, Length(HFData));
   Read(HFData[0], Size);
   for i := 0 to Length(HFData) - 1
    do IRData[i] := HalfFloatToSingle(HFData[i]);
   FConvolver.LoadImpulseResponse(IRData);
  finally
   Free;
  end;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSERealverbModule.SampleRateChanged;
begin
 inherited;
 // ignore
end;

// The most important part, processing the audio
procedure TSERealverbModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSERealverbModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 // lock processing
 FCriticalSection.Enter;
 try
  FConvolver.ProcessBlock(PDAVSingleFixedArray(@FInputBuffer[BufferOffset]),
                          PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]),
                          SampleFrames);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSERealverbModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize + FConvolver.IRSize + FConvolver.Latency;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSERealverbModule.GetModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   Name       := 'Realverb Module';
   ID         := 'DAV Realverb Module';
   About      := 'by Christian-W. Budde';
   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSERealverbModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSERealverbPins(index) of
  pinInput:
    with Properties^ do
     begin
      Name            := 'Input';
      VariableAddress := @FInputBuffer;
      Flags           := [iofLinearInput];
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
  pinMaxFFTOrder:
    with Properties^ do
     begin
      Name            := 'Maximum FFT Order';
      VariableAddress := @FMaxIRBlockOrder;
      Direction       := drIn;
      DataType        := dtEnum;
      DefaultValue    := '16';
      DatatypeExtra   := 'range 6,20';
     end;
  pinDesiredLatency:
    with Properties^ do
     begin
      Name            := 'Desired Latency';
      VariableAddress := @FDesiredLatencyIndex;
      Direction       := drParameter;
      DataType        := dtEnum;
      DefaultValue    := '2';
      DatatypeExtra   := '64, 128, 256, 512, 1024, 2048, 4096, 8192';
     end;
  pinRealLatency:
    with Properties^ do
     begin
      Name            := 'Real Latency';
      VariableAddress := @FRealLatency;
      Direction       := drOut;
      DataType        := dtInteger;
     end;
  else Result := False; // host will ask for plugs 0, 1, 2, 3 etc. return false to signal when done
 end;;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSERealverbModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSERealverbPins(CurrentPin.PinID) of
           pinInput : begin
                       ChooseProcess;
                       Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                      end;
     pinMaxFFTOrder : begin
                       FCriticalSection.Enter;
                       try
                        if FMaxIRBlockOrder <= FConvolver.MinimumIRBlockOrder
                         then FMaxIRBlockOrder := FConvolver.MinimumIRBlockOrder + 1;
                        FConvolver.MaximumIRBlockOrder := FMaxIRBlockOrder
                       finally
                        FCriticalSection.Leave;
                       end;
                      end;
  pinDesiredLatency : begin
                       FCriticalSection.Enter;
                       try
                        FConvolver.MinimumIRBlockOrder := 5 + FDesiredLatencyIndex;
                        FRealLatency := FConvolver.Latency;
                       finally
                        FCriticalSection.Leave;
                       end;
                      end;
 end; inherited;
end;


end.
