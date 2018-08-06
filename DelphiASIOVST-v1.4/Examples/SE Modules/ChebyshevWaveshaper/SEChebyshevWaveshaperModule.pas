unit SEChebyshevWaveshaperModule;

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
  DAV_Types, DAV_DspWaveshaper, DAV_SECommon, DAV_SEModule;

const
  CHarmonicCount = 24;

type
  // define some constants to make referencing in/outs clearer
  TSECustomChebyshevWaveshaperModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FWaveShaper   : TChebyshevWaveshaper;
    FStaticCount  : Integer;
    FOrder        : Integer;
    procedure Open; override;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEStaticChebyshevWaveshaperModule = class(TSECustomChebyshevWaveshaperModule)
  protected
    FHarmonics : array [0..CHarmonicCount - 1] of Single;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  end;

  TSEControlableChebyshevWaveshaperModule = class(TSEStaticChebyshevWaveshaperModule)
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEAutomatableChebyshevWaveshaperModule = class(TSECustomChebyshevWaveshaperModule)
  protected
    FHarmonics : array [0..CHarmonicCount - 1] of PDAVSingleFixedArray;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

implementation

uses
  SysUtils;

constructor TSECustomChebyshevWaveshaperModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FWaveShaper := TChebyshevWaveshaper.Create;
 FWaveShaper.Order := 24;
end;

destructor TSECustomChebyshevWaveshaperModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FWaveShaper);
 inherited;
end;

procedure TSECustomChebyshevWaveshaperModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[1].TransmitStatusChange(SampleClock, stRun);
end;

// The most important part, processing the audio
// describe your module
class procedure TSECustomChebyshevWaveshaperModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSECustomChebyshevWaveshaperModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
var
 str : AnsiString;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Input';
       VariableAddress := @FInput1Buffer;
       Direction       := drIn;
       Datatype        := dtFSAMPLE;
       DefaultValue    := '0';
      end;

  // typical output plug
  1: with Properties^ do
      begin
       Name            := 'Output';
       VariableAddress := @FOutputBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  2: Properties^.Name := 'Order';
  3: Properties^.Name := 'Fundamental';
  4..CHarmonicCount + 2:
     with Properties^ do
      begin
       str := AnsiString('Harmonic ' + IntToStr(Index - 3));
       Name            := PAnsiChar(str);
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSECustomChebyshevWaveshaperModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSECustomChebyshevWaveshaperModule.ChooseProcess;
begin
 if Pin[0].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;


{ TSEStaticChebyshevWaveshaperModule }

constructor TSEStaticChebyshevWaveshaperModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
var
  i : Integer;
begin
 inherited;
 for i := 0 to CHarmonicCount - 1
  do FHarmonics[i] := 0;
end;

class procedure TSEStaticChebyshevWaveshaperModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Chebyshev Waveshaper (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Chebyshev Waveshaper (static)';
  end;
end;

function TSEStaticChebyshevWaveshaperModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       VariableAddress := @FOrder;
       Direction       := drParameter;
       DataType        := dtEnum;
       DefaultValue    := '24';
       DatatypeExtra   := 'range 4,24';
      end;
  3: with Properties^ do
      begin
       VariableAddress := @FHarmonics[0];
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       DatatypeExtra   := 'range -1,1';
      end;
  4..CHarmonicCount + 2:
     with Properties^ do
      begin
       VariableAddress := @FHarmonics[Index - 3];
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '0';
       DatatypeExtra   := 'range -1,1';
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEStaticChebyshevWaveshaperModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case CurrentPin.PinID of
  0 : begin
       Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
       ChooseProcess;
      end;
  2 : FWaveShaper.Order := FOrder;
  3..CHarmonicCount + 1: if (CurrentPin.PinID - 3) < FWaveShaper.Order
                          then FWaveShaper.Gain[CurrentPin.PinID - 3] := FHarmonics[CurrentPin.PinID - 3];
 end;
end;

procedure TSEStaticChebyshevWaveshaperModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   // do the actual processing (multiplying the two input samples together)
   Output^[Sample] := FWaveShaper.ProcessSample64(Input[Sample]);
  end;
end;


{ TSEControlableChebyshevWaveshaperModule }

class procedure TSEControlableChebyshevWaveshaperModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Chebyshev Waveshaper';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Chebyshev Waveshaper';
  end;
end;

function TSEControlableChebyshevWaveshaperModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if Index in [2..CHarmonicCount + 2]
  then with Properties^ do Direction := drIn;
end;


{ TSEAutomatableChebyshevWaveshaperModule }

class procedure TSEAutomatableChebyshevWaveshaperModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Chebyshev Waveshaper (automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Chebyshev Waveshaper (automatable)';
  end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEAutomatableChebyshevWaveshaperModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case CurrentPin.PinID of
  0 : begin
       ChooseProcess;
       Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
      end;
  2 : FWaveShaper.Order := FOrder;
 end;
end;

function TSEAutomatableChebyshevWaveshaperModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       VariableAddress := @FOrder;
       Direction       := drIn;
       DataType        := dtEnum;
       DefaultValue    := '4';
       DatatypeExtra   := 'range 4,24';
      end;
  3: with Properties^ do
      begin
       VariableAddress := @FHarmonics[0];
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
      end;
  4..CHarmonicCount + 2:
     with Properties^ do
      begin
       VariableAddress := @FHarmonics[Index - 3];
       Direction       := drIn;
       DataType        := dtFSample;
      end;
 end;
end;

procedure TSEAutomatableChebyshevWaveshaperModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Sample, i : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   for i := 0 to FWaveShaper.Order - 1
    do FWaveShaper.Gain[i] := FHarmonics[i, BufferOffset + Sample];
   FOutputBuffer[BufferOffset + Sample] := FWaveShaper.ProcessSample64(FInput1Buffer[BufferOffset + Sample]);
  end;
end;

end.
