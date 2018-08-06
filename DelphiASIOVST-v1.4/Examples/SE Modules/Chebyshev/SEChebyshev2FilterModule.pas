unit SEChebyshev2FilterModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspFilterChebyshev,
  DAV_DspFilterChebyshevType2;

type
  // define some constants to make referencing in/outs clearer
  TSEChebyshev2FilterPins = (pinInput, pinOutput, pinFrequency, pinOrder,
    pinCorrectFrequency, pinStopband);

  TSECustomChebyshev2FilterModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FFilter       : TCustomChebyshev2Filter;
    FStaticCount  : Integer;
    class function GetModueName: AnsiString; virtual; abstract;
    procedure ChooseProcess;
    procedure Open; override;
    procedure SampleRateChanged; override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEStaticChebyshev2FilterModule = class(TSECustomChebyshev2FilterModule)
  protected
    FFrequency    : Single;
    FOrder        : Integer;
    FCorrectFreq  : Boolean;
    FStopband     : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;

    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEStaticChebyshev2FilterLPModule = class(TSEStaticChebyshev2FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEStaticChebyshev2FilterHPModule = class(TSEStaticChebyshev2FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEControlableChebyshev2FilterLPModule = class(TSEStaticChebyshev2FilterLPModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class function GetModueName: AnsiString; override;
  end;

  TSEControlableChebyshev2FilterHPModule = class(TSEStaticChebyshev2FilterHPModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class function GetModueName: AnsiString; override;
  end;

  TSEAutomatebleChebyshev2FilterModule = class(TSECustomChebyshev2FilterModule)
  protected
    FFreqBuffer  : PDAVSingleFixedArray;
    FRipplBuffer : PDAVSingleFixedArray;
    FCorrectFreq : Boolean;
    FOrder       : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  end;

  TSEAutomatebleChebyshev2FilterLPModule = class(TSEAutomatebleChebyshev2FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEAutomatebleChebyshev2FilterHPModule = class(TSEAutomatebleChebyshev2FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEAutomatebleXChebyshev2FilterModule = class(TSEAutomatebleChebyshev2FilterModule)
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEAutomatebleXChebyshev2FilterLPModule = class(TSEAutomatebleXChebyshev2FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEAutomatebleXChebyshev2FilterHPModule = class(TSEAutomatebleChebyshev2FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

implementation

uses
  SysUtils, DAV_Common;

destructor TSECustomChebyshev2FilterModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilter);
 inherited;
end;

procedure TSECustomChebyshev2FilterModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

// The most important part, processing the audio
procedure TSECustomChebyshev2FilterModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

// describe your module
class procedure TSECustomChebyshev2FilterModule.getModuleProperties(Properties : PSEModuleProperties);
var
  str : AnsiString;
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   str := AnsiString(GetModueName);
   Name := PAnsiChar(str);

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := PAnsiChar(str);

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSECustomChebyshev2FilterModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEChebyshev2FilterPins(index) of
  // typical input plug (inputs are listed first)
  pinInput: with Properties^ do
             begin
              Name            := 'Input';
              VariableAddress := @FInput1Buffer;
              Direction       := drIn;
              Datatype        := dtFSample;
              DefaultValue    := '0';
             end;

  // typical output plug
  pinOutput: with Properties^ do
              begin
               Name            := 'Output';
               VariableAddress := @FOutputBuffer;
               Direction       := drOut;
               Datatype        := dtFSample;
              end;
  pinFrequency: with Properties^ do Name := 'Frequency [kHz]';
  pinOrder: with Properties^ do Name := 'Order';
  pinCorrectFrequency: with Properties^ do Name := 'Correct Frequency';
  pinStopband: with Properties^ do Name := 'Stopband [dB]';
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

procedure TSECustomChebyshev2FilterModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSECustomChebyshev2FilterModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

{ TSEStaticChebyshev2FilterModule }

constructor TSEStaticChebyshev2FilterModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFrequency := 1;
 FOrder := 4;
 FStopband := 1;
end;

function TSEStaticChebyshev2FilterModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshev2FilterPins(index) of
  pinFrequency: with Properties^ do
                 begin
                  VariableAddress := @FFrequency;
                  Direction       := drParameter;
                  DataType        := dtSingle;
                  DefaultValue    := '1';
                 end;
  pinOrder: with Properties^ do
             begin
              VariableAddress := @FOrder;
              Direction       := drParameter;
              DataType        := dtEnum;
              DefaultValue    := '4';
              DatatypeExtra   := 'range -0,32';
             end;
  pinCorrectFrequency: with Properties^ do
                        begin
                         VariableAddress := @FCorrectFreq;
                         Direction       := drIn;
                         DataType        := dtBoolean;
                         DefaultValue    := '0';
                        end;
  pinStopband: with Properties^ do
              begin
               VariableAddress := @FStopband;
               Direction       := drParameter;
               DataType        := dtSingle;
               DefaultValue    := '-24';
              end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEStaticChebyshev2FilterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEChebyshev2FilterPins(CurrentPin.PinID) of
             pinInput : begin
                         ChooseProcess;
                         Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                        end;
         pinFrequency : FFilter.Frequency := 1E-5 + abs(1000 * FFrequency);
             pinOrder : FFilter.Order     := FOrder;
  pinCorrectFrequency : FFilter.FixFrequency := FCorrectFreq;
          pinStopband : FFilter.Stopband  := FStopband;
 end;
 inherited;
end;

procedure TSEStaticChebyshev2FilterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
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
   Output^[Sample] := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

{ TSEStaticChebyshev2FilterLPModule }

constructor TSEStaticChebyshev2FilterLPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev2LowpassFilter.Create;
 FFilter.SetFilterValues(FFrequency, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEStaticChebyshev2FilterLPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev II Lowpass (static)';
end;

{ TSEStaticChebyshev2FilterHPModule }

constructor TSEStaticChebyshev2FilterHPModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev2HighpassFilter.Create;
 FFilter.SetFilterValues(FFrequency, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEStaticChebyshev2FilterHPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev II Highpass (static)';
end;

{ TSEControlableChebyshev2FilterLPModule }

class function TSEControlableChebyshev2FilterLPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev II Lowpass';
end;

function TSEControlableChebyshev2FilterLPModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshev2FilterPins(index) of
  pinFrequency..pinStopband : with Properties^ do Direction := drIn;
 end;
end;

{ TSEControlableChebyshev2FilterHPModule }

class function TSEControlableChebyshev2FilterHPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev II Highpass';
end;

function TSEControlableChebyshev2FilterHPModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshev2FilterPins(index) of
  pinFrequency..pinStopband : with Properties^ do Direction := drIn;
 end;
end;

{ TSEAutomatebleChebyshev2FilterModule }

constructor TSEAutomatebleChebyshev2FilterModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FOrder := 4;
end;

function TSEAutomatebleChebyshev2FilterModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshev2FilterPins(index) of
  pinFrequency: with Properties^ do
                 begin
                  VariableAddress := @FFreqBuffer;
                  Direction       := drIn;
                  DataType        := dtFSample;
                  DefaultValue    := '1';
                 end;
  pinOrder: with Properties^ do
             begin
              VariableAddress := @FOrder;
              Direction       := drIn;
              DataType        := dtEnum;
              DefaultValue    := '4';
              DatatypeExtra   := 'range -0,32';
             end;
  pinCorrectFrequency: with Properties^ do
                        begin
                         VariableAddress := @FCorrectFreq;
                         Direction       := drIn;
                         DataType        := dtBoolean;
                         DefaultValue    := '0';
                        end;
  pinStopband: with Properties^ do
                begin
                 VariableAddress := @FRipplBuffer;
                 Direction       := drIn;
                 DataType        := dtFSample;
                 DefaultValue    := '-24';
                end;
 end;
end;

procedure TSEAutomatebleChebyshev2FilterModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEChebyshev2FilterPins(CurrentPin.PinID) of
 pinInput : begin
             ChooseProcess;
             Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
            end;
  pinOrder: FFilter.Order := FOrder;
  pinCorrectFrequency: FFilter.FixFrequency := FCorrectFreq;
  pinFrequency,
  pinStopband:
    if (Pin[2].Status <> stRun) and (Pin[4].Status <> stRun)
     then OnProcess := SubProcessStatic
     else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TSEAutomatebleChebyshev2FilterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input    : PDAVSingleFixedArray;
  Output   : PDAVSingleFixedArray;
  Freq     : PDAVSingleFixedArray;
  Stopband : PDAVSingleFixedArray;
  Sample   : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input    := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output   := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq     := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Stopband := PDAVSingleFixedArray(@FRipplBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
   FFilter.Stopband  := 10 * Stopband[Sample];
   Output^[Sample]   := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

procedure TSEAutomatebleChebyshev2FilterModule.SubProcessStatic(
  const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample]   := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
end;

{ TSEAutomatebleChebyshev2FilterLPModule }

constructor TSEAutomatebleChebyshev2FilterLPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev2LowpassFilter.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleChebyshev2FilterLPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev II Lowpass (automatable)';
end;

{ TSEAutomatebleChebyshev2FilterHPModule }

constructor TSEAutomatebleChebyshev2FilterHPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev2HighpassFilter.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleChebyshev2FilterHPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev II Highpass (automatable)';
end;

{ TSEAutomatebleXChebyshev2FilterModule }

procedure TSEAutomatebleXChebyshev2FilterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input    : PDAVSingleFixedArray;
  Output   : PDAVSingleFixedArray;
  Freq     : PDAVSingleFixedArray;
  Stopband : PDAVSingleFixedArray;
  Sample   : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input    := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output   := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq     := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Stopband := PDAVSingleFixedArray(@FRipplBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   if (Sample div 2 = 0) then
    begin
     FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
     FFilter.Stopband  := 10 * Stopband[Sample];
    end;
   Output^[Sample]   := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

{ TSEAutomatebleXChebyshev2FilterLPModule }

constructor TSEAutomatebleXChebyshev2FilterLPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev2LowpassFilterAutomatable.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleXChebyshev2FilterLPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev II Lowpass (automatable+)';
end;

{ TSEAutomatebleXChebyshev2FilterHPModule }

constructor TSEAutomatebleXChebyshev2FilterHPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev2HighpassFilterAutomatable.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleXChebyshev2FilterHPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev II Highpass (automatable+)';
end;

end.
