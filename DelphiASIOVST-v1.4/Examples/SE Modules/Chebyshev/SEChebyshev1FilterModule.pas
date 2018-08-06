unit SEChebyshev1FilterModule;

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
  DAV_DspFilterChebyshevType1;

type
  // define some constants to make referencing in/outs clearer
  TSEChebyshev1FilterPins = (pinInput, pinOutput, pinFrequency, pinOrder,
    pinRipple);

  TSECustomChebyshev1FilterModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FFilter       : TCustomChebyshev1Filter;
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

  TSEStaticChebyshev1FilterModule = class(TSECustomChebyshev1FilterModule)
  protected
    FFrequency    : Single;
    FOrder        : Integer;
    FRipple       : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;

    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEStaticChebyshev1FilterLPModule = class(TSEStaticChebyshev1FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEStaticChebyshev1FilterHPModule = class(TSEStaticChebyshev1FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEControlableChebyshev1FilterLPModule = class(TSEStaticChebyshev1FilterLPModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class function GetModueName: AnsiString; override;
  end;

  TSEControlableChebyshev1FilterHPModule = class(TSEStaticChebyshev1FilterHPModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class function GetModueName: AnsiString; override;
  end;

  TSEAutomatebleChebyshev1FilterModule = class(TSECustomChebyshev1FilterModule)
  protected
    FFreqBuffer  : PDAVSingleFixedArray;
    FRipplBuffer : PDAVSingleFixedArray;
    FOrder       : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  end;

  TSEAutomatebleChebyshev1FilterLPModule = class(TSEAutomatebleChebyshev1FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEAutomatebleChebyshev1FilterHPModule = class(TSEAutomatebleChebyshev1FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEAutomatebleXChebyshev1FilterModule = class(TSEAutomatebleChebyshev1FilterModule)
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEAutomatebleXChebyshev1FilterLPModule = class(TSEAutomatebleXChebyshev1FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEAutomatebleXChebyshev1FilterHPModule = class(TSEAutomatebleChebyshev1FilterModule)
  protected
    class function GetModueName: AnsiString; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

implementation

uses
  SysUtils, DAV_Common;

destructor TSECustomChebyshev1FilterModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilter);
 inherited;
end;

procedure TSECustomChebyshev1FilterModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

// The most important part, processing the audio
procedure TSECustomChebyshev1FilterModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

// describe your module
class procedure TSECustomChebyshev1FilterModule.getModuleProperties(Properties : PSEModuleProperties);
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
function TSECustomChebyshev1FilterModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEChebyshev1FilterPins(index) of
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
  pinRipple: with Properties^ do Name := 'Ripple [dB]';
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

procedure TSECustomChebyshev1FilterModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSECustomChebyshev1FilterModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

{ TSEStaticChebyshev1FilterModule }

constructor TSEStaticChebyshev1FilterModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFrequency := 1;
 FOrder := 4;
 FRipple := 1;
end;

function TSEStaticChebyshev1FilterModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshev1FilterPins(index) of
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
  pinRipple: with Properties^ do
              begin
               VariableAddress := @FRipple;
               Direction       := drParameter;
               DataType        := dtSingle;
               DefaultValue    := '1';
              end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEStaticChebyshev1FilterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEChebyshev1FilterPins(CurrentPin.PinID) of
      pinInput : begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
  pinFrequency : FFilter.Frequency := 1E-5 + abs(1000 * FFrequency);
      pinOrder : FFilter.Order     := FOrder;
     pinRipple : FFilter.Ripple    := FRipple;
 end;
 inherited;
end;

procedure TSEStaticChebyshev1FilterModule.SubProcess(const BufferOffset,
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

{ TSEStaticChebyshev1FilterLPModule }

constructor TSEStaticChebyshev1FilterLPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1LowpassFilter.Create;
 FFilter.SetFilterValues(FFrequency, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEStaticChebyshev1FilterLPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev I Lowpass (static)';
end;

{ TSEStaticChebyshev1FilterHPModule }

constructor TSEStaticChebyshev1FilterHPModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1HighpassFilter.Create;
 FFilter.SetFilterValues(FFrequency, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEStaticChebyshev1FilterHPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev I Highpass (static)';
end;

{ TSEControlableChebyshev1FilterLPModule }

class function TSEControlableChebyshev1FilterLPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev I Lowpass';
end;

function TSEControlableChebyshev1FilterLPModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshev1FilterPins(index) of
  pinFrequency..pinRipple : with Properties^ do Direction := drIn;
 end;
end;

{ TSEControlableChebyshev1FilterHPModule }

class function TSEControlableChebyshev1FilterHPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev I Highpass';
end;

function TSEControlableChebyshev1FilterHPModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshev1FilterPins(index) of
  pinFrequency..pinRipple : with Properties^ do Direction := drIn;
 end;
end;

{ TSEAutomatebleChebyshev1FilterModule }

constructor TSEAutomatebleChebyshev1FilterModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FOrder := 4;
end;

function TSEAutomatebleChebyshev1FilterModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshev1FilterPins(index) of
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
  pinRipple: with Properties^ do
              begin
               VariableAddress := @FRipplBuffer;
               Direction       := drIn;
               DataType        := dtFSample;
               DefaultValue    := '1';
              end;
 end;
end;

procedure TSEAutomatebleChebyshev1FilterModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEChebyshev1FilterPins(CurrentPin.PinID) of
 pinInput : begin
             ChooseProcess;
             Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
            end;
  pinOrder: FFilter.Order := FOrder;
  pinFrequency,
  pinRipple:
    if (Pin[2].Status <> stRun) and (Pin[4].Status <> stRun)
     then OnProcess := SubProcessStatic
     else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TSEAutomatebleChebyshev1FilterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Ripple : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Ripple := PDAVSingleFixedArray(@FRipplBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
   FFilter.Ripple    := 10 * Ripple[Sample];
   Output^[Sample]   := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

procedure TSEAutomatebleChebyshev1FilterModule.SubProcessStatic(
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

{ TSEAutomatebleChebyshev1FilterLPModule }

constructor TSEAutomatebleChebyshev1FilterLPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1LowpassFilter.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleChebyshev1FilterLPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev I Lowpass (automatable)';
end;

{ TSEAutomatebleChebyshev1FilterHPModule }

constructor TSEAutomatebleChebyshev1FilterHPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1HighpassFilter.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleChebyshev1FilterHPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev I Highpass (automatable)';
end;

{ TSEAutomatebleXChebyshev1FilterModule }

procedure TSEAutomatebleXChebyshev1FilterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Ripple : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Ripple := PDAVSingleFixedArray(@FRipplBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   if (Sample div 2 = 0) then
    begin
     FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
     FFilter.Ripple    := 10 * Ripple[Sample];
    end;
   Output^[Sample]   := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

{ TSEAutomatebleXChebyshev1FilterLPModule }

constructor TSEAutomatebleXChebyshev1FilterLPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1LowpassFilterAutomatable.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleXChebyshev1FilterLPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev I Lowpass (automatable+)';
end;

{ TSEAutomatebleXChebyshev1FilterHPModule }

constructor TSEAutomatebleXChebyshev1FilterHPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1HighpassFilterAutomatable.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleXChebyshev1FilterHPModule.GetModueName: AnsiString;
begin
 Result := 'Chebyshev I Highpass (automatable+)';
end;

end.
