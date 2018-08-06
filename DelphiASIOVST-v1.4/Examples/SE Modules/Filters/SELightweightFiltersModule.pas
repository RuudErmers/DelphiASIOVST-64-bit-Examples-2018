unit SELightweightFiltersModule;

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
  DAV_Types, DAV_DspFilter, DAV_DspFilterBasics, DAV_SECommon, DAV_SEModule,
  DAV_DspFilterBasicsAutomatable;

type
  // define some constants to make referencing in/outs clearer
  TSELightweightFiltersPins = (pinInput, pinOutput,
    {$IFDEF FilterReference} pinFilterReference, {$ENDIF}
    pinFrequency, pinGain, pinBandwidth, pinShape);

  TCustomSELightweightFiltersModule = class(TSEModuleBase)
  private
  protected
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FFilter       : TBiquadIIRFilter;
    {$IFDEF FilterReference}
    FFilterRef    : Pointer;
    {$ENDIF}
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure Open; override;
    procedure SampleRateChanged; override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TCustomSEGainFrequencyModule = class(TCustomSELightweightFiltersModule)
  protected
    FFreqBuffer      : PDAVSingleFixedArray;
    FGainBuffer      : PDAVSingleFixedArray;
    FBandwidthBuffer : PDAVSingleFixedArray;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSELightweightLowpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightHighpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightBandpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightNotchModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightLowshelfModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightLowshelfAModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightLowshelfBModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightHighshelfModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightHighshelfAModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightHighshelfBModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightPeakModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSELightweightAllpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TShapeFilter = class(TBasicPeakFilter)
  private
    FShape: Double;
    procedure SetShape(const Value: Double);
  protected
    procedure CalculateCoefficients; override;
    procedure BandwidthChanged; override;
  public
    property Shape : Double read FShape write SetShape;
  end;

  TSELightweightShapeModule = class(TCustomSEGainFrequencyModule)
  protected
    FShapeBuffer : PDAVSingleFixedArray;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

implementation

uses
  Math, SysUtils, DAV_Common;

constructor TCustomSELightweightFiltersModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 {$IFDEF FilterReference}
 FFilterRef := FFilter;
 {$ENDIF}
end;

destructor TCustomSELightweightFiltersModule.Destroy;
begin
 {$IFDEF FilterReference}
 FFilterRef := nil;
 Pin[Integer(pinFilterReference)].TransmitStatusChange(SampleClock, stRun);
 {$ENDIF}

 FreeAndNil(FFilter);
 inherited;
end;

procedure TCustomSELightweightFiltersModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 {$IFDEF FilterReference}
 FFilterRef := FFilter;
 Pin[Integer(pinFilterReference)].TransmitStatusChange(SampleClock, stRun);
 {$ENDIF}

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TCustomSELightweightFiltersModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  begin
   ChooseProcess;
   Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
  end;
end;

// The most important part, processing the audio
procedure TCustomSELightweightFiltersModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

procedure TCustomSELightweightFiltersModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
end;

procedure TCustomSELightweightFiltersModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSELightweightFiltersModule.ChooseProcess;
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
class procedure TCustomSELightweightFiltersModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSELightweightFiltersModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSELightweightFiltersPins(index) of
  // typical input plug (inputs are listed first)
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

  // typical output plug
  pinOutput:
    with Properties^ do
     begin
      Name            := 'Output';
      VariableAddress := @FOutputBuffer;
      Direction       := drOut;
      Datatype        := dtFSample;
     end;

  {$IFDEF FilterReference}
  // filter reference
  pinFilterReference:
    with Properties^ do
     begin
      Name            := 'Filter Reference';
      VariableAddress := @FFilterRef;
      Direction       := drOut;
      Datatype        := dtInteger;
     end;
  {$ENDIF}

  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;


{ TCustomSEGainFrequencyModule }

function TCustomSEGainFrequencyModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSELightweightFiltersPins(Index) of
  pinFrequency:
    with Properties^ do
     begin
      Name            := 'Frequency [kHz]';
      VariableAddress := @FFreqBuffer;
      Direction       := drIn;
      DataType        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '0.5';
      Result          := True;
     end;
  pinGain:
    with Properties^ do
     begin
      Name            := 'Gain [dB]';
      VariableAddress := @FGainBuffer;
      Direction       := drIn;
      DataType        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '0';
      Result          := True;
     end;
  pinBandwidth:
    with Properties^ do
     begin
      Name            := 'Bandwidth';
      VariableAddress := @FBandwidthBuffer;
      Direction       := drIn;
      DataType        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '0.5';
      Result          := True;
     end;
 end;
end;

procedure TCustomSEGainFrequencyModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Gain   : PDAVSingleFixedArray;
  BW     : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Gain   := PDAVSingleFixedArray(@FGainBuffer[BufferOffset]);
 BW     := PDAVSingleFixedArray(@FBandwidthBuffer[BufferOffset]); 

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   if Sample mod 2 = 0 then
    begin
     FFilter.Frequency := 10000 * Freq[Sample];
     FFilter.Gain      := 10 * Gain[Sample];
     FFilter.Bandwidth := 0.1 + 9.9 * abs(BW[Sample]);
    end;
   Output^[Sample]   := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;


{ TSELightweightLowpassModule }

constructor TSELightweightLowpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableLowpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightLowpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Lowpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Lowpass';
  end;
end;

{ TSELightweightHighpassModule }

constructor TSELightweightHighpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableHighpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightHighpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Highpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Highpass';
  end;
end;

{ TSELightweightBandpassModule }

constructor TSELightweightBandpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableBandpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightBandpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Bandpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Bandpass';
  end;
end;

{ TSELightweightNotchModule }

constructor TSELightweightNotchModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableNotchFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightNotchModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Notch';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Notch';
  end;
end;

{ TSELightweightLowshelfModule }

constructor TSELightweightLowshelfModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableLowShelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightLowshelfModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Lowshelf';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Lowshelf';
  end;
end;

{ TSELightweightHighshelfModule }

{ TSELightweightLowshelfAModule }

constructor TSELightweightLowshelfAModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableLowShelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightLowshelfAModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Lowshelf A';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Lowshelf A';
  end;
end;

{ TSELightweightLowshelfBModule }

constructor TSELightweightLowshelfBModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableLowShelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightLowshelfBModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Lowshelf B';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Lowshelf B';
  end;
end;

constructor TSELightweightHighshelfModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableHighshelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightHighshelfModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Highshelf';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Highshelf';
  end;
end;

{ TSELightweightHighshelfAModule }

constructor TSELightweightHighshelfAModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableHighshelfAFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightHighshelfAModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Highshelf A';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Highshelf A';
  end;
end;

{ TSELightweightHighshelfBModule }

constructor TSELightweightHighshelfBModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableHighshelfBFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightHighshelfBModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Highshelf B';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Highshelf B';
  end;
end;

{ TSELightweightPeakModule }

constructor TSELightweightPeakModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatablePeakFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightPeakModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Peak';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Peak';
  end;
end;

{ TSELightweightAllpassModule }

constructor TSELightweightAllpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TAutomatableAllpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightAllpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Allpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Allpass';
  end;
end;

{ TShapeFilter }

procedure TShapeFilter.BandwidthChanged;
var
  d : Double;
begin
 if abs(FShape) > 1
  then d := ln(1 + Power(FBandWidth, abs(FShape)))
  else d := ln(1 + FBandWidth);
 if abs(FShape) > 1
  then FAlpha := tan(FW0 * 0.5) * d / (cos(0.5 * FW0)) * 2
  else FAlpha := tan(FW0 * 0.5) * d / (cos(0.5 * FW0)) * Power(2, abs(FShape));
end;

procedure TShapeFilter.CalculateCoefficients;
var t, K, G, V, A  : Double;
begin
 if FShape < -1 then
  begin
   G := FGainFactor * (2 + FShape);
   V := Power(FGainFactor ,(2 + FShape));

   K := tan(FW0 * 0.5);
   A := Power(FGainFactor, - 0.5);

   t               := 1 / (sqr(K) / V + 1 + FAlpha * A);
   FDenominator[1] := 2 * (sqr(K) / V - 1) * t;
   FDenominator[2] := t * (sqr(K) / V + 1 - FAlpha * A);

   FNominator[0]   :=     (sqr(K) * G + FAlpha / A + 1) * t;
   FNominator[1]   := 2 * (sqr(K) * G              - 1) * t;
   FNominator[2]   :=     (sqr(K) * G - FAlpha / A + 1) * t;
  end else
 if FShape > 1 then
  begin
   G := FGainFactor * (2 - FShape);
   V := Power(FGainFactor ,(2 - FShape));

   K := tan(FW0 * 0.5);
   A := Power(FGainFactor, 0.5);

   t               := 1 / (sqr(K) * V + 1 + FAlpha * A);
   FDenominator[1] := 2 * (sqr(K) * V - 1) * t;
   FDenominator[2] := t * (sqr(K) * V + 1 - FAlpha * A);

   FNominator[0]   :=     V * (sqr(K) + FAlpha * A + G) * t;
   FNominator[1]   := 2 * V * (sqr(K)              - G) * t;
   FNominator[2]   :=     V * (sqr(K) - FAlpha * A + G) * t;
  end
 else
  begin
   if FShape < 0
    then G := 1
    else G := Power(FGainFactor, 2 * FShape);

   K := tan(FW0*0.5);
   V := Power(FGainFactor, FShape);
   A := Power(FGainFactor, sqr(FShape) + 0.5 * FShape - 1);

   t               := 1 / (sqr(K) * V + FAlpha * A + 1);
   FDenominator[1] := 2 * (sqr(K) * V              - 1) * t;
   FDenominator[2] := t * (sqr(K) * V - FAlpha * A + 1);

   FNominator[0]   :=     G * (sqr(K) / V + FAlpha / A + 1) * t;
   FNominator[1]   := 2 * G * (sqr(K) / V              - 1) * t;
   FNominator[2]   :=     G * (sqr(K) / V - FAlpha / A + 1) * t;
  end;

 CalculatePoleZeroes;
end;

procedure TShapeFilter.SetShape(const Value: Double);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   BandwidthChanged;
   CalculateCoefficients;
  end;
end;

{ TSELightweightShapeModule }

constructor TSELightweightShapeModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TShapeFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSELightweightShapeModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Filter Shape Peak';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Filter Shape Peak';
  end;
end;

function TSELightweightShapeModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSELightweightFiltersPins(Index) of
  pinShape:
    with Properties^ do
     begin
      Name            := 'Shape';
      VariableAddress := @FShapeBuffer;
      Direction       := drIn;
      DataType        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '0';
      Result          := True;
     end;
 end;
end;

procedure TSELightweightShapeModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Gain   : PDAVSingleFixedArray;
  BW     : PDAVSingleFixedArray;
  Sym    : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Gain   := PDAVSingleFixedArray(@FGainBuffer[BufferOffset]);
 BW     := PDAVSingleFixedArray(@FBandwidthBuffer[BufferOffset]);
 Sym    := PDAVSingleFixedArray(@FShapeBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   FFilter.Frequency := 10000 * Freq[Sample];
   FFilter.Gain      := 15 * Gain[Sample];
   FFilter.Bandwidth := 0.1 + 9.9 * abs(BW[Sample]);
   TShapeFilter(FFilter).Shape := Sym[Sample];
   Output^[Sample]   := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;


end.
