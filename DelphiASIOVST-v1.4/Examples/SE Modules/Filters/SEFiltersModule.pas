unit SEFiltersModule;

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
  DAV_Types, DAV_DspFilter, DAV_DspFilterBasics, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEFiltersPins = (pinInput, pinOutput,
    {$IFDEF FilterReference} pinFilterReference, {$ENDIF}
    pinFrequency, pinGain, pinBandwidth, pinShape);

  TCustomSEFiltersModule = class(TSEModuleBase)
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

  TCustomSEGainFrequencyModule = class(TCustomSEFiltersModule)
  protected
    FFreqBuffer      : PDAVSingleFixedArray;
    FGainBuffer      : PDAVSingleFixedArray;
    FBandwidthBuffer : PDAVSingleFixedArray;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEBasicLowpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicHighpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicBandpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicNotchModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicLowshelfModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicLowshelfAModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicLowshelfBModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicHighshelfModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicHighshelfAModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicHighshelfBModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicPeakModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicAllpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicShapeModule = class(TCustomSEGainFrequencyModule)
  protected
    FShapeBuffer : PDAVSingleFixedArray;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  {$IFDEF FilterReference}
  TFilterCascadeModule = class(TSEModuleBase)
  protected
    FInputBuffer   : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer  : PDAVSingleFixedArray;
    FFilterCascade : TFilterCascade;
    FFilterRef     : Pointer;
    FFilterRefs    : array of Pointer;
    FStaticCount   : Integer;
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
  {$ENDIF}

implementation

uses
  Math, SysUtils, DAV_Common;

constructor TCustomSEFiltersModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 {$IFDEF FilterReference}
 FFilterRef := FFilter;
 {$ENDIF}
end;

destructor TCustomSEFiltersModule.Destroy;
begin
 {$IFDEF FilterReference}
 FFilterRef := nil;
 Pin[Integer(pinFilterReference)].TransmitStatusChange(SampleClock, stRun);
 {$ENDIF}

 FreeAndNil(FFilter);
 inherited;
end;

procedure TCustomSEFiltersModule.Open;
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

procedure TCustomSEFiltersModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEFiltersPins(CurrentPin.PinID) of
  pinInput : begin
              ChooseProcess;
              Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
             end;
 end;
end;

// The most important part, processing the audio
procedure TCustomSEFiltersModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

procedure TCustomSEFiltersModule.SubProcess(const BufferOffset, SampleFrames: Integer);
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

procedure TCustomSEFiltersModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEFiltersModule.ChooseProcess;
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
class procedure TCustomSEFiltersModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEFiltersModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEFiltersPins(index) of
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
      Datatype        := dtFilterReference;
      DefaultValue    := PAnsiChar(IntToStr(Integer(FFilterRef)));
//      Flags           := [iofHideWhenLocked];
     end;
  {$ENDIF}

  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;


{ TCustomSEGainFrequencyModule }

function TCustomSEGainFrequencyModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEFiltersPins(Index) of
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
   FFilter.Frequency := 10000 * Freq[Sample];
   FFilter.Gain      := 10 * Gain[Sample];
   FFilter.Bandwidth := 0.1 + 9.9 * abs(BW[Sample]);
   Output^[Sample]   := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;


{ TSEBasicLowpassModule }

constructor TSEBasicLowpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicLowpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicLowpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Lowpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Lowpass';
  end;
end;

{ TSEBasicHighpassModule }

constructor TSEBasicHighpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicHighpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicHighpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Highpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Highpass';
  end;
end;

{ TSEBasicBandpassModule }

constructor TSEBasicBandpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicBandpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicBandpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Bandpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Bandpass';
  end;
end;

{ TSEBasicNotchModule }

constructor TSEBasicNotchModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicNotchFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicNotchModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Notch';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Notch';
  end;
end;

{ TSEBasicLowshelfModule }

constructor TSEBasicLowshelfModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicLowShelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicLowshelfModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Lowshelf';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Lowshelf';
  end;
end;

{ TSEBasicLowshelfAModule }

constructor TSEBasicLowshelfAModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicLowShelfAFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicLowshelfAModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Lowshelf A';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Lowshelf A';
  end;
end;

{ TSEBasicLowshelfBModule }

constructor TSEBasicLowshelfBModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicLowShelfBFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicLowshelfBModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Lowshelf B';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Lowshelf B';
  end;
end;

{ TSEBasicHighshelfModule }

constructor TSEBasicHighshelfModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicHighshelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicHighshelfModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Highshelf';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Highshelf';
  end;
end;

{ TSEBasicHighshelfAModule }

constructor TSEBasicHighshelfAModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicHighshelfAFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicHighshelfAModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Highshelf A';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Highshelf A';
  end;
end;

{ TSEBasicHighshelfBModule }

constructor TSEBasicHighshelfBModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicHighshelfBFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicHighshelfBModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Highshelf B';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Highshelf B';
  end;
end;

{ TSEBasicPeakModule }

constructor TSEBasicPeakModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicPeakFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicPeakModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Peak';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Peak';
  end;
end;

{ TSEBasicAllpassModule }

constructor TSEBasicAllpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicAllpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicAllpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Allpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Allpass';
  end;
end;


{ TSEBasicShapeModule }

constructor TSEBasicShapeModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 FFilter := TBasicShapeFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
 inherited;
end;

class procedure TSEBasicShapeModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Shape Peak';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Shape Peak';
  end;
end;

function TSEBasicShapeModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEFiltersPins(Index) of
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

procedure TSEBasicShapeModule.SubProcess(const BufferOffset,
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
   TBasicShapeFilter(FFilter).Shape := Sym[Sample];
   Output^[Sample]   := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

{ TFilterCascadeModule }

{$IFDEF FilterReference}
constructor TFilterCascadeModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
 inherited;
 FFilterCascade := TFilterCascade.Create;
 FFilterCascade.OwnFilters := False;
 FFilterRef := FFilterCascade;
end;

destructor TFilterCascadeModule.Destroy;
begin
 FFilterRef := nil;
 Pin[Integer(pinFilterReference)].TransmitStatusChange(SampleClock, stRun);
 FreeAndNil(FFilterCascade);
 inherited;
end;

procedure TFilterCascadeModule.Open;
var
  DynamicPlugCount : Integer;
  Plug             : Integer;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 DynamicPlugCount := CallHost(SEAudioMasterGetTotalPinCount) - 3;
 SetLength(FFilterRefs, DynamicPlugCount);
 FFilterCascade.Clear;

 for Plug := 0 to DynamicPlugCount - 1 do
  try
   FFilterRefs[Plug] := Pointer(CallHost(SEAudioMasterGetPinVarAddress, 3 + Plug));
   if FFilterRefs[Plug] <> nil
    then FFilterCascade.AddFilter(TCustomFilter(FFilterRefs[Plug]^));
  except  
  end;

 // FFilterRef := FFilter;
 Pin[Integer(pinFilterReference)].TransmitStatusChange(SampleClock, stRun);

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TFilterCascadeModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

class procedure TFilterCascadeModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited;

 with Properties^ do
  begin
   Name := 'Filter Cascade';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SdkVersion := CSeSdkVersion;
  end;
end;

function TFilterCascadeModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  0: with Properties^ do
      begin
       Name            := 'Input';
       VariableAddress := @FInputBuffer;
       Direction       := drIn;
       Flags           := [iofLinearInput];
       Datatype        := dtFSample;
       DefaultValue    := '0';
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
       Name            := 'Filter Reference';
       VariableAddress := @FFilterRef;
       Direction       := drOut;
       Datatype        := dtFilterReference;
      end;
  3: with Properties^ do
      begin
       Name            := 'Filter Reference';
       Direction       := drIn;
       Datatype        := dtFilterReference;
       Flags           := [iofAutoDuplicate, iofRename];
      end;

  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

procedure TFilterCascadeModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEFiltersPins(CurrentPin.PinID) of
  pinInput : begin
              ChooseProcess;
              Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
             end;
 end;
end;

procedure TFilterCascadeModule.SampleRateChanged;
begin
 inherited;
 FFilterCascade.SampleRate := SampleRate;
end;

procedure TFilterCascadeModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FFilterCascade.ProcessSample64(Input[Sample] + cDenorm64);
end;

procedure TFilterCascadeModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;
{$ENDIF}

end.
