unit SEReverbModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_StkNReverb, DAV_StkJCReverb,
  DAV_DspFreeverb;

type
  // define some constants to make referencing in/outs clearer
  TSEFreeverbPins = (pinFvInput, pinFvOutput, pinRoomsize, pinDamp, pinWet,
    pinDry);
  TSEStkReverbPins = (pinInput, pinOutput, pinT60, pinEffectMix);
  TSEStkReverb2Pins = (pin2Input, pin2Output1, pin2Output2, pin2T60,
    pin2EffectMix);

  TCustomSEFreeverbModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FFreeverb : TFreeverb;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEFreeverbStaticModule = class(TCustomSEFreeverbModule)
  private
    FDamp      : Single;
    FRoomsize  : Single;
    FDry, FWet : Single;    
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEFreeverbControllableModule = class(TSEFreeverbStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TCustomSEStkNReverbModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FNReverb      : TStkNReverb;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEStkNReverbStaticModule = class(TCustomSEStkNReverbModule)
  private
    FT60       : Single;
    FEffectMix : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEStkNReverbControllableModule = class(TSEStkNReverbStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TCustomSEStkJCReverbModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FJCReverb     : TStkJCReverb;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEStkJCReverbStaticModule = class(TCustomSEStkJCReverbModule)
  private
    FT60       : Single;
    FEffectMix : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEStkJCReverbControllableModule = class(TSEStkJCReverbStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TCustomSEStkNReverb2Module = class(TSEModuleBase)
  private
    FInputBuffer   : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer1 : PDAVSingleFixedArray;
    FOutputBuffer2 : PDAVSingleFixedArray;
    FStaticCount   : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FNReverb      : TStkNReverb;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEStkNReverb2StaticModule = class(TCustomSEStkNReverb2Module)
  private
    FT60       : Single;
    FEffectMix : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEStkNReverb2ControllableModule = class(TSEStkNReverb2StaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TCustomSEStkJCReverb2Module = class(TSEModuleBase)
  private
    FInputBuffer   : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer1 : PDAVSingleFixedArray;
    FOutputBuffer2 : PDAVSingleFixedArray;
    FStaticCount   : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FJCReverb     : TStkJCReverb;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEStkJCReverb2StaticModule = class(TCustomSEStkJCReverb2Module)
  private
    FT60       : Single;
    FEffectMix : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEStkJCReverb2ControllableModule = class(TSEStkJCReverb2StaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils, DAV_StkReverb;

{ TCustomSEFreeverbModule }

constructor TCustomSEFreeverbModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FFreeverb := TFreeverb.Create
end;

destructor TCustomSEFreeverbModule.Destroy;
begin
 FreeAndNil(FFreeverb);
 inherited;
end;

procedure TCustomSEFreeverbModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEFreeverbModule.SampleRateChanged;
begin
 inherited;
 FFreeverb.SampleRate := SampleRate;
end;

procedure TCustomSEFreeverbModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEFreeverbModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize + Round(FFreeverb.RoomSize * SampleRate);
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSEFreeverbModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEFreeverbModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEFreeverbPins(index) of
  pinFvInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinFvOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEFreeverbModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverbPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
 end;
end;


{ TSEFreeverbStaticModule }

// describe your module
class procedure TSEFreeverbStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified Freeverb (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV modified Freeverb (static)';
  end;
end;

procedure TSEFreeverbStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FFreeverb.ProcessSample64(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSEFreeverbStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if not Result then
  case TSEFreeverbPins(index) of
   pinRoomsize:
    with Properties^ do
     begin
      Name            := 'Roomsize';
      VariableAddress := @FRoomsize;
      Direction       := drParameter;
      Datatype        := dtSingle;
      DefaultValue    := '1';
      Result          := True;
     end;
  pinDamp:
   with Properties^ do
    begin
     Name            := 'Damp';
     VariableAddress := @FDamp;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '30';
     Result          := True;
    end;
  pinWet:
   with Properties^ do
    begin
     Name            := 'Wet [%]';
     VariableAddress := @FWet;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '70';
     Result          := True;
    end;
  pinDry:
   with Properties^ do
    begin
     Name            := 'Dry [%]';
     VariableAddress := @FDry;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '30';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEFreeverbStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEFreeverbPins(CurrentPin.PinID) of
  pinRoomsize : FFreeverb.RoomSize := FRoomsize;
      pinDamp : FFreeverb.Damp := FDamp;
       pinWet : FFreeverb.Wet := 0.01 * FWet;
       pinDry : FFreeverb.Dry := 0.01 * FDry;
 end;
end;


{ TSEFreeverbControllableModule }

class procedure TSEFreeverbControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified Freeverb';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV modified Freeverb';
  end;
end;

function TSEFreeverbControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEStkReverbPins(index) in [pinT60..pinEffectMix]
  then with Properties^ do Direction := drIn;
end;


{ TCustomSEStkNReverbModule }

constructor TCustomSEStkNReverbModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FNReverb := TStkNReverb.Create
end;

destructor TCustomSEStkNReverbModule.Destroy;
begin
 FreeAndNil(FNReverb);
 inherited;
end;

procedure TCustomSEStkNReverbModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEStkNReverbModule.SampleRateChanged;
begin
 inherited;
 FNReverb.SampleRate := SampleRate;
end;

procedure TCustomSEStkNReverbModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEStkNReverbModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize + Round(FNReverb.T60 * SampleRate);
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSEStkNReverbModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEStkNReverbModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEStkReverbPins(index) of
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
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEStkNReverbModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverbPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
 end;
end;


{ TSEStkNReverbStaticModule }

// describe your module
class procedure TSEStkNReverbStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" Reverb Network (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK Reverb Network (static)';
  end;
end;

procedure TSEStkNReverbStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FNReverb.Tick(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSEStkNReverbStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEStkReverbPins(index) of
  pinT60:
   with Properties^ do
    begin
     Name            := 'T60 [s]';
     VariableAddress := @FT60;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
  pinEffectMix:
   with Properties^ do
    begin
     Name            := 'EffectMix [%]';
     VariableAddress := @FEffectMix;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '30';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEStkNReverbStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverbPins(CurrentPin.PinID) of
        pinT60: FNReverb.T60       := FT60;
  pinEffectMix: FNReverb.EffectMix := 0.01 * FEffectMix;
 end;
end;


{ TSEStkNReverbControllableModule }

class procedure TSEStkNReverbControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" Reverb Network';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK Reverb Network';
  end;
end;

function TSEStkNReverbControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEStkReverbPins(index) in [pinT60..pinEffectMix]
  then with Properties^ do Direction := drIn;
end;


{ TCustomSEStkJCReverbModule }

constructor TCustomSEStkJCReverbModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FJCReverb := TStkJCReverb.Create
end;

destructor TCustomSEStkJCReverbModule.Destroy;
begin
 FreeAndNil(FJCReverb);
 inherited;
end;

procedure TCustomSEStkJCReverbModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEStkJCReverbModule.SampleRateChanged;
begin
 inherited;
 FJCReverb.SampleRate := SampleRate;
end;

procedure TCustomSEStkJCReverbModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEStkJCReverbModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize + Round(FJCReverb.T60 * SampleRate);
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSEStkJCReverbModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEStkJCReverbModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEStkReverbPins(index) of
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
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEStkJCReverbModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverbPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
 end;
end;


{ TSEStkJCReverbStaticModule }

// describe your module
class procedure TSEStkJCReverbStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" JC Reverb (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK JC Reverb (static)';
  end;
end;

procedure TSEStkJCReverbStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FJCReverb.Tick(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSEStkJCReverbStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEStkReverbPins(index) of
  pinT60:
   with Properties^ do
    begin
     Name            := 'T60 [s]';
     VariableAddress := @FT60;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
  pinEffectMix:
   with Properties^ do
    begin
     Name            := 'EffectMix [%]';
     VariableAddress := @FEffectMix;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '30';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEStkJCReverbStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverbPins(CurrentPin.PinID) of
        pinT60: FJCReverb.T60       := FT60;
  pinEffectMix: FJCReverb.EffectMix := 0.01 * FEffectMix;
 end;
end;


{ TSEStkJCReverbControllableModule }

class procedure TSEStkJCReverbControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" JC Reverb';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK JC Reverb';
  end;
end;

function TSEStkJCReverbControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEStkReverbPins(index) in [pinT60..pinEffectMix]
  then with Properties^ do Direction := drIn;
end;

{ TCustomSEStkNReverb2Module }

constructor TCustomSEStkNReverb2Module.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FNReverb := TStkNReverb.Create
end;

destructor TCustomSEStkNReverb2Module.Destroy;
begin
 FreeAndNil(FNReverb);
 inherited;
end;

procedure TCustomSEStkNReverb2Module.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEStkNReverb2Module.SampleRateChanged;
begin
 inherited;
 FNReverb.SampleRate := SampleRate;
end;

procedure TCustomSEStkNReverb2Module.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0 then
  begin
   FillChar(FOutputBuffer1[BufferOffset], SampleFrames * SizeOf(Single), 0);
   FillChar(FOutputBuffer2[BufferOffset], SampleFrames * SizeOf(Single), 0);
   Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
   Pin[2].TransmitStatusChange(SampleClock, Pin[0].Status);
   CallHost(SEAudioMasterSleepMode);
  end;
end;

procedure TCustomSEStkNReverb2Module.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun then
  begin
   OnProcess := SubProcess;
   Pin[1].TransmitStatusChange(SampleClock, stRun);
   Pin[2].TransmitStatusChange(SampleClock, stRun);
  end
 else
  begin
   FStaticCount := BlockSize + Round(FNReverb.T60 * SampleRate);
   OnProcess := SubProcessStatic;
  end;
end;

// describe your module
class procedure TCustomSEStkNReverb2Module.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEStkNReverb2Module.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEStkReverb2Pins(Index) of
  pin2Input:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pin2Output1:
   with Properties^ do
    begin
     Name            := 'Output (left)';
     VariableAddress := @FOutputBuffer1;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pin2Output2:
   with Properties^ do
    begin
     Name            := 'Output (right)';
     VariableAddress := @FOutputBuffer2;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEStkNReverb2Module.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case CurrentPin.PinID of
  0: ChooseProcess;
 end;
end;


{ TSEStkNReverb2StaticModule }

// describe your module
class procedure TSEStkNReverb2StaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" Reverb2 Network (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK Reverb2 Network (static)';
  end;
end;

procedure TSEStkNReverb2StaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp1  : PDAVSingleFixedArray;
  Outp2  : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp1 := PDAVSingleFixedArray(@FOutputBuffer1[BufferOffset]);
 Outp2 := PDAVSingleFixedArray(@FOutputBuffer2[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FNReverb.Tick(Inp^[Sample]);
   Outp1^[Sample] := FNReverb.LastOutputLeft;
   Outp2^[Sample] := FNReverb.LastOutputRight;
  end;
end;

// describe the pins (plugs)
function TSEStkNReverb2StaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEStkReverb2Pins(index) of
  pin2T60:
   with Properties^ do
    begin
     Name            := 'T60 [s]';
     VariableAddress := @FT60;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
  pin2EffectMix:
   with Properties^ do
    begin
     Name            := 'EffectMix [%]';
     VariableAddress := @FEffectMix;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '30';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEStkNReverb2StaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverb2Pins(CurrentPin.PinID) of
        pin2T60: FNReverb.T60       := FT60;
  pin2EffectMix: FNReverb.EffectMix := 0.01 * FEffectMix;
 end;
end;


{ TSEStkNReverb2ControllableModule }

class procedure TSEStkNReverb2ControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" Reverb2 Network';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK Reverb2 Network';
  end;
end;

function TSEStkNReverb2ControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEStkReverb2Pins(index) in [pin2T60..pin2EffectMix]
  then with Properties^ do Direction := drIn;
end;


{ TCustomSEStkJCReverb2Module }

constructor TCustomSEStkJCReverb2Module.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FJCReverb := TStkJCReverb.Create
end;

destructor TCustomSEStkJCReverb2Module.Destroy;
begin
 FreeAndNil(FJCReverb);
 inherited;
end;

procedure TCustomSEStkJCReverb2Module.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEStkJCReverb2Module.SampleRateChanged;
begin
 inherited;
 FJCReverb.SampleRate := SampleRate;
end;

procedure TCustomSEStkJCReverb2Module.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0 then
  begin
   FillChar(FOutputBuffer1[BufferOffset], SampleFrames * SizeOf(Single), 0);
   FillChar(FOutputBuffer2[BufferOffset], SampleFrames * SizeOf(Single), 0);
   Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
   Pin[2].TransmitStatusChange(SampleClock, Pin[0].Status);
   CallHost(SEAudioMasterSleepMode);
  end;
end;

procedure TCustomSEStkJCReverb2Module.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun then
  begin
   Pin[1].TransmitStatusChange(SampleClock, stRun);
   Pin[2].TransmitStatusChange(SampleClock, stRun);
   OnProcess := SubProcess
  end
 else
  begin
   FStaticCount := BlockSize + Round(FJCReverb.T60 * SampleRate);
   OnProcess := SubProcessStatic;
  end;
end;

// describe your module
class procedure TCustomSEStkJCReverb2Module.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEStkJCReverb2Module.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEStkReverb2Pins(index) of
  pin2Input:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pin2Output1:
   with Properties^ do
    begin
     Name            := 'Output (left)';
     VariableAddress := @FOutputBuffer1;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pin2Output2:
   with Properties^ do
    begin
     Name            := 'Output (right)';
     VariableAddress := @FOutputBuffer2;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEStkJCReverb2Module.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case CurrentPin.PinID of
  0..1: ChooseProcess;
 end;
end;


{ TSEStkJCReverb2StaticModule }

// describe your module
class procedure TSEStkJCReverb2StaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" JC Stereo Reverb (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK JC Stereo Reverb (static)';
  end;
end;

procedure TSEStkJCReverb2StaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp1  : PDAVSingleFixedArray;
  Outp2  : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp1 := PDAVSingleFixedArray(@FOutputBuffer1[BufferOffset]);
 Outp2 := PDAVSingleFixedArray(@FOutputBuffer2[BufferOffset]);

 for Sample := 0 to SampleFrames -1 do
  begin
   FJCReverb.Tick(Inp^[Sample]);
   Outp1^[Sample] := FJCReverb.LastOutputLeft;
   Outp2^[Sample] := FJCReverb.LastOutputRight;
  end;
end;

// describe the pins (plugs)
function TSEStkJCReverb2StaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEStkReverb2Pins(index) of
  pin2T60:
   with Properties^ do
    begin
     Name            := 'T60 [s]';
     VariableAddress := @FT60;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
  pin2EffectMix:
   with Properties^ do
    begin
     Name            := 'EffectMix [%]';
     VariableAddress := @FEffectMix;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '30';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEStkJCReverb2StaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverb2Pins(CurrentPin.PinID) of
        pin2T60: FJCReverb.T60       := FT60;
  pin2EffectMix: FJCReverb.EffectMix := 0.01 * FEffectMix;
 end;
end;


{ TSEStkJCReverb2ControllableModule }

class procedure TSEStkJCReverb2ControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" JC Stereo Reverb';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK JC Stereo Reverb';
  end;
end;

function TSEStkJCReverb2ControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEStkReverb2Pins(index) in [pin2T60..pin2EffectMix]
  then with Properties^ do Direction := drIn;
end;

end.
