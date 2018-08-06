unit SELeslieModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspLeslie;

type
  // define some constants to make referencing in/outs clearer
  TSELesliePins = (pinInput, pinOutputLeft, pinOutputRight,
    pinLeslieSpeed, pinLowThrob, pinLowWidth, pinHighThrob, pinHighDepth,
    pinHighWidth, pinCrossover, pinOutputGain, pinSpeed);

  TCustomSELeslieModule = class(TSEModuleBase)
  private
    FInputBuffer    : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutLeftBuffer  : PDAVSingleFixedArray;
    FOutRightBuffer : PDAVSingleFixedArray;
    FStaticCount    : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FLeslie       : TLeslieRotator;
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

  TSELeslieStaticModule = class(TCustomSELeslieModule)
  private
    FCrossover   : Single;
    FLeslieSpeed : TLeslieSpeed;
    FLowThrob    : Single;
    FLowWidth    : Single;
    FHighThrob   : Single;
    FHighDepth   : Single;
    FHighWidth   : Single;
    FOutputGain  : Single;
    FSpeed       : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSELeslieControllableModule = class(TSELeslieStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSELeslieAutomatedModule = class(TSELeslieControllableModule)
  private
    FSpeedBuffer : PDAVSingleFixedArray;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

implementation

uses
  SysUtils;

{ TCustomSELeslieModule }

constructor TCustomSELeslieModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FLeslie := TLeslieRotator.Create
end;

destructor TCustomSELeslieModule.Destroy;
begin
 FreeAndNil(FLeslie);
 inherited;
end;

procedure TCustomSELeslieModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSELeslieModule.SampleRateChanged;
begin
 inherited;
 FLeslie.SampleRate := SampleRate;
end;

procedure TCustomSELeslieModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSELeslieModule.ChooseProcess;
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
class procedure TCustomSELeslieModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSELeslieModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSELesliePins(index) of
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
  pinOutputLeft:
   with Properties^ do
    begin
     Name            := 'Output Left';
     VariableAddress := @FOutLeftBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinOutputRight:
   with Properties^ do
    begin
     Name            := 'Output Right';
     VariableAddress := @FOutRightBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSELeslieModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSELesliePins(CurrentPin.PinID) of
  pinInput: begin
             ChooseProcess;
             Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
             Pin[2].TransmitStatusChange(SampleClock, Pin[0].Status);
            end;
 end;
end;


{ TSELeslieStaticModule }

// describe your module
class procedure TSELeslieStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Leslie (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Leslie (static)';
  end;
end;

procedure TSELeslieStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  OutL   : PDAVSingleFixedArray;
  OutR   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 OutL := PDAVSingleFixedArray(@FOutLeftBuffer[BufferOffset]);
 OutR := PDAVSingleFixedArray(@FOutRightBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do FLeslie.ProcessSample(Inp^[Sample], OutL^[Sample], OutR^[Sample]);
end;

// describe the pins (plugs)
function TSELeslieStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSELesliePins(index) of
  pinLeslieSpeed:
   with Properties^ do
    begin
     Name            := 'Leslie Speed';
     VariableAddress := @FLeslieSpeed;
     Direction       := drParameter;
     Datatype        := dtEnum;
     DatatypeExtra   := 'Stop, Slow, Fast';
     DefaultValue    := '2';
     Result          := True;
    end;
  pinLowThrob:
   with Properties^ do
    begin
     Name            := 'Low Throb [%]';
     VariableAddress := @FLowThrob;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '50';
     Result          := True;
    end;
  pinLowWidth:
   with Properties^ do
    begin
     Name            := 'Low Width [%]';
     VariableAddress := @FLowWidth;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '60';
     Result          := True;
    end;
  pinHighThrob:
   with Properties^ do
    begin
     Name            := 'High Throb [%]';
     VariableAddress := @FHighThrob;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '70';
     Result          := True;
    end;
  pinHighDepth:
   with Properties^ do
    begin
     Name            := 'High Depth [%]';
     VariableAddress := @FHighDepth;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '60';
     Result          := True;
    end;
  pinHighWidth:
   with Properties^ do
    begin
     Name            := 'High Width [%]';
     VariableAddress := @FHighWidth;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '70';
     Result          := True;
    end;
  pinCrossover:
   with Properties^ do
    begin
     Name            := 'Crossover';
     VariableAddress := @FCrossover;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '48';
     Result          := True;
    end;
  pinOutputGain:
   with Properties^ do
    begin
     Name            := 'Output Gain [dB]';
     VariableAddress := @FOutputGain;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0';
     Result          := True;
    end;
  pinSpeed:
   with Properties^ do
    begin
     Name            := 'Speed [%]';
     VariableAddress := @FSpeed;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '100';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSELeslieStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSELesliePins(CurrentPin.PinID) of
  pinLeslieSpeed : FLeslie.LeslieSpeed := FLeslieSpeed;
     pinLowThrob : FLeslie.LowThrob := FLowThrob;
     pinLowWidth : FLeslie.LowWidth := FLowWidth;
    pinHighThrob : FLeslie.HighThrob := FHighThrob;
    pinHighDepth : FLeslie.HighDepth := FHighDepth;
    pinHighWidth : FLeslie.HighWidth := FHighWidth;
    pinCrossover : FLeslie.Crossover := 0.01 * FCrossover;
   pinOutputGain : FLeslie.OutputGain := FOutputGain;
        pinSpeed : FLeslie.Speed := FSpeed;
 end;
end;


{ TSELeslieControllableModule }

class procedure TSELeslieControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Leslie';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Leslie';
  end;
end;

function TSELeslieControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSELesliePins(index) in [pinLeslieSpeed..pinSpeed]
  then with Properties^ do Direction := drIn;
end;

{ TSELeslieAutomatedModule }

class procedure TSELeslieAutomatedModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Leslie+';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Leslie+';
  end;
end;

function TSELeslieAutomatedModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSELesliePins(Index) = pinSpeed then
  with Properties^ do
   begin
    VariableAddress := @FSpeedBuffer;
    Direction       := drIn;
    Flags           := [iofLinearInput];
    Datatype        := dtFSample;
    DefaultValue    := '0';
   end;
end;

procedure TSELeslieAutomatedModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  OutL   : PDAVSingleFixedArray;
  OutR   : PDAVSingleFixedArray;
  Speed  : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 OutL  := PDAVSingleFixedArray(@FOutLeftBuffer[BufferOffset]);
 OutR  := PDAVSingleFixedArray(@FOutRightBuffer[BufferOffset]);
 Speed := PDAVSingleFixedArray(@FSpeedBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FLeslie.Speed := 100 * (1 + Speed^[Sample]);
   FLeslie.ProcessSample(Inp^[Sample], OutL^[Sample], OutR^[Sample]);
  end;
end;

end.
