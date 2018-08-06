unit SEDitherNoiseshaperModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspDitherNoiseshaper;

type
  // define some constants to make referencing in/outs clearer
  TSEDitherNoiseshaperPins = (pinInput, pinOutput, pinBitDepth, pinLimit,
    pinDitherType, pinDitherAmplitude, pinNoiseshaperType);

  TCustomSEDitherNoiseshaperModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FDitherNoiseshaper : TDitherNoiseshaper32;
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

  TSEDitherNoiseshaperStaticModule = class(TCustomSEDitherNoiseshaperModule)
  private
    FBitDepth        : Integer;
    FLimit           : Boolean;
    FDitherType      : TDitherType;
    FDitherAmplitude : Single;
    FNoiseshaperType : TNoiseShaperType;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEDitherNoiseshaperControllableModule = class(TSEDitherNoiseshaperStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEDitherNoiseshaperModule }

constructor TCustomSEDitherNoiseshaperModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FDitherNoiseshaper := TDitherNoiseshaper32.Create
end;

destructor TCustomSEDitherNoiseshaperModule.Destroy;
begin
 FreeAndNil(FDitherNoiseshaper);
 inherited;
end;

procedure TCustomSEDitherNoiseshaperModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEDitherNoiseshaperModule.SampleRateChanged;
begin
 inherited;
// FDitherNoiseshaper.SampleRate := SampleRate;
end;

procedure TCustomSEDitherNoiseshaperModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEDitherNoiseshaperModule.ChooseProcess;
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
class procedure TCustomSEDitherNoiseshaperModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEDitherNoiseshaperModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEDitherNoiseshaperPins(index) of
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
procedure TCustomSEDitherNoiseshaperModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEDitherNoiseshaperPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
 end;
end;


{ TSEDitherNoiseshaperStaticModule }

// describe your module
class procedure TSEDitherNoiseshaperStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Dither & Noiseshaper (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Dither & Noiseshaper (static)';
  end;
end;

procedure TSEDitherNoiseshaperStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FDitherNoiseshaper.ProcessFloat(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSEDitherNoiseshaperStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEDitherNoiseshaperPins(index) of
  pinBitDepth:
   with Properties^ do
    begin
     Name            := 'Bit Depth';
     VariableAddress := @FBitDepth;
     Direction       := drParameter;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range 1,32';
     DefaultValue    := '16';
     Result          := True;
    end;
  pinLimit:
   with Properties^ do
    begin
     Name            := 'Limit';
     VariableAddress := @FLimit;
     Direction       := drParameter;
     Datatype        := dtBoolean;
     DefaultValue    := 'True';
     Result          := True;
    end;
  pinDitherType:
   with Properties^ do
    begin
     Name            := 'Dither Type';
     VariableAddress := @FDitherType;
     Direction       := drParameter;
     Datatype        := dtEnum;
     DatatypeExtra   := 'None, Rectangle, Triangular, Gauss, Fast Gauss';
     Result          := True;
    end;
  pinDitherAmplitude:
   with Properties^ do
    begin
     Name            := 'Dither Amplitude [Steps]';
     VariableAddress := @FDitherAmplitude;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
  pinNoiseshaperType:
   with Properties^ do
    begin
     Name            := 'Noiseshaper Type';
     VariableAddress := @FNoiseshaperType;
     Direction       := drParameter;
     Datatype        := dtEnum;
     DatatypeExtra   := 'None, Error Feedback, Simple Highpass (2nd Order), ' +
       'mod. E-weighting (2nd Order), mod. E-weighting (3rd Order), ' +
       'mod. E-weighting (5th Order), improved E-weighting (9th Order), ' +
       'improved E-weighting (9th Order), F-weighting (9th Order), ' +
       'F-weighting (3rd Order), Sony "Super Bit Mapping", ' +
       'Reduced "Super Bit Mapping", Sharp 14k (7th Order), ' +
       'Sharp 15k (8th Order), Sharp 16k (9th Order), Experimental';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEDitherNoiseshaperStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEDitherNoiseshaperPins(CurrentPin.PinID) of
          pinBitDepth : FDitherNoiseshaper.BitDepth := FBitDepth;
             pinLimit : FDitherNoiseshaper.Limit := FLimit;
        pinDitherType : FDitherNoiseshaper.DitherType := FDitherType;
   pinDitherAmplitude : FDitherNoiseshaper.DitherAmplitude := FDitherAmplitude;
   pinNoiseshaperType : FDitherNoiseshaper.NoiseshaperType := FNoiseshaperType;
 end;
end;


{ TSEDitherNoiseshaperControllableModule }

class procedure TSEDitherNoiseshaperControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Dither & Noiseshaper';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Dither & Noiseshaper';
  end;
end;

function TSEDitherNoiseshaperControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEDitherNoiseshaperPins(index) in [pinBitDepth..pinNoiseshaperType]
  then with Properties^ do Direction := drIn;
end;

end.
