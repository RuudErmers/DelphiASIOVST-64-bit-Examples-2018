unit SEAudioRGBModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspPolyphaseDownsampler,
  DAV_DspPolyphaseUpsampler;

type
  // define some constants to make referencing in/outs clearer
  TSEAudioRGBPins = (pinInput, pinOutputR, pinOutputG, pinOutputB,
    pinCoefficients, pinTransition);

  TCustomSEAudioRGBModule = class(TSEModuleBase)
  private
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FStaticCount : Integer;
    procedure Open; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEAudioRGBModule = class(TCustomSEAudioRGBModule)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : array [0..2] of PDAVSingleFixedArray;
    FCoefficients : Integer;
    FTransition   : Single;
  protected
    FSplitter     : Array [0..1] of TPolyphaseDownsampler32;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TCustomSEColorConverterModule = class(TCustomSEAudioRGBModule)
  protected
    FInputBuffer  : array [0..2] of PDAVSingleFixedArray;
    FOutputBuffer : array [0..2] of PDAVSingleFixedArray;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  end;

  TSERGBToHSLModule = class(TCustomSEColorConverterModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEHSLToRGBModule = class(TCustomSEColorConverterModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

implementation

uses
  SysUtils, DAV_Common;

{ TCustomSEAudioRGBModule }

class procedure TCustomSEAudioRGBModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TCustomSEAudioRGBModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TCustomSEAudioRGBModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEAudioRGBModule.ChooseProcess;
begin
 if Pin[0].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;


{ TSEAudioRGBModule }

constructor TSEAudioRGBModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FSplitter[0] := TPolyphaseDownsampler32.Create;
 FSplitter[1] := TPolyphaseDownsampler32.Create;
end;

destructor TSEAudioRGBModule.Destroy;
begin
 FreeAndNil(FSplitter[0]);
 FreeAndNil(FSplitter[1]);
 inherited;
end;

// The most important part, processing the audio
procedure TSEAudioRGBModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp     : PDAVSingleFixedArray;
  R, G, B : PDAVSingleFixedArray;
  Data    : TDAV2SingleArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 R   := PDAVSingleFixedArray(@FOutputBuffer[0][BufferOffset]);
 G   := PDAVSingleFixedArray(@FOutputBuffer[1][BufferOffset]);
 B   := PDAVSingleFixedArray(@FOutputBuffer[2][BufferOffset]);
 Data[1] := 0;

 for Sample := 0 to SampleFrames - 1 do
  begin
   Data[0] := Inp^[Sample];
   FSplitter[0].ProcessSampleSplit(G^[Sample], B^[Sample], Data);
   Data[0] := G^[Sample];
   FSplitter[0].ProcessSampleSplit(R^[Sample], G^[Sample], Data);
  end;
end;

// describe your module
class procedure TSEAudioRGBModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Audio to RGB Converter';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Audio to RGB Converter';
  end;
end;

// describe the pins (plugs)
function TSEAudioRGBModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEAudioRGBPins(index) of
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
  pinOutputR:
   with Properties^ do
    begin
     Name            := 'Output (R)';
     VariableAddress := @FOutputBuffer[0];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinOutputG:
   with Properties^ do
    begin
     Name            := 'Output (G)';
     VariableAddress := @FOutputBuffer[1];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinOutputB:
   with Properties^ do
    begin
     Name            := 'Output (B)';
     VariableAddress := @FOutputBuffer[2];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinCoefficients:
   with Properties^ do
    begin
     Name            := 'Coefficients';
     VariableAddress := @FCoefficients;
     Direction       := drIn;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range -0,64';
     DefaultValue    := '8';
    end;
  pinTransition:
   with Properties^ do
    begin
     Name            := 'Transition';
     VariableAddress := @FTransition;
     Direction       := drIn;
     Datatype        := dtSingle;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEAudioRGBModule.PlugStateChange(const CurrentPin: TSEPin);
var
  OutState : TSEStateType;
begin
 inherited;

 case TSEAudioRGBPins(CurrentPin.PinID) of
         pinInput : begin
                     if (Pin[0].Status < stRun) and (Pin[0].Value = 0)
                      then OutState := stStatic
                      else OutState := stRun;
                     Pin[Integer(pinOutputR)].TransmitStatusChange(SampleClock, OutState);
                     Pin[Integer(pinOutputG)].TransmitStatusChange(SampleClock, OutState);
                     Pin[Integer(pinOutputB)].TransmitStatusChange(SampleClock, OutState);
                     ChooseProcess;
                    end;
  pinCoefficients : begin
                     FSplitter[0].NumberOfCoefficients := FCoefficients;
                     FSplitter[1].NumberOfCoefficients := FCoefficients;
                    end;
    pinTransition : begin
                     FSplitter[0].Transition := Limit(FTransition, 0.01, 0.499);
                     FSplitter[1].Transition := Limit(FTransition, 0.01, 0.499);
                    end;
 end;
end;


{ TCustomSEColorConverterModule }

// An input plug has changed value
procedure TCustomSEColorConverterModule.PlugStateChange(const CurrentPin: TSEPin);
var
  OutState : TSEStateType;
begin
 OutState := stRun;
 if (Pin[0].Status < stRun) and (Pin[0].Value = 0) then OutState := stStatic;
 if (Pin[1].Status < stRun) and (Pin[1].Value = 0) then OutState := stStatic;
 if (Pin[2].Status < stRun) and (Pin[2].Value = 0) then OutState := stStatic;

 Pin[3].TransmitStatusChange(SampleClock, OutState);
 Pin[4].TransmitStatusChange(SampleClock, OutState);
 Pin[5].TransmitStatusChange(SampleClock, OutState);
 inherited;
end;


procedure HSLToRGB(const H, S, L: Single; out R, G, B: Single);
var
  M1, M2: Single;
const
  OneThird: Single = 1 / 3;

  function HueToColorValue(Hue: Single): Single;
  begin
    Hue := Hue - Round(Hue - 0.5);
    if 6 * Hue < 1 then Result := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then Result := M2
    else if 3 * Hue < 2 then Result := M1 + (M2 - M1) * (2 * OneThird - Hue) * 6
    else Result := M1;
  end;

begin
 if S = 0
  then begin R := L; G := L; B := L; end
  else
   begin
    if L <= 0.5
     then M2 := L * (1 + S)
     else M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColorValue(H - OneThird);
    G := HueToColorValue(H           );
    B := HueToColorValue(H + OneThird)
  end;
end;

procedure RGBToHSL(const R, G, B: Single; out H, S, L: Single);
var
  D, Cmax, Cmin: Single;
const
  OneSixth: Single = 1 / 6;
begin
  if G > B then
   if R > G then Cmax := R else Cmax := G else
   if R > B then Cmax := R else Cmax := B;

  if G < B then
   if R < G then Cmin := R else Cmin := G else
   if R < B then Cmin := R else Cmin := B;

  L    := (Cmax + Cmin) * 0.5;

  if Cmax = Cmin then begin H := 0; S := 0 end
  else
   begin
    D := Cmax - Cmin;
    if L < 0.5
     then S := D / (Cmax + Cmin)
     else S := D / (2 - Cmax - Cmin);
    if R = Cmax then H := (G - B) / D else
    if G = Cmax
     then H := 2 + (B - R) / D
     else H := 4 + (R - G) / D;
    H := H * OneSixth;
    if H < 0 then H := H + 1;
   end;
end;


{ TSERGBToHSLModule }

// describe your module
class procedure TSERGBToHSLModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'RGB To HSL';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV RGB To HSL';
  end;
end;

// describe the pins (plugs)
function TSERGBToHSLModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  0:
   with Properties^ do
    begin
     Name            := 'Input (R)';
     VariableAddress := @FInputBuffer[0];
     Direction       := drIn;
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  1:
   with Properties^ do
    begin
     Name            := 'Input (G)';
     VariableAddress := @FInputBuffer[1];
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  2:
   with Properties^ do
    begin
     Name            := 'Input (b)';
     VariableAddress := @FInputBuffer[2];
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  3:
   with Properties^ do
    begin
     Name            := 'Output (H)';
     VariableAddress := @FOutputBuffer[0];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  4:
   with Properties^ do
    begin
     Name            := 'Output (S)';
     VariableAddress := @FOutputBuffer[1];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  5:
   with Properties^ do
    begin
     Name            := 'Output (L)';
     VariableAddress := @FOutputBuffer[2];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// The most important part, processing the audio
procedure TSERGBToHSLModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Rin, Gin, Bin    : PDAVSingleFixedArray;
  Hout, Sout, Lout : PDAVSingleFixedArray;
  Sample           : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Rin  := PDAVSingleFixedArray(@FInputBuffer[0][BufferOffset]);
 Gin  := PDAVSingleFixedArray(@FInputBuffer[1][BufferOffset]);
 Bin  := PDAVSingleFixedArray(@FInputBuffer[2][BufferOffset]);
 Hout := PDAVSingleFixedArray(@FOutputBuffer[0][BufferOffset]);
 Sout := PDAVSingleFixedArray(@FOutputBuffer[1][BufferOffset]);
 Lout := PDAVSingleFixedArray(@FOutputBuffer[2][BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do RGBToHSL(Rin[Sample], Gin[Sample], Bin[Sample], Hout[Sample], Sout[Sample], Lout[Sample]);
end;


{ TSEHSLToRGBModule }

// describe your module
class procedure TSEHSLToRGBModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'HSL to RGB';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV HSL to RGB';
  end;
end;

// describe the pins (plugs)
function TSEHSLToRGBModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  0:
   with Properties^ do
    begin
     Name            := 'Input (H)';
     VariableAddress := @FInputBuffer[0];
     Direction       := drIn;
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  1:
   with Properties^ do
    begin
     Name            := 'Input (S)';
     VariableAddress := @FInputBuffer[1];
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  2:
   with Properties^ do
    begin
     Name            := 'Input (L)';
     VariableAddress := @FInputBuffer[2];
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  3:
   with Properties^ do
    begin
     Name            := 'Output (R)';
     VariableAddress := @FOutputBuffer[0];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  4:
   with Properties^ do
    begin
     Name            := 'Output (G)';
     VariableAddress := @FOutputBuffer[1];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  5:
   with Properties^ do
    begin
     Name            := 'Output (B)';
     VariableAddress := @FOutputBuffer[2];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// The most important part, processing the audio
procedure TSEHSLToRGBModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Rout, Gout, Bout : PDAVSingleFixedArray;
  HIn, SIn, LIn    : PDAVSingleFixedArray;
  Sample           : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 HIn  := PDAVSingleFixedArray(@FInputBuffer[0][BufferOffset]);
 SIn  := PDAVSingleFixedArray(@FInputBuffer[1][BufferOffset]);
 LIn  := PDAVSingleFixedArray(@FInputBuffer[2][BufferOffset]);
 Rout := PDAVSingleFixedArray(@FOutputBuffer[0][BufferOffset]);
 Gout := PDAVSingleFixedArray(@FOutputBuffer[1][BufferOffset]);
 Bout := PDAVSingleFixedArray(@FOutputBuffer[2][BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do HSLToRGB(HIn[Sample], SIn[Sample], LIn[Sample], Rout[Sample], Gout[Sample], Bout[Sample]);
end;

end.
