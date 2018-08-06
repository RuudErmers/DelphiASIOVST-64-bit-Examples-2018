unit SELowLatencyConvolutionModule;

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
  Windows, Classes, SysUtils, SyncObjs, DAV_Types, DAV_SECommon, DAV_SEModule,
  DAV_Complex, DAV_DspConvolution, DAV_AudioData;

type
  // define some constants to make referencing in/outs clearer
  TSELowLatencyConvolutionPins = (pinInput, pinOutput, pinFileName,
    pinMaxFFTOrder, pinDesiredLatency, pinRealLatency);

  TSELowLatencyConvolutionModule = class(TSEModuleBase)
  private
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FInputBuffer         : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer        : PDAVSingleFixedArray;

    FConvolver           : TLowLatencyConvolution32;
    FImpulseResponse     : TAudioData32;


    FCriticalSection     : TCriticalSection;
    FStaticCount         : Integer;
    FFileName            : PAnsiChar;
    FMaxIRBlockOrder     : Integer;
    FRealLatency         : Integer;
    FDesiredLatencyIndex : Integer;

    FContainedIRs        : TStringList;

    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
    procedure SubProcessBypass(const BufferOffset, SampleFrames: Integer);

    procedure LoadIR(FileName: TFileName); overload;
    procedure LoadIR(ID: Integer); overload;
    procedure LoadImpulseResponse;
  end;

implementation

uses
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

resourcestring
  RCStrSynthEditOnly = 'This module is not allowed to be embedded into a VST Plugin';

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

constructor TSELowLatencyConvolutionModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FFileName            := '';
 FCriticalSection     := TCriticalSection.Create;
 FConvolver           := TLowLatencyConvolution32.Create;
 FMaxIRBlockOrder     := FConvolver.MaximumIRBlockOrder;
 FDesiredLatencyIndex := 5;

 // create and enumerate contained IR resource names
 FContainedIRs := TStringList.Create;
 EnumResourceNames(HInstance, 'IR', @EnumNamesFunc, LongWord(FContainedIRs));

 // create impulse response storage
 FImpulseResponse := TAudioData32.Create;

 if FContainedIRs.Count > 0
  then NativeInt(FFileName) := 0
  else FFileName := '';
end;

destructor TSELowLatencyConvolutionModule.Destroy;
begin
 FreeAndNil(FCriticalSection);
 FreeAndNil(FContainedIRs);
 FreeAndNil(FConvolver);
 FreeAndNil(FImpulseResponse);

 inherited;
end;

procedure TSELowLatencyConvolutionModule.Open;
var
  SingleImpulse  : Single;
{$IFDEF Use_IPPS}
{$IFNDEF Registered}
  VSTHostParams  : TSECallVstHostParams;
  RegisteredName : array [0..99] of AnsiChar;
  ID             : Integer;
{$ENDIF}
{$ENDIF}
begin
 {$IFDEF Use_IPPS}
 {$IFNDEF Registered}
 try
  FillChar(RegisteredName[0], SizeOf(RegisteredName), 0);
  CallHost(SEAudioMasterGetRegisteredName, 0, 0, @RegisteredName);
 except
  RegisteredName := '';
 end;

 if (RegisteredName <> 'Treck.de') and
    (RegisteredName <> 'Boris Kovalev') and
    (RegisteredName <> 'Victor Pavia') and
    (RegisteredName <> 'Bruno Bordi') and
    (RegisteredName <> 'Xhun Audio') and
    (RegisteredName <> 'Andromeda Audio & Video Design')
  then
   try
    ID := $29A2A826;
    if CSepMagic <> (ID shl 1)
     then raise Exception.Create(RCStrSynthEditOnly);
    VSTHostParams.Opcode := 32;

    if CallHost(SEAudioMasterCallVstHost, 0, 0, @VSTHostParams) <> -1
     then raise Exception.Create(RCStrSynthEditOnly)
   except
//    on E: Exception do ShowMessage(E.Message);
    raise;
   end;
 {$ENDIF}
 {$ENDIF}

 inherited Open;

 SingleImpulse := 1;
 FConvolver.LoadImpulseResponse(@SingleImpulse, 1);

 // choose which function is used to process audio
 OnProcess := SubProcessBypass;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSELowLatencyConvolutionModule.SampleRateChanged;
begin
 inherited;

 LoadImpulseResponse;
end;

// The most important part, processing the audio
procedure TSELowLatencyConvolutionModule.SubProcessBypass(const BufferOffset, SampleFrames: Integer);
begin
 Move(FInputBuffer[BufferOffset], FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single));
end;

procedure TSELowLatencyConvolutionModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSELowLatencyConvolutionModule.ChooseProcess;
begin
 if (FContainedIRs.Count = 0) and (not FileExists(string(FFileName)))
  then OnProcess := SubProcessBypass
  else
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize + FConvolver.IRSize + FConvolver.Latency;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSELowLatencyConvolutionModule.GetModuleProperties(Properties : PSEModuleProperties);
var
  ContainedIRs : TStringList;
begin
 ContainedIRs := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'IR', @EnumNamesFunc, LongWord(ContainedIRs));

  with Properties^ do
   begin
    {$IFDEF Use_IPPS}
    if ContainedIRs.Count > 0 then
     begin
      Name := 'Low Latency Resource Convolution Module (IPP based)';
      ID   := 'IPP ResLL Convolution Module';
     end
    else
     begin
      Name := 'Low Latency Convolution Module (IPP based)';
      ID   := 'IPP Low Latency Convolution Module';
     end;
    {$ELSE}
    if ContainedIRs.Count > 0 then
     begin
      Name := 'Low Latency Convolution Module';
      ID   := 'DAV Low Latency Convolution Module';
     end
    else
     begin
      Name := 'Low Latency Resource Convolution Module';
      ID   := 'DAV ResLL Convolution Module';
     end;
    {$ENDIF}
    About      := 'by Christian-W. Budde';
    SdkVersion := CSeSdkVersion;
   end;
 finally
  FreeAndNil(ContainedIRs);
 end;
end;

// describe the pins (plugs)
function TSELowLatencyConvolutionModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
var
  str : AnsiString;
begin
 Result := True;
 case TSELowLatencyConvolutionPins(Index) of
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
  pinFileName:
    with Properties^ do
     begin
      if FContainedIRs.Count <= 0 then
       begin
        Name            := 'FileName';
        VariableAddress := @FFileName;
        Flags           := [iofFilename];
        Direction       := drIn;
        DataType        := dtText;
        DefaultValue    := 'IR.wav';
       end
      else
       begin
        Name            := 'IR ID';
        VariableAddress := @FFileName;
        Direction       := drIn;
        DataType        := dtEnum;
        DefaultValue    := '0';
        str             := 'range 0,' + AnsiString(IntToStr(FContainedIRs.Count - 1));
        DatatypeExtra   := PAnsiChar(str);
       end;
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
procedure TSELowLatencyConvolutionModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSELowLatencyConvolutionPins(CurrentPin.PinID) of
           pinInput : begin
                       ChooseProcess;
                       Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                      end;
        pinFileName : begin
                       if FContainedIRs.Count <= 0 then
                        begin
                         if FileExists(string(FFileName))
                          then LoadIR(string(StrPas(FFileName)));
                        end
                       else LoadIR(Integer(FFileName));
                       ChooseProcess;
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
                        if 5 + FDesiredLatencyIndex >= FConvolver.MaximumIRBlockOrder
                         then FDesiredLatencyIndex := FConvolver.MaximumIRBlockOrder - 5;
                        FConvolver.MinimumIRBlockOrder := 5 + FDesiredLatencyIndex;
                       finally
                        FCriticalSection.Leave;
                       end;
                      end;
 end; inherited;
end;

procedure TSELowLatencyConvolutionModule.LoadIR(FileName: TFileName);
var
  ADC : TAudioDataCollection32;
begin
 FCriticalSection.Enter;
 try
  ADC := TAudioDataCollection32.Create(nil);
  with ADC do
   try
    LoadFromFile(FileName);

    // copy impulse response
    FImpulseResponse.SampleCount := ADC.SampleFrames;
    FImpulseResponse.SampleRate := ADC.SampleRate;
    Move(ADC[0].ChannelDataPointer^, FImpulseResponse.ChannelDataPointer^,
      ADC.SampleFrames * SizeOf(Single));

    LoadImpulseResponse;
   finally
    FreeAndNil(ADC);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSELowLatencyConvolutionModule.LoadIR(ID: Integer);
var
  ADC : TAudioDataCollection32;
  RS  : TResourceStream;
begin
 if (ID >= 0) and (ID < FContainedIRs.Count) then
  begin
   FCriticalSection.Enter;
   try
    ADC := TAudioDataCollection32.Create(nil);
    with ADC do
     try
      RS := TResourceStream.Create(HInstance, FContainedIRs[ID], 'IR');
      try
       LoadFromStream(RS);
      finally
       FreeAndNil(RS);
      end;

      // copy impulse response
      FImpulseResponse.SampleCount := ADC.SampleFrames;
      FImpulseResponse.SampleRate := ADC.SampleRate;
      Move(ADC[0].ChannelDataPointer^, FImpulseResponse.ChannelDataPointer^,
        ADC.SampleFrames * SizeOf(Single));

      LoadImpulseResponse;
     finally
      FreeAndNil(ADC);
     end;
   finally
    FCriticalSection.Leave;
   end;
  end;
end;

procedure TSELowLatencyConvolutionModule.LoadImpulseResponse;
var
  SampleIndex : Integer;
  Ratio, Pos  : Single;
begin
 with TAudioData32.Create do
  try
   Ratio := FImpulseResponse.SampleRate / SampleRate;
   SampleCount := Round(FImpulseResponse.SampleCount / Ratio);

   Pos := 0;
   for SampleIndex := 0 to SampleCount - 1 do
    begin
     if Round(Pos) < FImpulseResponse.SampleCount
      then ChannelDataPointer^[SampleIndex] := FImpulseResponse.ChannelDataPointer^[Round(Pos)]
      else ChannelDataPointer^[SampleIndex] := 0;
     Pos := Pos + Ratio;
    end;

   FConvolver.LoadImpulseResponse(ChannelDataPointer, SampleCount);
  finally
   Free;
  end;
end;

procedure TSELowLatencyConvolutionModule.SubProcess(const BufferOffset, SampleFrames: Integer);
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

end.
