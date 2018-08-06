unit DAV_ASIOHostAudioData;

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
//  The initial developer of this code is Tobias Fleischer and                //
//  Christian-W. Budde, based on a code snipped by Frederic Vanmol            //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
//  Contributor(s):                                                           //
//    Martin Fay (original Delphi ASIO interface, author of OpenAsio)         //
//    Benjamin Rosseaux (author of the stdcall interface)                     //
//    Maik Menz (various refactorings)                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LclType, LMessages, LResources,
  {$ELSE}Windows, Messages,{$ENDIF}
  {$IFDEF OpenASIO} DAV_OpenAsio {$ELSE} DAV_AsioInterface {$ENDIF},
  {$IFDEF ASIOMixer} Forms, ComCtrls, Graphics, StdCtrls, DAVASIOMixer,{$ENDIF}
  {$IFDEF DELPHI5} Forms, DsgnIntf, {$ENDIF}
  SysUtils, Classes, Controls, DAV_Types, DAV_Asio, DAV_AsioList,
  DAV_AsioConvert, DAV_AsioGenerator, DAV_AsioHost, DAV_AudioData;

type
  TAsioAudioData32 = class(TAudioData32);
  TAsioAudioData64 = class(TAudioData64);

  TAsioAudioChannel32 = class(TAudioChannel32);
  TAsioAudioChannel64 = class(TAudioChannel64);

  TAsioAudioDataCollection32 = class(TCustomAudioDataCollection32)
  published
    property Channels;
    property SampleRate;
  end;

  TAsioAudioDataCollection64 = class(TCustomAudioDataCollection64)
  published
    property Channels;
    property SampleRate;
  end;

  TBufferSwitchAudioData32Event = procedure(Sender: TObject; const InBuffer, OutBuffer: TAsioAudioDataCollection32) of object;
  TBufferSwitchAudioData64Event = procedure(Sender: TObject; const InBuffer, OutBuffer: TAsioAudioDataCollection64) of object;

  {$IFDEF DELPHI10_UP} {$region 'TAsioHostAudioData'} {$ENDIF}
  TCustomAsioHostAudioData = class(TCustomAsioHostBasic)
  private
    FPreventClipping      : TPreventClipping;
    FInBufferPreFill      : TBufferPreFill;
    FOutBufferPreFill     : TBufferPreFill;

    FOnBufferSwitch32     : TBufferSwitchAudioData32Event;
    FOnBufferSwitch64     : TBufferSwitchAudioData64Event;

    FConvertOptimizations : TConvertOptimizations;
    FOutputVolume         : TDAVSingleDynArray;
    FClipPrevent          : TClipBuffer;
    FConvertMethod        : TConvertMethod;
    FOutputDither         : TAsioOutputDither;

    FAudioDataInput       : TCustomAudioDataCollection;
    FAudioDataOutput      : TCustomAudioDataCollection;

    {$IFDEF AsioMixer}
    FAsioMixer            : TFmAsioMixer;
    {$ENDIF}
    procedure SetConvertOptimizations(const Value: TConvertOptimizations);
    procedure SetConvertMethod(const Value: TConvertMethod);
    procedure SetPreventClipping(v: TPreventClipping);
    {$IFDEF AsioMixer}
    procedure SetupMixer;
    procedure VolumeChange(Sender: TObject);
    {$ENDIF}
    procedure SetOnBufferSwitch32(const Value: TBufferSwitchAudioData32Event);
    procedure SetOnBufferSwitch64(const Value: TBufferSwitchAudioData64Event);
  protected
    function CreateBuffers: Boolean; override;
    procedure BufferSwitchTimeInfo(Index: Integer; const params: TAsioTime); override;
    procedure DetermineBuffersize; override;
    procedure ConvertMethodChanged; virtual;
    procedure ConvertOptimizationsChanged; virtual;

    property ConvertMethod: TConvertMethod read FConvertMethod write SetConvertMethod default cmNone;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF AsioMixer}
    procedure Mixer;
    {$ENDIF}
    property ConvertOptimizations: TConvertOptimizations read FConvertOptimizations write SetConvertOptimizations;

    property OnBufferSwitch32: TBufferSwitchAudioData32Event read FOnBufferSwitch32 write SetOnBufferSwitch32;
    property OnBufferSwitch64: TBufferSwitchAudioData64Event read FOnBufferSwitch64 write SetOnBufferSwitch64;

    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill write FInBufferPreFill default bpfNone;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill write FOutBufferPreFill default bpfNone;
    property PreventClipping: TPreventClipping read FPreventClipping write SetPreventClipping default pcNone;
  end;

  TAsioHostAudioData = class(TCustomAsioHostAudioData)
  published
    property Active;
    property AsioTime;
    property BufferGranularity;
    property BufferMaximum;
    property BufferMinimum;
    property BufferPreferredSize;
    property BufferSize;
    property CanDos;
    property ConvertOptimizations;
    property DriverIndex;
    property DriverList;
    property DriverName;
    property DriverVersion;
    property InputChannelCount;
    property InputLatency;
    property OutputChannelCount;
    property OutputLatency;
    property PreFillInBuffer;
    property PreFillOutBuffer;
    property PreventClipping;
    property SampleRate;
    property Supports;
    property OnBuffersCreate;
    property OnBuffersDispose;
    property OnBufferSwitch32;
    property OnBufferSwitch64;
    property OnCreate;
    property OnDestroy;
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnReset;
    property OnSampleRateChanged;
    property OnUpdateSamplePos;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TAsioHostAudioData'} {$ENDIF}

implementation

uses
  Registry, ComObj, Math, DAV_Common {$IFDEF AsioMixer}, DAV_AsioChannelStrip {$ENDIF};

resourcestring
  RStrAsioDriverFailed        = 'Asio driver failed!';
  RStrAsioNoBuffersCreated    = 'Asio buffers could not be created!';
  RStrConverterTypeUnknown    = 'Converter type unknown';
  RCStrIndexOutOfBounds       = 'Index out of bounds %d';
  RCStrOnlyOneAsioHost        = 'Only one Asio host is allowed per instance';
  RCStrPreferedBufferSize     = 'Prefered buffer size invalid!';
  RCStrDriverNotPresent       = 'Driver not present';
  RCStrHardwareMalfunction    = 'Hardware malfunctioning';
  RCStrInputParameterInvalid  = 'Input parameter invalid';
  RCStrInvalidMode            = 'Hardware is in a bad mode or used in a bad mode';
  RCStrSPNotAdvancing         = 'Hardware is not running when sample position is inquired';
  RCStrNoClock                = 'Sample clock or rate cannot be determined or is not present';
  RCStrNoMemory               = 'Not enough memory for completing the request';

const
  CInprocServer = 'InprocServer32';
  CAsioPath     = 'software\asio';
  CComClsId     = 'clsid';

////////////////////////////////////////////////////////////////////////////////
////////////////////////// TCustomAsioHostAudioData ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TCustomAsioHostAudioData implementation'} {$ENDIF}

constructor TCustomAsioHostAudioData.Create(AOwner: TComponent);
begin
  FClipPrevent          := ClipDigital;
  FConvertOptimizations := [coSSE, co3DNow];
  FConvertMethod        := cmNone;
  FOutputDither         := odNone;

  {$IFDEF AsioMixer} FAsioMixer := TFmAsioMixer.Create(nil); {$ENDIF}
  inherited;
end;

destructor TCustomAsioHostAudioData.Destroy;
begin
 if assigned(FAudioDataInput)  then FreeAndNil(FAudioDataInput);
 if assigned(FAudioDataOutput) then FreeAndNil(FAudioDataOutput);

 {$IFDEF AsioMixer} FreeAndNil(FAsioMixer); {$ENDIF}
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TCustomAsioHostAudioData.SetOnBufferSwitch32(const Value: TBufferSwitchAudioData32Event);
begin
 FOnBufferSwitch32 := Value;
 if assigned(FOnBufferSwitch64) then ConvertMethod := cm64 else
 if assigned(FOnBufferSwitch32) then ConvertMethod := cm32
  else ConvertMethod := cmNone;
end;

procedure TCustomAsioHostAudioData.SetOnBufferSwitch64(const Value: TBufferSwitchAudioData64Event);
begin
 FOnBufferSwitch64 := Value;
 if assigned(FOnBufferSwitch64) then ConvertMethod := cm64 else
 if assigned(FOnBufferSwitch32) then ConvertMethod := cm32
  else ConvertMethod := cmNone;
end;

procedure TCustomAsioHostAudioData.ConvertMethodChanged;
var
  OldIn, OldOut  : TCustomAudioDataCollection;
begin
 OldIn  := FAudioDataInput;
 OldOut := FAudioDataOutput;
 case FConvertMethod of
  cm32 : begin
          FAudioDataInput  := TAsioAudioDataCollection32.Create(Self, InputChannelCount, BufferSize);
          FAudioDataOutput := TAsioAudioDataCollection32.Create(Self, OutputChannelCount, BufferSize);
         end;
  cm64 : begin
          FAudioDataInput  := TAsioAudioDataCollection64.Create(Self, InputChannelCount, BufferSize);
          FAudioDataOutput := TAsioAudioDataCollection64.Create(Self, OutputChannelCount, BufferSize);
         end;
 end;
 if assigned(OldIn)  then FreeAndNil(OldIn);
 if assigned(OldOut) then FreeAndNil(OldOut);
end;

procedure TCustomAsioHostAudioData.SetConvertMethod(
  const Value: TConvertMethod);
begin
 if FConvertMethod <> Value then
  begin
   FConvertMethod := Value;
   ConvertMethodChanged;
  end;
end;

procedure TCustomAsioHostAudioData.SetConvertOptimizations(const Value: TConvertOptimizations);
begin
 if FConvertOptimizations <> Value then
  begin
   FConvertOptimizations := Value;
   ConvertOptimizationsChanged;
  end;
end;

procedure TCustomAsioHostAudioData.ConvertOptimizationsChanged;
begin
 Use_FPU;
 case ProcessorType of
  ptSSE: if coSSE in FConvertOptimizations then Use_SSE;
  pt3DNow: if co3DNow in FConvertOptimizations then Use_3DNow;
 end;
end;

procedure TCustomAsioHostAudioData.SetPreventClipping(v : TPreventClipping);
begin
 FPreventClipping := v;
 case FPreventClipping of
  pcDigital: FClipPrevent := ClipDigital;
   pcAnalog: FClipPrevent := ClipAnalog;
 end;
end;

procedure TCustomAsioHostAudioData.DetermineBuffersize;
begin
 inherited;
 if assigned(FAudioDataInput) then
  with FAudioDataInput do
   begin
    ChannelCount := InputChannelCount;
    SampleFrames := BufferSize;
   end;
 if assigned(FAudioDataOutput) then
  with FAudioDataOutput do
   begin
    ChannelCount := InputChannelCount;
    SampleFrames := BufferSize;
   end;
end;

{$IFDEF AsioMixer}
procedure TCustomAsioHostAudioData.VolumeChange(Sender: TObject);
begin
 assert(Sender is TFrChannelStrip);
 with TFrChannelStrip(Sender) do
  begin
   FOutputVolume[Channel] := Volume;
   if Mute then FOutputVolume[Channel] := 0;
  end;
end;

procedure TCustomAsioHostAudioData.SetupMixer;
var
  Channel: Integer;
begin
 with FAsioMixer do
  begin
   for Channel := 0 to Length(ChannelsStrips) - 1
    do FreeAndNil(ChannelsStrips[Channel]);
   SetLength(ChannelsStrips, FOutputChannels);
   for Channel := FOutputChannels - 1 downto 0 do
    begin
     ChannelsStrips[Channel] := TFrChannelStrip.Create(FAsioMixer);
     with ChannelsStrips[Channel] do
      begin
       Width := 44;
       Name := 'ChannelStrip' + IntToStr(Channel);
       Parent := FAsioMixer.MixerPanel;
       Align := alLeft;
       OnVolumeChange := VolumeChange;
       OnMuteChange := VolumeChange;
       Channel := FOutputChannels - 1 - Channel;
      end; 
    end;
   if FOutputChannels > 0 then
    begin
     ClientHeight := 20 + ChannelsStrips[0].Height;
     ClientWidth := 20 + FOutputChannels * ChannelsStrips[0].Width;
    end;
  end;  
end;
{$ENDIF AsioMixer}

function TCustomAsioHostAudioData.CreateBuffers: Boolean;
var
  Channel : Integer;
begin
 Result := inherited CreateBuffers;

 if Result then
  begin
   SetLength(FOutputVolume, FOutputChannelCount);
   for Channel := 0 to FOutputChannelCount - 1 do FOutputVolume[Channel] := 1;
   {$IFDEF AsioMixer} SetupMixer; {$ENDIF}

   if assigned(FAudioDataInput)
    then FAudioDataInput.ChannelCount := FInputChannelCount;
   if assigned(FAudioDataOutput)
    then FAudioDataOutput.ChannelCount := FOutputChannelCount;
  end;
end;

{$IFDEF AsioMixer}
procedure TCustomAsioHostAudioData.Mixer;
begin
 FAsioMixer.Show;
end;
{$ENDIF}

procedure TCustomAsioHostAudioData.BufferSwitchTimeInfo(Index: Integer; const params: TAsioTime);
var
  Channel        : Integer;
  CurrentBuffer  : PAsioBufferInfos;
  PChannelArray  : Pointer;
begin
 if FDriver = nil then exit;
 PMUpdSamplePos.wParam := params.TimeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := params.TimeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);
 CurrentBuffer := FInputBuffers;

 if FConvertMethod = cm64 then
  begin
   // 64bit float processing

   // process input
   with TAsioAudioDataCollection64(FAudioDataInput) do
    case FInBufferPreFill of
      bpfZero : FAudioDataInput.Clear;
     bpfNoise : FAudioDataInput.GenerateWhiteNoise(1);
     else
      // convert soundcard dependent format to float data
      for Channel := 0 to FInputChannelCount - 1 do
       begin
        PChannelArray := CurrentBuffer^[0].buffers[Index];
        if Assigned(PChannelArray)
         then FInConverters[Channel].ic64(PChannelArray,
                PDouble(TAsioAudioDataCollection64(FAudioDataInput).ChannelDataPointerList[Channel]),
                FBufferSize);
        Inc(CurrentBuffer);
       end;
    end;

   // process output
   case FOutBufferPreFill of
    bpfZero : FAudioDataOutput.Clear;
    bpfNoise: FAudioDataOutput.GenerateWhiteNoise(1);
   end;

   // call event to send in and get output data
   FOnBufferSwitch64(Self,
     TAsioAudioDataCollection64(FAudioDataInput),
     TAsioAudioDataCollection64(FAudioDataOutput));

   with TAsioAudioDataCollection64(FAudioDataOutput) do
    begin
     // eventually clip data to avoid ugly artifacts caused by the soundcard
     if FPreventClipping <> pcNone then
      for Channel := 0 to FOutputChannelCount - 1
       do FClipPrevent.cb64(PDouble(ChannelDataPointerList[Channel]) ,FBufferSize);

     // convert float data to soundcard dependent format
     CurrentBuffer := FOutputBuffers;
     for Channel := 0 to FOutputChannelCount - 1 do
      begin
       PChannelArray := CurrentBuffer^[0].buffers[Index];
       if assigned(PChannelArray)
        then FOutConverters[Channel].oc64(PDouble(ChannelDataPointerList[Channel]),
               PChannelArray, FBufferSize);
       inc(CurrentBuffer);
      end;
    end;
  end
 else
  begin
   // 32bit float processing

   // process input
   with TAsioAudioDataCollection32(FAudioDataInput) do
    case FInBufferPreFill of
      bpfZero : FAudioDataInput.Clear;
     bpfNoise : FAudioDataInput.GenerateWhiteNoise(1);
     else
      // convert soundcard dependent format to float data
      for Channel := 0 to FInputChannelCount - 1 do
       begin
        PChannelArray := CurrentBuffer^[0].buffers[Index];
        if Assigned(PChannelArray)
         then FInConverters[Channel].ic32(PChannelArray,
                PSingle(ChannelDataPointer[Channel]),
                FBufferSize);
        Inc(CurrentBuffer);
       end;
    end;

   // process output
   case FOutBufferPreFill of
    bpfZero : FAudioDataOutput.Clear;
    bpfNoise: FAudioDataOutput.GenerateWhiteNoise(1);
   end;

   // call event to send in and get output data
   FOnBufferSwitch32(Self,
     TAsioAudioDataCollection32(FAudioDataInput),
     TAsioAudioDataCollection32(FAudioDataOutput));

   with TAsioAudioDataCollection32(FAudioDataOutput) do
    begin
     // eventually clip data to avoid ugly artifacts caused by the soundcard
     if FPreventClipping <> pcNone then
      for Channel := 0 to FOutputChannelCount - 1
       do FClipPrevent.cb32(PSingle(ChannelDataPointer[Channel]) ,FBufferSize);

     // convert float data to soundcard dependent format
     CurrentBuffer := FOutputBuffers;
     for Channel := 0 to FOutputChannelCount - 1 do
      begin
       PChannelArray := CurrentBuffer^[0].Buffers[Index];
       if assigned(PChannelArray)
        then FOutConverters[Channel].oc32(PSingle(ChannelDataPointer[Channel]),
               PChannelArray, FBufferSize);
       inc(CurrentBuffer);
      end;
    end;

   end;
 FDriver.OutputReady;
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

end.
