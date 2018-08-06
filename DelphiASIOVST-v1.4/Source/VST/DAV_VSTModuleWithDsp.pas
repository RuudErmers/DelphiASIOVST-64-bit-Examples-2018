unit DAV_VSTModuleWithDsp;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_VSTEffect, DAV_VSTModuleWithPrograms,
  DAV_ProcessingComponent, DAV_VSTCustomModule;

type
  TProcessingMode = (pmNormal, pmBlockSave, pmCopy, pmMute, pmDspQueue);

  TDspVSTModule = class(TVSTModuleWithPrograms)
  private
    procedure ProcessingModeChanged;
  protected
    FBlockModeSize        : Cardinal;
    FBlockModeOverlap     : Cardinal;
    FProcessingMode       : TProcessingMode;
    FBlockPosition        : Cardinal;
    FDspQueueList         : TDAVProcessingComponentList;
    FBlockInBuffer32      : TDAVArrayOfSingleFixedArray;
    FBlockOutBuffer32     : TDAVArrayOfSingleFixedArray;
    FBlockInBuffer64      : TDAVArrayOfDoubleFixedArray;
    FBlockOutBuffer64     : TDAVArrayOfDoubleFixedArray;
    FOnProcess            : TProcessAudio32Event;
    FOnProcess32Replacing : TProcessAudio32Event;
    FOnProcess64Replacing : TProcessAudio64Event;
    FDspDirectProcessItem : TDAVProcessingComponent;

    function IOChanged: Boolean; override;
    procedure SampleRateChanged; override;

    procedure ProcessChanged; virtual;
    procedure Process32ReplacingChanged; virtual;
    procedure Process64ReplacingChanged; virtual;

    procedure DoProcessCopy(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal); overload;
    procedure DoProcessCopy(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal); overload;
    procedure DoProcessMute(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal); overload;
    procedure DoProcessMute(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal); overload;
    procedure DoBlockSaveProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal); overload;
    procedure DoBlockSaveProcess(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal); overload;
    procedure DoBlockSaveProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal); overload;
    procedure DoBlockSaveProcess32Replacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal); overload;
    procedure DoProcessDspQueue(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal); overload;
    procedure DoProcessDspQueue(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal); overload;

    procedure ProcessMidiEvent(const MidiEvent: TVstMidiEvent); override;

    procedure NumInputsChanged; override;
    procedure NumOutputsChanged; override;
    procedure InitialDelayChanged; override;

    procedure SetOnProcess(Value : TProcessAudio32Event);
    procedure SetOnProcess32Replacing(Value : TProcessAudio32Event);
    procedure SetOnProcess64Replacing(Value : TProcessAudio64Event);
    procedure SetProcessingMode(Value : TProcessingMode);
    procedure PrepareBlockProcessing; virtual;
    procedure SetBlockForcedSize(Value: Cardinal); virtual;
    procedure SetBlockOverlapSize(Value: Cardinal); virtual;
    procedure SetDspDirectProcessItem(v: TDAVProcessingComponent); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterDSPItem(item: TDAVProcessingComponent);
    procedure UnRegisterDSPItem(item: TDAVProcessingComponent);

    property BlockModeSize: Cardinal read FBlockModeSize write SetBlockForcedSize default 1024;
    property BlockModeOverlap: Cardinal read FBlockModeOverlap write SetBlockOverlapSize default 0;
    property ProcessingMode: TProcessingMode read FProcessingmode write SetProcessingMode default pmNormal;

    property OnProcess: TProcessAudio32Event read FOnProcess write SetOnProcess;
    property OnProcess32Replacing: TProcessAudio32Event read FOnProcess32Replacing write SetOnProcess32Replacing;
    property OnProcess64Replacing: TProcessAudio64Event read FOnProcess64Replacing write SetOnProcess64Replacing;
  published
    property DspDirectProcessItem: TDAVProcessingComponent read FDspDirectProcessItem write SetDspDirectProcessItem default nil;
  end;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, SysUtils;

constructor TDspVSTModule.Create(AOwner: TComponent);
begin
  inherited; 

  SetLength(FBlockInBuffer32, 0);
  SetLength(FBlockOutBuffer32, 0);
  SetLength(FBlockInBuffer64, 0);
  SetLength(FBlockOutBuffer64, 0);

  FProcessingmode := pmNormal;
  FBlockModeSize := 1024;  
  FBlockModeOverlap := 0;
  FDspDirectProcessItem := nil;
  FDspQueueList := TDAVProcessingComponentList.Create;
  {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.Create'); {$ENDIF}
end;

destructor TDspVSTModule.Destroy;
begin
  if Assigned(FDspQueueList) then FreeAndNil(FDspQueueList);
  inherited;
end;

procedure TDspVSTModule.RegisterDSPItem(item: TDAVProcessingComponent);
begin
 with FDspQueueList do
  begin
   if IndexOf(item) < 0 then
    begin
     Add(item);
     if (FProcessingmode = pmDspQueue) and not Assigned(FDspDirectProcessItem)
      then FDspDirectProcessItem := item;
    end;
   Item.SampleRate := FSampleRate;
   Item.Channels := max(FEffect.numInputs, FEffect.numOutputs);
  end;
end;

procedure TDspVSTModule.UnRegisterDSPItem(item: TDAVProcessingComponent);
begin
 with FDspQueueList do
  if IndexOf(item) >= 0
   then Remove(item);

 if FDspDirectProcessItem = item then
  if FDspQueueList.Count > 0
   then FDspDirectProcessItem := FDspQueueList.Items[0]
   else FDspDirectProcessItem := nil;
end;

function TDspVSTModule.IOChanged: Boolean;
begin
  Result := inherited IOChanged;
  FDspQueueList.SetChannels(max(FEffect.numInputs, FEffect.numOutputs));
end;

procedure TDspVSTModule.SampleRateChanged;
begin
  FDspQueueList.SetSampleRate(fSampleRate);
  inherited;
end;

procedure TDspVSTModule.NumInputsChanged;
begin
 inherited;
 PrepareBlockProcessing;
end;

procedure TDspVSTModule.NumOutputsChanged;
begin
 inherited;
 PrepareBlockProcessing;
end;

procedure TDspVSTModule.InitialDelayChanged;
begin
 if (FProcessingmode = pmBlockSave) and
    (FInitialDelay < Integer(FBlockModeSize - FBlockModeOverlap))
  then FEffect.initialDelay := FBlockModeSize - FBlockModeOverlap
  else FEffect.initialDelay := FInitialDelay;

 if HostProduct <> 'energyXT' then IOChanged;
end;

procedure TDspVSTModule.SetBlockForcedSize(Value: Cardinal);
begin
 if Value > 0 then FBlockModeSize := Value;

 FBlockPosition := FBlockModeOverlap;
 PrepareBlockProcessing;
end;

procedure TDspVSTModule.SetBlockOverlapSize(Value: Cardinal);
begin
 if Value < FBlockModeSize then FBlockModeOverlap := Value;

 if (FProcessingmode = pmBlockSave) and
    (FEffect.InitialDelay < Integer(FBlockModeSize - FBlockModeOverlap))
  then InitialDelayChanged;
end;

procedure TDspVSTModule.ProcessMidiEvent(const MidiEvent: TVstMidiEvent);
var
  tmp    : TDAVMidiEvent;
  Filter : Boolean;
begin
 with MidiEvent do
  begin
   tmp.MidiData[0]     := MidiData[0];
   tmp.MidiData[1]     := MidiData[1];
   tmp.MidiData[2]     := MidiData[2];
   tmp.DeltaFrames     := DeltaFrames;
   tmp.NoteOffset      := NoteOffset;
   tmp.NoteLength      := NoteLength;
   tmp.Detune          := Detune;
   tmp.NoteOffVelocity := NoteOffVelocity;
  end;

 Filter := False;
 FDspQueueList.ProcessMidiEventQueue(tmp, Filter);

 if not Filter then inherited;
end;


procedure TDspVSTModule.DoProcessCopy(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoProcessCopy'); {$ENDIF}
 for Channel := 0 to min(FEffect.numInputs, FEffect.numOutputs) - 1
  do Move(Inputs[Channel, 0], PSingle(@Outputs[Channel, 0])^, SampleFrames * SizeOf(Single));
end;

procedure TDspVSTModule.DoProcessCopy(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoProcessCopy'); {$ENDIF}
 for Channel := 0 to min(FEffect.numInputs, FEffect.numOutputs) - 1
  do Move(Inputs[Channel, 0], PDouble(@Outputs[Channel, 0])^, SampleFrames * SizeOf(Double));
end;

procedure TDspVSTModule.DoProcessMute(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoProcessMute'); {$ENDIF}
 for Channel := 0 to FEffect.numOutputs - 1
  do FillChar(PSingle(@Outputs[Channel, 0])^, SampleFrames * SizeOf(Single), 0);
end;

procedure TDspVSTModule.DoProcessMute(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoProcessMute'); {$ENDIF}
 for Channel := 0 to FEffect.numOutputs - 1
  do FillChar(PDouble(@Outputs[Channel, 0])^, SampleFrames * SizeOf(Single), 0);
end;

procedure TDspVSTModule.DoBlockSaveProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  CurrentPosition : Cardinal;
  Channel         : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoBlockSaveProcess'); {$ENDIF}
 CurrentPosition := 0;
 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockModeSize then
   begin
    for Channel := 0 to numInputs  - 1 do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer32[Channel, FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer32[Channel, FBlockPosition], PSingle(@Outputs[Channel, CurrentPosition])^, (SampleFrames - CurrentPosition) * SizeOf(Single));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    Exit;
   end
  else
   begin
    for Channel := 0 to numInputs - 1  do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer32[Channel, FBlockPosition], (FBlockModeSize - FBlockPosition) * SizeOf(Single));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer32[Channel, FBlockPosition], PSingle(@Outputs[Channel, CurrentPosition])^, (FBlockModeSize - FBlockPosition) * SizeOf(Single));

    FOnProcess(FBlockInBuffer32, FBlockOutBuffer32, FBlockModeSize);

    for Channel := 0 to numInputs - 1  do Move(FBlockInBuffer32[Channel, (FBlockModeSize - FBlockModeOverlap)], FBlockInBuffer32[Channel, 0], FBlockModeOverlap * SizeOf(Single));

    CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
    FBlockPosition := FBlockModeOverlap;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcess(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  CurrentPosition : Cardinal;
  Channel         : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoBlockSaveProcess'); {$ENDIF}
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockModeSize then
   begin
    for Channel := 0 to numInputs - 1  do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer64[Channel, FBlockPosition], (SampleFrames-CurrentPosition) * SizeOf(Double));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer64[Channel, FBlockPosition], PDouble(@Outputs[Channel, CurrentPosition])^, (SampleFrames-CurrentPosition) * SizeOf(Double));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    CurrentPosition := SampleFrames;
   end
  else
   begin
    for Channel := 0 to numInputs-1  do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer64[Channel, FBlockPosition],(FBlockModeSize-FBlockPosition) * SizeOf(Double));
    for Channel := 0 to numOutputs-1 do Move(FBlockOutBuffer64[Channel, FBlockPosition], PDouble(@Outputs[Channel, CurrentPosition])^, (FBlockModeSize-FBlockPosition) * SizeOf(Double));

    FOnProcess64Replacing(FBlockInBuffer64, FBlockOutBuffer64, FBlockModeSize);

    for Channel := 0 to numInputs - 1  do Move(FBlockInBuffer64[Channel, (FBlockModeSize - FBlockModeOverlap)], FBlockInBuffer64[Channel, 0], FBlockModeOverlap * SizeOf(Double));

    CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
    FBlockPosition := FBlockModeOverlap;
   end;
 until CurrentPosition >= SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  CurrentPosition : Cardinal;
  Channel         : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoBlockSaveProcess32Replacing'); {$ENDIF}
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockModeSize then
   begin
    for Channel := 0 to numInputs  - 1 do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer32[Channel, FBlockPosition],(SampleFrames-CurrentPosition)*SizeOf(Single));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer32[Channel, FBlockPosition], PSingle(@Outputs[Channel, CurrentPosition])^,(SampleFrames-CurrentPosition)*SizeOf(Single));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    CurrentPosition := SampleFrames;
   end
  else
   begin
    for Channel := 0 to numInputs  - 1 do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer32[Channel, FBlockPosition], (FBlockModeSize - FBlockPosition) * SizeOf(Single));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer32[Channel, FBlockPosition], PSingle(@Outputs[Channel, CurrentPosition])^, (FBlockModeSize - FBlockPosition) * SizeOf(Single));

    FOnProcess32Replacing(FBlockInBuffer32, FBlockOutBuffer32, FBlockModeSize);

    for Channel := 0 to numInputs - 1
     do Move(FBlockInBuffer32[Channel, (FBlockModeSize - FBlockModeOverlap)], FBlockInBuffer32[Channel, 0], FBlockModeOverlap * SizeOf(Single));

    CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
    FBlockPosition := FBlockModeOverlap;
   end;
 until CurrentPosition >= SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcess32Replacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  CurrentPosition : Cardinal;
  Channel         : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoBlockSaveProcess32Replacing'); {$ENDIF}
 CurrentPosition := 0;
 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockModeSize then
   begin
    for Channel := 0 to numInputs  - 1  do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer64[Channel, FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer64[Channel, FBlockPosition], PDouble(@Outputs[Channel, CurrentPosition])^, (SampleFrames - CurrentPosition) * SizeOf(Double));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    CurrentPosition := SampleFrames;
   end
  else
   begin
    for Channel := 0 to numInputs  - 1 do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer64[Channel, FBlockPosition],(FBlockModeSize - FBlockPosition) * SizeOf(Double));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer64[Channel, FBlockPosition], PDouble(@Outputs[Channel, CurrentPosition])^, (FBlockModeSize - FBlockPosition) * SizeOf(Double));

    FOnProcess64Replacing(FBlockInBuffer64, FBlockOutBuffer64, FBlockModeSize);

    for Channel := 0 to numInputs - 1 do Move(FBlockInBuffer64[Channel, (FBlockModeSize - FBlockModeOverlap)], FBlockInBuffer64[Channel, 0], FBlockModeOverlap * SizeOf(Double));

    CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
    FBlockPosition := FBlockModeOverlap;
   end;
 until CurrentPosition >= SampleFrames;
end;

procedure TDspVSTModule.DoProcessDspQueue(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ProcessBuffer : TDAVArrayOfSingleFixedArray;
  Channel       : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoProcessDspQueue'); {$ENDIF}
 SetLength(ProcessBuffer, Max(numOutputs, numInputs) * SizeOf(PDAVSingleFixedArray));
 for Channel := 0 to Max(numOutputs, numInputs) - 1 do
  begin
   GetMem(ProcessBuffer[Channel], SampleFrames * SizeOf(Single));
   FillChar(ProcessBuffer[Channel]^, SampleFrames * SizeOf(Single), 0);
  end;
 try
  for Channel := 0 to numInputs - 1
   do Move(Inputs[Channel, 0], ProcessBuffer[Channel, 0], SampleFrames * SizeOf(Single));

(*
   if Assigned(FDspDirectProcessItem) then
    FDspDirectProcessItem.ProcessQueueSAA(ProcessBuffer, SampleFrames);
*)

  for Channel := 0 to numOutputs - 1
   do Move(ProcessBuffer[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single));
 finally
  for Channel := 0 to Max(numOutputs, numInputs) - 1
   do FreeMem(ProcessBuffer[Channel]);
 end;
end;

procedure TDspVSTModule.DoProcessDspQueue(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  ProcessBuffer : TDAVArrayOfDoubleFixedArray;
  Channel       : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('TDspVSTModule.DoProcessDspQueue'); {$ENDIF}
 SetLength(ProcessBuffer, Max(numOutputs, numInputs) * SizeOf(PDAVDoubleFixedArray));
 for Channel := 0 to Max(numOutputs, numInputs) - 1 do
  begin
   GetMem(ProcessBuffer[Channel], SampleFrames * SizeOf(Double));
   FillChar(ProcessBuffer[Channel]^, SampleFrames * SizeOf(Double), 0);
  end;
 try
  for Channel := 0 to numInputs - 1
   do Move(Inputs[Channel, 0], ProcessBuffer[Channel, 0], SampleFrames * SizeOf(Double));

(*
   if Assigned(FDspDirectProcessItem) then
    FDspDirectProcessItem.ProcessQueueDAA(ProcessBuffer, SampleFrames);
*)

  for Channel := 0 to numOutputs - 1
   do Move(ProcessBuffer[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Double));
 finally
  for Channel := 0 to Max(numOutputs, numInputs) - 1
   do FreeMem(ProcessBuffer[Channel]);
 end;
end;

procedure TDspVSTModule.PrepareBlockProcessing;
var
  ChannelIndex : Integer;
begin
 if FProcessingmode = pmBlockSave then
  begin
   SetLength(FBlockInBuffer32,  numInputs);
   SetLength(FBlockOutBuffer32, numOutputs);
   SetLength(FBlockInBuffer64,  numInputs);
   SetLength(FBlockOutBuffer64, numOutputs);

   for ChannelIndex := 0 to numInputs - 1 do
    begin
     GetMem(FBlockInBuffer32[ChannelIndex], FBlockModeSize * SizeOf(Single));
     GetMem(FBlockInBuffer64[ChannelIndex], FBlockModeSize * SizeOf(Double));
     FillChar(FBlockInBuffer32[ChannelIndex]^, FBlockModeSize * SizeOf(Single), 0);
     FillChar(FBlockInBuffer64[ChannelIndex]^, FBlockModeSize * SizeOf(Double), 0);
    end;
   for ChannelIndex := 0 to numOutputs - 1 do
    begin
     GetMem(FBlockOutBuffer32[ChannelIndex], FBlockModeSize * SizeOf(Single));
     GetMem(FBlockOutBuffer64[ChannelIndex], FBlockModeSize * SizeOf(Double));
     FillChar(FBlockOutBuffer32[ChannelIndex]^, FBlockModeSize * SizeOf(Single), 0);
     FillChar(FBlockOutBuffer64[ChannelIndex]^, FBlockModeSize * SizeOf(Double), 0);
    end;

   FBlockPosition := FBlockModeOverlap;
   if (FProcessingmode = pmBlockSave) and
      (FEffect.InitialDelay < Integer(FBlockModeSize - FBlockModeOverlap))
    then InitialDelayChanged;
  end
 else
  begin
   Assert(Length(FBlockInBuffer32) = Length(FBlockInBuffer64));
   for ChannelIndex := 0 to Length(FBlockInBuffer32) - 1 do
    begin
     FreeMem(FBlockInBuffer32[ChannelIndex]);
     FreeMem(FBlockInBuffer64[ChannelIndex]);
    end;
   Assert(Length(FBlockOutBuffer32) = Length(FBlockOutBuffer64));
   for ChannelIndex := 0 to Length(FBlockOutBuffer32) - 1 do
    begin
     FreeMem(FBlockOutBuffer32[ChannelIndex]);
     FreeMem(FBlockOutBuffer64[ChannelIndex]);
    end;

   SetLength(FBlockInBuffer32, 0);
   SetLength(FBlockOutBuffer32, 0);
   SetLength(FBlockInBuffer64, 0);
   SetLength(FBlockOutBuffer64, 0);
  end;
end;

procedure TDspVSTModule.SetOnProcess(Value : TProcessAudio32Event);
begin
 if @FOnProcess <> @Value then
  begin
   FOnProcess := Value;
   ProcessChanged;
  end;
end;

procedure TDspVSTModule.SetOnProcess32Replacing(Value : TProcessAudio32Event);
begin
 if @FOnProcess32Replacing <> @Value then
  begin
   FOnProcess32Replacing := Value;
   Process32ReplacingChanged;
  end;
end;

procedure TDspVSTModule.SetOnProcess64Replacing(Value : TProcessAudio64Event);
begin
 if @FOnProcess64Replacing <> @Value then
  begin
   FOnProcess64Replacing := Value;
   Process64ReplacingChanged;
  end;
end;

procedure TDspVSTModule.SetProcessingMode(Value : TProcessingMode);
begin
 if Value <> FProcessingMode then
  begin
   FProcessingMode := Value;
   ProcessingModeChanged;
  end;
end;

procedure TDspVSTModule.ProcessChanged;
begin
 case FProcessingMode of
   pmNormal:     FOnProcessEx := FOnProcess;
   pmBlockSave:  if Assigned(FOnProcess)
                   then FOnProcessEx := DoBlockSaveProcess
                   else FOnProcessEx := FOnProcess;
   pmCopy:       FOnProcessEx := DoProcessCopy;
   pmMute:       FOnProcessEx := DoProcessMute;
   pmDspQueue:   FOnProcessEx := DoProcessDspQueue;
 end;
end;

procedure TDspVSTModule.Process32ReplacingChanged;
begin
 case FProcessingMode of
   pmNormal:    FOnProcess32ReplacingEx := FOnProcess32Replacing;
   pmBlockSave: if Assigned(FOnProcess32Replacing)
                  then FOnProcess32ReplacingEx := DoBlockSaveProcess32Replacing
                  else FOnProcess32ReplacingEx := FOnProcess32Replacing;
   pmCopy:      FOnProcess32ReplacingEx := DoProcessCopy;
   pmMute:      FOnProcess32ReplacingEx := DoProcessMute;
   pmDspQueue:  FOnProcess32ReplacingEx := DoProcessDspQueue;
 end;
end;

procedure TDspVSTModule.Process64ReplacingChanged;
begin
 case FProcessingMode of
   pmNormal:    FOnProcess64ReplacingEx := FOnProcess64Replacing;
   pmBlockSave: if Assigned(FOnProcess64Replacing)
                  then FOnProcess64ReplacingEx := DoBlockSaveProcess32Replacing
                  else FOnProcess64ReplacingEx := FOnProcess64Replacing;
   pmCopy:      FOnProcess64ReplacingEx := DoProcessCopy;
   pmMute:      FOnProcess64ReplacingEx := DoProcessMute;
   pmDspQueue:  FOnProcess64ReplacingEx := DoProcessDspQueue;
 end;
end;

procedure TDspVSTModule.ProcessingModeChanged;
begin
 case FProcessingMode of
    pmNormal:
       begin
         FOnProcessEx := FOnProcess;
         FOnProcess32ReplacingEx := FOnProcess32Replacing;
         FOnProcess64ReplacingEx := FOnProcess64Replacing;
       end;
    pmBlockSave:
       begin
         if Assigned(FOnProcess)
          then FOnProcessEx := DoBlockSaveProcess
          else FOnProcessEx := FOnProcess;

         if Assigned(FOnProcess32Replacing)
          then FOnProcess32ReplacingEx := DoBlockSaveProcess32Replacing
          else FOnProcess32ReplacingEx := FOnProcess32Replacing;

         if Assigned(FOnProcess64Replacing)
          then FOnProcess64ReplacingEx := DoBlockSaveProcess32Replacing
          else FOnProcess64ReplacingEx := FOnProcess64Replacing;

         PrepareBlockProcessing;
       end;
    pmCopy:
       begin
         FOnProcessEx := DoProcessCopy;
         FOnProcess32ReplacingEx := DoProcessCopy;
         FOnProcess64ReplacingEx := DoProcessCopy;
       end;
    pmMute:
       begin
         FOnProcessEx := DoProcessMute;
         FOnProcess32ReplacingEx := DoProcessMute;
         FOnProcess64ReplacingEx := DoProcessMute;
       end;
    pmDspQueue:
      begin
        FOnProcessEx := DoProcessDspQueue;
        FOnProcess32ReplacingEx := DoProcessDspQueue;
        FOnProcess64ReplacingEx := DoProcessDspQueue;
      end;
 end;
end;

procedure TDspVSTModule.SetDspDirectProcessItem(v: TDAVProcessingComponent);
begin
  if v <> FDspDirectProcessItem then
  begin
    if v = nil then
      FDspDirectProcessItem := v
    else if FDspQueueList.IndexOf(v) >= 0 then
    begin
      FDspDirectProcessItem := v;
      SetProcessingMode(pmDspQueue);
    end else
      raise Exception.Create('DspDirectProcessItem has to be the first item of a queue');
  end;
end;

end.
