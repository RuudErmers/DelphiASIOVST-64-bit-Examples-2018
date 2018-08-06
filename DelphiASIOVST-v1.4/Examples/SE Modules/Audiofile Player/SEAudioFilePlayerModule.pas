unit SEAudioFilePlayerModule;

interface

uses
  {$IFDEF UseEmbedding}Windows, Classes, {$ENDIF} SysUtils, SyncObjs,
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspBufferedAudioFilePlayer,
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  // define some constants to make referencing in/outs clearer
  TSEAudioFilePlayerPins = (pinFileName, pinBufferSize, pinSemitones,
    pinInterpolation, pinReset, pinOutputLeft, pinOutputRight
    {$IFDEF BufferFill}, pinBufferFill{$ENDIF});

  TSEAudioFilePlayerModule = class(TSEModuleBase)
  private
    FOutLeftBuffer   : PDAVSingleFixedArray;
    FOutRightBuffer  : PDAVSingleFixedArray;
    {$IFDEF BufferFill}
    FBufferFillState : PDAVSingleFixedArray;
    {$ENDIF}
    FFileName        : PChar;
    FPosition        : Integer;
    FReset           : Boolean;
    FBufferSize      : Integer;
    FSemitones       : Single;
    FInterpolation   : TBufferInterpolation;
    FCriticalSection : TCriticalSection;
    {$IFDEF UseEmbedding}
    FBufferedPlayer  : TBufferedAudioFileStreamPlayer;
    FResourceStream  : TResourceStream;
    FContainedData   : TStringList;
    procedure LoadFromResource(ID: Integer);
    {$ELSE}
    FBufferedPlayer  : TBufferedAudioFilePlayer;
    {$ENDIF}
  protected
    procedure Open; override;
    procedure Close; override;
    procedure ChooseProcess; virtual;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessBypass(const BufferOffset, SampleFrames: Integer);
  end;

implementation

{$IFDEF UseEmbedding}
function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;
{$ENDIF}

constructor TSEAudioFilePlayerModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FCriticalSection := TCriticalSection.Create;

 {$IFDEF UseEmbedding}
 FBufferedPlayer := TBufferedAudioFileStreamPlayer.Create;
 FContainedData := TStringList.Create;
 EnumResourceNames(HInstance, 'AudioFile', @EnumNamesFunc, LongWord(FContainedData));

 if FContainedData.Count > 0
  then Integer(FFileName) := 0
  else FFileName := '';
 {$ELSE}
 FBufferedPlayer := TBufferedAudioFilePlayer.Create;
 {$ENDIF}
end;

destructor TSEAudioFilePlayerModule.Destroy;
begin
 {$IFDEF UseEmbedding}
 FreeAndNil(FContainedData);
 {$ENDIF}
 FreeAndNil(FCriticalSection);
 if Assigned(FBufferedPlayer)
  then FreeAndNil(FBufferedPlayer);
 inherited;
end;

procedure TSEAudioFilePlayerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 ChooseProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutputLeft)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputRight)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEAudioFilePlayerModule.Close;
begin
 OnProcess := SubProcessBypass;
 inherited;
end;

procedure TSEAudioFilePlayerModule.ChooseProcess;
begin
 if {$IFDEF UseEmbedding}(FContainedData.Count = 0) {$ELSE} (not FileExists(FFileName)) {$ENDIF}
  then OnProcess := SubProcessBypass
  else OnProcess := SubProcess
end;

procedure TSEAudioFilePlayerModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 inherited;
 case TSEAudioFilePlayerPins(CurrentPin.PinID) of
       pinFileName : begin
                      FCriticalSection.Enter;
                      try
                       {$IFDEF UseEmbedding}
                       if FContainedData.Count > 0
                        then LoadFromResource(Integer(FFileName))
                        else
                       {$ENDIF}
                         if FileExists(FFileName) then
                          try
                           FBufferedPlayer.Filename := FFileName;
                           FPosition := 0;
                          except
                          end;

                       ChooseProcess;
                      finally
                       FCriticalSection.Leave;
                      end;
                     end;
     pinBufferSize : begin
                      if FBufferSize < 1024 then FBufferSize := 1024 else
                      if FBufferSize > 65536 then FBufferSize := 65536;
                      FBufferedPlayer.BufferSize := FBufferSize;
                      FBufferedPlayer.BlockSize := FBufferSize div 4;
                     end;
      pinSemitones : begin
                      FBufferedPlayer.Pitch := FSemitones;
                     end;
  pinInterpolation : begin
                      FBufferedPlayer.Interpolation := FInterpolation;
                     end;
          pinReset : if FReset then
                      begin
                       FPosition := 0;
                       FReset := False;
                       if Assigned(FBufferedPlayer)
                        then FBufferedPlayer.Reset;
                       Pin[Integer(pinReset)].TransmitStatusChange(SampleClock, stOneOff);
                      end;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEAudioFilePlayerModule.LoadFromResource(ID: Integer);
begin
 if (ID >= 0) and (ID < FContainedData.Count) then
  begin
   if Assigned(FResourceStream) then FreeAndNil(FResourceStream)
   FResourceStream := TResourceStream.Create(HInstance, FContainedData[ID], 'AudioFile');
   FBufferedPlayer.Stream := FResourceStream;
  end;
end;
{$ENDIF}

// The most important part, processing the audio
procedure TSEAudioFilePlayerModule.SampleRateChanged;
begin
 inherited;
 FBufferedPlayer.SampleRate := SampleRate;
end;

procedure TSEAudioFilePlayerModule.SubProcess(const BufferOffset, SampleFrames: Integer);
{$IFDEF BufferFill}
var
  BufferFill : Single;
  Sample     : Integer;
{$ENDIF}
begin
 FCriticalSection.Enter;
 try
  FBufferedPlayer.GetSamples(@FOutLeftBuffer^[BufferOffset], @FOutRightBuffer^[BufferOffset], SampleFrames)
 finally
  FCriticalSection.Leave;
 end;
 {$IFDEF BufferFill}
 BufferFill := 0.01 * FBufferedPlayer.BufferFill;
 for Sample := 0 to SampleFrames - 1 do
  begin
   FBufferFillState^[BufferOffset] := 10 * BufferFill;
  end;
 {$ENDIF}
end;

procedure TSEAudioFilePlayerModule.SubProcessBypass(
  const BufferOffset, SampleFrames: Integer);
begin
 FillChar(FOutLeftBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 FillChar(FOutRightBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 {$IFDEF BufferFill}
 FillChar(FBufferFillState[BufferOffset], SampleFrames * SizeOf(Single), 0);
 {$ENDIF}
end;

// describe your module
class procedure TSEAudioFilePlayerModule.GetModuleProperties(Properties : PSEModuleProperties);
{$IFDEF UseEmbedding}
var
  ContainedData : TStringList;
  i             : Integer;
  str           : string;
{$ENDIF}
begin
 {$IFDEF UseEmbedding}
 ContainedData := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'AudioFile', @EnumNamesFunc, LongWord(ContainedData));
  {$ENDIF}
  with Properties^ do
   begin
    {$IFDEF UseEmbedding}
    if ContainedData.Count > 0 then
     begin
      Name := 'Embedded AudioFile Player';
      str  := 'DAV ESAudioFile';
      for i := 0 to ContainedData.Count - 1
       do str := str + ContainedData[i];
      ID := PAnsiChar(str);
     end
    else
    {$ENDIF}
     begin
      Name := 'AudioFile Player';
      ID := 'DAV AudioFile Player';
     end;

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';
    SDKVersion := CSeSdkVersion;
   end;
 {$IFDEF UseEmbedding}
 finally
  FreeAndNil(ContainedData);
 end;
 {$ENDIF}
end;

// describe the pins (plugs)
function TSEAudioFilePlayerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
{$IFDEF UseEmbedding}
var
  str : AnsiString;
{$ENDIF}
begin
 Result := True;
 case TSEAudioFilePlayerPins(index) of
       pinFileName : with Properties^ do
                      {$IFDEF UseEmbedding}
                      if FContainedData.Count > 0 then
                       begin
                        Name            := 'AudioFile ID';
                        VariableAddress := @FFileName;
                        Direction       := drIn;
                        DataType        := dtEnum;
                        DefaultValue    := '0';
                        str             := 'range 0,' + IntToStr(FContainedData.Count - 1);
                        DatatypeExtra   := PAnsiChar(str);
                       end
                      else
                      {$ENDIF}
                       begin
                        Name            := 'FileName';
                        VariableAddress := @FFileName;
                        Flags           := [iofFilename];
                        Direction       := drIn;
                        DataType        := dtText;
                       end;
     pinBufferSize : with Properties^ do
                      begin
                       Name            := 'BufferSize';
                       VariableAddress := @FBufferSize;
                       Direction       := drIn;
                       Datatype        := dtInteger;
//                       DatatypeExtra   := 'range 1024,65536';
                       DefaultValue    := '16384';
                      end;
      pinSemitones : with Properties^ do
                      begin
                       Name            := 'Semitones';
                       VariableAddress := @FSemitones;
                       Direction       := drIn;
                       Datatype        := dtSingle;
                      end;
  pinInterpolation : with Properties^ do
                      begin
                       Name            := 'Interpolation';
                       VariableAddress := @FInterpolation;
                       Direction       := drIn;
                       Datatype        := dtEnum;
                       DatatypeExtra   := 'none, linear, hermite, bspline';
                       DefaultValue    := 'none';
                      end;
          pinReset : with Properties^ do
                      begin
                       Name            := 'Reset';
                       VariableAddress := @FReset;
                       Direction       := drIn;
                       Datatype        := dtBoolean;
                      end;
     pinOutputLeft : with Properties^ do
                      begin
                       Name            := 'Left';
                       VariableAddress := @FOutLeftBuffer;
                       Direction       := drOut;
                       Datatype        := dtFSample;
                      end;
    pinOutputRight : with Properties^ do
                      begin
                       Name            := 'Right';
                       VariableAddress := @FOutRightBuffer;
                       Direction       := drOut;
                       Datatype        := dtFSample;
                      end;
    {$IFDEF BufferFill}
    pinBufferFill : with Properties^ do
                      begin
                       Name            := 'Buffer State';
                       VariableAddress := @FBufferFillState;
                       Direction       := drOut;
                       Datatype        := dtFSample;
                      end;
    {$ENDIF}
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
