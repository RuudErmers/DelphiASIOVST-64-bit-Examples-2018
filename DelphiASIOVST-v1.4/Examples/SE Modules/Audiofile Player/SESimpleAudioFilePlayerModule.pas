unit SESimpleAudioFilePlayerModule;

interface

uses
  {$IFDEF UseEmbedding}Windows, Classes, {$ENDIF} SysUtils, SyncObjs,
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspBufferedAudioFilePlayer,
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  // define some constants to make referencing in/outs clearer
  TSEAudioFilePlayerPins = (pinFileName, pinBufferSize, pinReset,
    pinOutputLeft, pinOutputRight);

  TSESimpleAudioFilePlayerModule = class(TSEModuleBase)
  private
    FOutLeftBuffer   : PDAVSingleFixedArray;
    FOutRightBuffer  : PDAVSingleFixedArray;
    FFileName        : PChar;
    FPosition        : Integer;
    FReset           : Boolean;
    FBufferSize      : Integer;
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

constructor TSESimpleAudioFilePlayerModule.Create(
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

destructor TSESimpleAudioFilePlayerModule.Destroy;
begin
 {$IFDEF UseEmbedding}
 FreeAndNil(FContainedData);
 {$ENDIF}
 FreeAndNil(FCriticalSection);
 if Assigned(FBufferedPlayer)
  then FreeAndNil(FBufferedPlayer);
 inherited;
end;

procedure TSESimpleAudioFilePlayerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 ChooseProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutputLeft)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputRight)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSESimpleAudioFilePlayerModule.Close;
begin
 OnProcess := SubProcessBypass;
 inherited;
end;

procedure TSESimpleAudioFilePlayerModule.ChooseProcess;
begin
 if {$IFDEF UseEmbedding}(FContainedData.Count = 0) {$ELSE} (not FileExists(FFileName)) {$ENDIF}
  then OnProcess := SubProcessBypass
  else OnProcess := SubProcess
end;

procedure TSESimpleAudioFilePlayerModule.PlugStateChange(
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
       pinReset : if FReset then
                   begin
                    FPosition := 0;
                    FReset := False;
                    if Assigned(FBufferedPlayer)
                     then FBufferedPlayer.Reset;
                    Pin[Integer(pinReset)].TransmitStatusChange(SampleClock, stOneOff);
                   end;
  pinBufferSize : begin
                   if FBufferSize < 1024 then FBufferSize := 1024 else
                   if FBufferSize > 65536 then FBufferSize := 65536;
                   FBufferedPlayer.BufferSize := FBufferSize;
                   FBufferedPlayer.BlockSize := FBufferSize div 4;
                  end;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSESimpleAudioFilePlayerModule.LoadFromResource(ID: Integer);
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
procedure TSESimpleAudioFilePlayerModule.SampleRateChanged;
begin
 inherited;
 FBufferedPlayer.SampleRate := SampleRate;
end;

procedure TSESimpleAudioFilePlayerModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 FCriticalSection.Enter;
 try
  FBufferedPlayer.GetSamples(@FOutLeftBuffer^[BufferOffset], @FOutRightBuffer^[BufferOffset], SampleFrames)
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSESimpleAudioFilePlayerModule.SubProcessBypass(
  const BufferOffset, SampleFrames: Integer);
begin
 FillChar(FOutLeftBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 FillChar(FOutRightBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
end;

// describe your module
class procedure TSESimpleAudioFilePlayerModule.GetModuleProperties(Properties : PSEModuleProperties);
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
      Name := 'Embedded Simple AudioFile Player';
      str  := 'DAV ESAudioFile';
      for i := 0 to ContainedData.Count - 1
       do str := str + ContainedData[i];
      ID := PAnsiChar(str);
     end
    else
    {$ENDIF}
     begin
      Name := 'Simple AudioFile Player';
      ID := 'DAV Simple AudioFile Player';
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
function TSESimpleAudioFilePlayerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
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
          pinReset : with Properties^ do
                      begin
                       Name            := 'Reset';
                       VariableAddress := @FReset;
                       Direction       := drIn;
                       Datatype        := dtBoolean;
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
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
