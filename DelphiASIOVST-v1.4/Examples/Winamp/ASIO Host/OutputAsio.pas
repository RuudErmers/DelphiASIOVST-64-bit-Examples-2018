unit OutputAsio;

interface

uses
  Windows, Messages, Classes, Controls, Forms, Dialogs, StdCtrls, DAV_Types,
  DAV_ASIOHost;

type
  TSmallIntArray = array [0..40000] of Smallint;
  PSmallIntArray = ^TSmallIntArray;

  TFmASIOConfig = class(TForm)
    ASIOHost: TASIOHost;
    LbDriver: TLabel;
    CBDriver: TComboBox;
    BtControlPanel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CBDriverChange(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ASIOHostLatencyChanged(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
  private
    FVolume : Single;
    FPan    : Single;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    AB         : array of PDAVSingleFixedArray;
    ABSize     : Integer;
    RP, WP     : Integer;
    GPBSN      : Single;
    GlobalPos  : Integer;
    PauseState : Boolean;
    NCh, Bps   : Integer;
    function CanPlay: Integer;
    property Volume: Single read FVolume write FVolume;
    property Pan: Single read FPan write FPan;
  end;

{ Output plugin functions }
procedure Config(HwndParent: HWND); cdecl;
procedure About(HwndParent: HWND); cdecl;
procedure Init; cdecl;
procedure Quit; cdecl;
function Open(Samplerate, NumChannels, BitsPerSample, BufferLen_ms,
  PreBuffer_ms: Integer): Integer; cdecl;
procedure Close; cdecl;
function Write(Buffer: Pointer; Length: Integer): Integer; cdecl;
function CanWrite: Integer; cdecl;
function IsPlaying: Integer; cdecl;
function Pause(IsPause: Integer): integer; cdecl;
procedure SetVolume(Volume: Integer); cdecl;
procedure SetPan(Pan: Integer); cdecl;
procedure Flush(t: Integer); cdecl;
function GetOutputTime: Integer; cdecl;

type
  POut_Module = ^TOut_Module;

  TOut_Module = record
    Version      : Integer; // module version (OUT_VER)
    Description  : PChar;   // description of module, with version string
    Id           : Integer; // module id. each input module gets its own [non-nullsoft modules should be >= 65536]
    hMainWindow  : HWND;    // winamp's main window (filled in by winamp)
    hDllInstance : HINST;   // DLL instance handle (filled in by winamp)

    Config: procedure(hwndParent: HWND); cdecl; // configuration dialog
    About: procedure(hwndParent: HWND); cdecl;  // about dialog

    Init: procedure; cdecl; // called when loaded
    Quit: procedure; cdecl; // called when unloaded


    { following functions and procedures are called by input plug-in
      you do not need to use these directly. }

    Open: function(Samplerate, NumChannels, BitsPerSample,
        BufferLen_ms, PreBuffer_ms: Integer): Integer;
        cdecl;
    { returns >=0 on success, <0 on failure
      NOTENOTENOTE: BufferLen_ms and PreBuffer_ms are ignored in most
      if not all output plug-ins.
         ... so don't expect the max latency returned to be what you asked for.
      returns max latency in ms (0 for diskwriters, etc)
      BufferLen_ms and PreBuffer_ms must be in ms. 0 to use defaults.
      PreBuffer_ms must be <= BufferLen_ms }

    Close: procedure; cdecl;
          { close the ol' output device. }

    Write: function(Buffer: Pointer; Length: Integer): Integer; cdecl;
                    { 0 on success. Length == bytes to write (<= 8192 always).
                                                     Buffer is straight audio data.
                      1 returns not able to write (yet). Non-blocking, always. }

    CanWrite: function: Integer; cdecl;
                    { returns number of bytes possible to write at a given time.
                     Never will decrease unless you call Write (or Close, heh) }

    IsPlaying: function: Integer; cdecl;
     { non 0 if output is still going or if data in buffers waiting to be
     written (i.e. closing while IsPlaying() returns 1 would truncate the song }

    Pause: function(pause: Integer): Integer; cdecl;
                                                { returns previous pause state }

    SetVolume: procedure(volume: Integer); cdecl;
      { volume is 0-255 }
    SetPan: procedure(pan: Integer); cdecl;
      { pan is -128 to 128 }

    Flush: procedure(t: Integer); cdecl;
                         { flushes buffers and restarts output at time t (in ms)
                           (used for seeking) }

    GetOutputTime: function: Integer;
        cdecl;     { returns played time in MS }
    GetWrittenTime: function: Integer; cdecl;
                 { returns time written in MS (used for synching up vis stuff) }
  end;

var
  outmod: TOut_Module;
  ASIOOutput: TFmASIOConfig;

implementation

{$R *.dfm}

uses
  Math, SysUtils, Registry, DAV_Common;

{ WinAmp Interface }

procedure Config(hwndParent: hwnd); cdecl; // configuration dialog
begin
 ASIOOutput.Show;
end;

procedure About(hwndParent: hwnd); cdecl;  // about dialog
begin
 ASIOOutput.Show;
end;

procedure Init; cdecl; // called when loaded
begin
 ASIOOutput := TFmASIOConfig.Create(Application);
end;

procedure Quit; cdecl; // called when unloaded
begin
 with TRegistry.Create do
  try
   if OpenKey('Software\WinAmp\ASIO Output', True) then
    begin
     WriteBool('Visible', ASIOOutput.Visible);
     WriteInteger('Left', ASIOOutput.Left);
     WriteInteger('Top', ASIOOutput.Top);
    end;
  finally
   CloseKey;
   Free;
  end;
 ASIOOutput.ASIOHost.Active := False;
 FreeAndNil(ASIOOutput);
end;

// returns >=0 on success, <0 on failure
// called by input plug-in just before starting play
function Open(Samplerate, NumChannels, BitsPerSample,
  BufferLen_ms, PreBuffer_ms: Integer): Integer; cdecl;
var
  Channel: Integer;
begin
 Assert(Assigned(ASIOOutput));
 with ASIOOutput do
  begin
   SetLength(ASIOOutput.AB, Max(NumChannels, 2));
   ABSize := Max(8192, ASIOHost.BufferSize);
   for Channel := 0 to Max(NumChannels, 2) - 1
    do ReallocMem(AB[Channel], ABSize * SizeOf(Single));
   ASIOHost.Samplerate := Samplerate;
   if ASIOHost.Samplerate <> Samplerate
    then ShowMessage('Samplerate ' + IntToStr(Round(Samplerate)) + ' not yet supported!');
   ASIOHost.Active := True;
   NCh := NumChannels;
   Bps := BitsPerSample;
   GlobalPos := 0;
   GPBSN := 1 / (1000 * Bps * Samplerate * NCh);
  end;
 Result := Integer(ASIOOutput.ASIOHost.Active);
end;

procedure Close; cdecl; // called by input plug-in to close output device
begin
 ASIOOutput.ASIOHost.Active := False;
end;

{ 0 on success. Length == bytes to write (<= 8192 always).
  1 returns not able to write (yet). Non-blocking, always. }
function Write(Buffer: Pointer; Length: Integer): Integer; cdecl;
const
  DivFak: Single = 1 / $8000;
  MulFak: Single = $7FFF;
var
  i, n: Integer;
begin
 Result := 1;
 with ASIOOutput do
  if Bps = 16 then
   for i := 0 to (Length div (nCh * 2)) - 1 do
    begin
     for n := 0 to NCh - 1
      do AB[n, RP] := FVolume * PSmallIntArray(Buffer)^[i * nCh + n] * DivFak;
     Inc(RP);
     if RP >= ABSize then RP := 0;
     Result := 0;
    end;
end;

function CanWrite: Integer; cdecl;
begin
 Result := ASIOOutput.CanPlay;
end;

function IsPlaying: Integer; cdecl;
begin
 Result := 0;
end;

function Pause(IsPause: Integer): Integer; cdecl; // returns previous pause state
begin
 with ASIOOutput do
  begin
   Result := Integer(PauseState);
   PauseState := Boolean(IsPause);
   ASIOHost.Active := not PauseState;
  end;
end;

procedure SetVolume(Volume: Integer); cdecl; { volume is 0-255 }
begin
 Assert(Assigned(ASIOOutput));
 if (Volume >= 0) and (Volume <= 255)
  then ASIOOutput.Volume := Volume * 0.003921568627450980392156862745098;
end;

procedure SetPan(Pan: Integer); cdecl; { pan is -128 to 128 }
begin
 Assert(Assigned(ASIOOutput));
 ASIOOutput.Pan := Pan * 0.0078125;
end;

{ Flushes buffers and restarts output at time t (in ms) (used for seeking)
  This procedure is called by input plug-in when position is changed }
procedure Flush(t: Integer); cdecl;
begin
  with ASIOOutput do
    GlobalPos := Round((t * 1000) * Bps * ASIOHost.Samplerate * NCh);
end;

function GetOutputTime: Integer; cdecl; { returns played time in MS }
begin
 with ASIOOutput do Result := Round(GlobalPos * GPBSN);
end;

{ TFmASIOConfig }

procedure TFmASIOConfig.FormCreate(Sender: TObject);
begin
  CBDriver.Items := ASIOHost.DriverList;
  FVolume := 1;
  FPan := 0;
  with TRegistry.Create do
     try
      if OpenKeyReadOnly('Software\WinAmp\ASIO Output') then
       begin
        if ValueExists('Visible') then
          if ReadBool('Visible') then
            Show;
        if ValueExists('Left') then
          Left := ReadInteger('Left');
        if ValueExists('Top') then
          Top := ReadInteger('Top');
        if ValueExists('Driver') then
         begin
          CBDriver.ItemIndex := ReadInteger('Driver');
          ASIOHost.DriverIndex := ReadInteger('Driver');
         end;
       end;
     finally
      CloseKey;
      Free;
     end;
end;

function TFmASIOConfig.CanPlay: Integer;
begin
  if WP < RP
   then Result := ABSize - RP + WP
   else Result := WP - RP;
end;

procedure TFmASIOConfig.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := outmod.hMainWindow;
end;

procedure TFmASIOConfig.CBDriverChange(Sender: TObject);
var
  act: Boolean;
begin
 act := ASIOHost.Active;
 ASIOHost.Active := False;
 ASIOHost.DriverIndex := CBDriver.ItemIndex;
 ASIOHost.Active := act;
 with TRegistry.Create do
  try
   if OpenKey('Software\WinAmp\ASIO Output', True) then
     WriteInteger('Driver', ASIOHost.DriverIndex);
  finally
   CloseKey;
   Free;
  end;
end;

procedure TFmASIOConfig.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmASIOConfig.ASIOHostSampleRateChanged(Sender: TObject);
begin
 GPBSN := 1 / (1000 * Bps * ASIOHost.Samplerate * NCh);
end;

procedure TFmASIOConfig.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  n, l: Integer;
begin
 l := min(Length(OutBuffer), Length(AB));
 if ABSize - WP >= ASIOHost.BufferSize then
  begin
   for n := 0 to l - 1
    do Move(AB[n, WP], OutBuffer[n, 0], ASIOHost.BufferSize * SizeOf(Single));
   WP := WP + ASIOHost.BufferSize;
   if WP >= ABSize
    then WP := 0;
  end
 else
  begin
   for n := 0 to l - 1
    do Move(AB[n, WP], OutBuffer[n, 0], (ABSize - WP) * SizeOf(Single));
   WP := ASIOHost.BufferSize - (ABSize - WP);
   for n := 0 to l - 1
    do Move(AB[n, 0], OutBuffer[n, 0], WP * SizeOf(Single));
  end;
end;

procedure TFmASIOConfig.ASIOHostLatencyChanged(Sender: TObject);
begin
 GPBSN := 1 / (1000 * Bps * ASIOHost.Samplerate * NCh);
end;

end.
