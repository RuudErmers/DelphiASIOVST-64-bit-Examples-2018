unit DAV_ModularEnvelopeFollower;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_ModularBaseComponent, DAV_Types;

type
  TDspEnvelopeFollower = class(TDspBaseComponent)
  protected
    FLastOutputSingle : TDAVSingleDynArray;
    FLastOutputDouble : TDAVDoubleDynArray;
    FInternalAttack   : Single;
    FInternalRelease  : Single;
    FAttack           : Single;
    FRelease          : Single;
    procedure SetAttack(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure BeforeDestroy; override;
  protected
    procedure SampleRateChanged; override;
    procedure ChannelsChanged; override;
    procedure Process(var Data: Single; const channel: integer); overload;
    procedure Process(var Data: Double; const channel: integer); overload;
  public
    procedure Init; override;
    procedure Reset; override;
  published
    property Attack:  Single read FAttack write SetAttack;   // 0..1
    property Release: Single read FRelease write SetRelease; // 0..1
  end;

implementation

uses
  Math, DAV_Common;

procedure TDspEnvelopeFollower.Init;
begin
  FStdProcessS  := Process;
  FStdProcessD  := Process;

  FAttack:=0;
  FRelease:=0;
  Reset;
end;

procedure TDspEnvelopeFollower.BeforeDestroy;
begin
  SetLength(FLastOutputSingle, 0);
  SetLength(FLastOutputDouble, 0);
end;

procedure TDspEnvelopeFollower.Reset;
begin
  ChannelsChanged;
  SampleRateChanged;
end;

procedure TDspEnvelopeFollower.SampleRateChanged;
begin
  FInternalAttack  := power(0.01, 1/((0.001 + FAttack  * 1.999) * fSampleRate));
  FInternalRelease := power(0.01, 1/((0.001 + FRelease * 1.999) * fSampleRate));
end;

procedure TDspEnvelopeFollower.ChannelsChanged;
begin
  SetLength(FLastOutputSingle, fChannels);
  SetLength(FLastOutputDouble, fChannels);

  FillChar(FLastOutputSingle[0], fChannels * SizeOf(Single), 0);
  FillChar(FLastOutputDouble[0], fChannels * SizeOf(Double), 0);
end;


procedure TDspEnvelopeFollower.SetAttack(const Value: Single);
begin
  if FAttack <> Value then
  begin
    FAttack := max(0,min(1,Value));
    SampleRateChanged;
  end;
end;

procedure TDspEnvelopeFollower.SetRelease(const Value: Single);
begin
  if FRelease <> Value then
  begin
    FRelease := max(0,min(1,Value));
    SampleRateChanged;
  end;
end;

procedure TDspEnvelopeFollower.Process(var Data: Double; const channel: integer);
var tmp: Double;
begin
 {$IFDEF FPC}
  Data := abs(Data);
 {$ELSE}
  FastAbs(Data);
 {$ENDIF}

  if Data>=FLastOutputDouble[channel] then
    tmp:=FInternalAttack
  else
    tmp:=FInternalRelease;

  FLastOutputDouble[channel] := tmp * (FLastOutputDouble[channel] - Data) + Data;
  Data:=FLastOutputDouble[channel];
end;

procedure TDspEnvelopeFollower.Process(var Data: Single; const channel: integer);
var tmp: Single;
begin
 {$IFDEF FPC}
  Data := abs(Data);
 {$ELSE}
  FastAbs(Data);
 {$ENDIF}

  if Data>=FLastOutputSingle[channel] then
    tmp:=FInternalAttack
  else
    tmp:=FInternalRelease;

  FLastOutputSingle[channel] := tmp * (FLastOutputSingle[channel] - Data) + Data;
  Data:=FLastOutputSingle[channel];  
end;

end.
