unit DAV_ModularEnvelope;

interface
               
{$I ..\DAV_Compiler.inc}

uses
  DAV_ModularBaseComponent, DAV_Common;

type
  TDspEnvelopeInputProcessing = (eipIgnoreInput, eipMultiply, eipAdd);
  TDspEnvelope = class(TDspBaseComponent)
  private
  protected
    FAttack: Single;
    FDecay: Single;
    FSustain: Single;
    FRelease: Single;
    FAttackSamples: Integer;
    FDecaySamples: Integer;
    FMaxAmplitude: Single;

    FInverted: Boolean;
    FInputProcessing: TDspEnvelopeInputProcessing;
    fInternalCounter: array of integer;
    fNoteOff: Boolean;

    procedure SampleRateChanged; override;
    procedure BeforeDestroy; override;
    procedure Loaded; override;
    procedure SetAttack(const Value: Single);
    procedure SetDecay(const Value: Single);
    procedure SetSustain(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetMaxAmplitude(const Value: Single);

    procedure SetInputProcessing(const Value: TDspEnvelopeInputProcessing);
    procedure SetInverted(const Value: Boolean);

    procedure TimeToSamples;

    procedure ChannelsChanged;   override;
    procedure IgnoreInputProcess(var Data: Single; const channel: integer); overload;
    procedure IgnoreInputProcess(var Data: Double; const channel: integer); overload;
    procedure MultiplyProcess(var Data: Single; const channel: integer); overload;
    procedure MultiplyProcess(var Data: Double; const channel: integer); overload;
    procedure AddProcess(var Data: Single; const channel: integer); overload;
    procedure AddProcess(var Data: Double; const channel: integer); overload;
  public
    procedure Init;  override;
    procedure Reset; override;
    procedure NoteOff; override;
    procedure NoteOn;
  published
    property Attack:  Single read FAttack write SetAttack; // time in seconds
    property Decay:   Single read FDecay write SetDecay; // time in seconds
    property Sustain: Single read FSustain write SetSustain; // 0..1
    property Release: Single read FRelease write SetRelease; // time in seconds

    property MaxAmplitude: Single read FMaxAmplitude write SetMaxAmplitude; // -unlimited..unlimited

    property Inverted: Boolean read FInverted write SetInverted default false;
    property InputProcessing: TDspEnvelopeInputProcessing read FInputProcessing write SetInputProcessing default eipMultiply;
  end;

implementation


uses
  Math, Dialogs;

{ TDspEnvelope }

procedure TDspEnvelope.Init;
begin         
  fStdProcessS  := MultiplyProcess;
  fStdProcessD  := MultiplyProcess;
  
  FInverted := false;
  FInputProcessing := eipMultiply;
  fAttack := 0;
  fDecay := 0;
  fSustain := 0;
  fRelease := 0;
  fMaxAmplitude := 0;
  fNoteOff := false;
  Reset;
  TimeToSamples;
end; 

procedure TDspEnvelope.Loaded;
begin
  Reset;
  TimeToSamples;
end;

procedure TDspEnvelope.Reset;
begin
  ChannelsChanged;
  SampleRateChanged;
end;   

procedure TDspEnvelope.NoteOff;
begin
  fNoteOff := true;
  ChannelsChanged;
end;

procedure TDspEnvelope.NoteOn;
begin
  fNoteOff := false;
  ChannelsChanged;
end;

procedure TDspEnvelope.BeforeDestroy;
begin
  SetTrailingSamples(0);
  SetLength(fInternalCounter, 0);
end;

procedure TDspEnvelope.ChannelsChanged;
begin
  SetLength(fInternalCounter, fChannels);

  FillChar(fInternalCounter[0], fChannels * SizeOf(Single), 0);
end;

procedure TDspEnvelope.SampleRateChanged;
begin
  TimeToSamples;
end;

procedure TDspEnvelope.TimeToSamples;
begin
  FAttackSamples  := ceil(FAttack  * fSampleRate);
  FDecaySamples   := ceil(FDecay   * fSampleRate);
  SetTrailingSamples(ceil(FRelease * fSampleRate));
end;

procedure TDspEnvelope.SetAttack(const Value: Single);
begin
  if FAttack<>Value then
  begin
    FAttack := Value;
    TimeToSamples;
  end;
end;

procedure TDspEnvelope.SetDecay(const Value: Single);
begin
  if FDecay<>Value then
  begin
    FDecay := Value;
    TimeToSamples;
  end;
end;

procedure TDspEnvelope.SetSustain(const Value: Single);
begin
  FSustain := Value;
end;

procedure TDspEnvelope.SetRelease(const Value: Single);
begin
  if FRelease<>Value then
  begin
    FRelease := Value;
    TimeToSamples;
  end;
end;


procedure TDspEnvelope.SetMaxAmplitude(const Value: Single);
begin
  FMaxAmplitude := Value;
end;

procedure TDspEnvelope.SetInputProcessing(const Value: TDspEnvelopeInputProcessing);
begin
  FInputProcessing := Value;

  case FInputProcessing of
    eipIgnoreInput: begin
            fStdProcessS  := IgnoreInputProcess;
            fStdProcessD  := IgnoreInputProcess;
          end;
    eipMultiply: begin
            fStdProcessS  := MultiplyProcess;
            fStdProcessD  := MultiplyProcess;
          end;
    eipAdd: begin
            fStdProcessS  := AddProcess;
            fStdProcessD  := AddProcess;
          end;
  end;
  UpdateProcessingFunc;
end;

procedure TDspEnvelope.SetInverted(const Value: Boolean);
begin
  FInverted := Value;
end;

procedure TDspEnvelope.IgnoreInputProcess(var Data: Single; const channel: integer);
begin
  if fNoteOff then
  begin
    // Release
    Data := FSustain * (1-fInternalCounter[channel]/fTrailingSamples);

  end else if fInternalCounter[channel]>FDecaySamples+FAttackSamples then
  begin
    // Sustain
    Data := FSustain;
  end else if fInternalCounter[channel]>FAttackSamples then
  begin
    // Decay
    if FSustain=1 then
      Data := 1
    else begin
      Data := 1-(fInternalCounter[channel]-FAttackSamples)/FDecaySamples;
      Data := Data*(1-FSustain)+FSustain;
    end;
  end else begin
    // Attack
    Data := fInternalCounter[channel]/FAttackSamples;
  end;


  if FInverted
   then Data := MaxAmplitude * (1 - Data)
   else Data := Data * MaxAmplitude;

  inc(fInternalCounter[channel]);
end;

procedure TDspEnvelope.IgnoreInputProcess(var Data: Double; const channel: integer);
begin
  if fNoteOff then
   begin
    // Release
    Data := FSustain * (1 - fInternalCounter[channel] / fTrailingSamples);

   end
  else if fInternalCounter[channel] > FDecaySamples + FAttackSamples then
   begin
     // Sustain
     Data := FSustain;
   end
  else if fInternalCounter[channel] > FAttackSamples then
   begin
    // Decay
    if FSustain = 1
     then Data := 1
     else
      begin
       Data := 1 - (fInternalCounter[channel] - FAttackSamples) / FDecaySamples;
       Data := Data * (1 - FSustain) + FSustain;
      end;
   end
  else
   begin
    // Attack
    Data := fInternalCounter[channel] / FAttackSamples;
   end;


  if FInverted
   then Data := MaxAmplitude * (1 - Data)
   else Data := Data*MaxAmplitude;

  inc(fInternalCounter[channel]);
end;

procedure TDspEnvelope.MultiplyProcess(var Data: Single; const channel: integer);
var
  amp: Single;
begin
  IgnoreInputProcess(amp, channel);
  Data := Data*amp;
end;

procedure TDspEnvelope.MultiplyProcess(var Data: Double; const channel: integer);        
var
  amp : Double;
begin
  IgnoreInputProcess(amp, channel);
  Data := Data*amp;
end;


procedure TDspEnvelope.AddProcess(var Data: Single; const channel: integer);
var
  amp : Single;
begin
  IgnoreInputProcess(amp, channel);
  Data := Data+amp;
end;

procedure TDspEnvelope.AddProcess(var Data: Double; const channel: integer);
var
  amp : Double;
begin
  IgnoreInputProcess(amp, channel);
  Data := Data + amp;
end;

end.
