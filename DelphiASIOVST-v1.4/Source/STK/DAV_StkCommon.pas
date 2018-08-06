unit DAV_StkCommon;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common;

{$I DAV_StkConsts.inc}

type
  TStk = class
  public
    procedure SetSampleRate(const Value: Single);
  protected
    FSampleRate    : Single;
    FSampleRateInv : Single;
    procedure SampleRateChanged; virtual;
  public
    constructor Create(const SampleRate: Single = 44100); virtual;

    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

implementation

constructor TStk.Create(const SampleRate: Single = 44100);
begin
 FSampleRate := SampleRate;
 SampleRateChanged;
end;

procedure TStk.SampleRateChanged;
begin
 FSampleRateInv := 1 / FSampleRate;
end;

procedure TStk.SetSampleRate(const Value: Single);
begin
 if (Value > 0) then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

end.
