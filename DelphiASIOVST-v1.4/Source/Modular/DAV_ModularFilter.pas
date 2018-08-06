unit DAV_ModularFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_ModularBase, DAV_ModularPin,
  DAV_DspFilter, DAV_DspFilterBasics, DAV_DspFilterButterworth;

type
  TCustomModularFilter = class(TCustomModularBase)
  protected
    FFilter : TCustomFilter;
    procedure SetupPins; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessModule; override;
  end;

  TCustomModularBandwidthIIRFilter = class(TCustomModularFilter)
  protected
    procedure SetupPins; override;
  end;

  TModularBasicGainFilter = class(TCustomModularBandwidthIIRFilter)
  public
    constructor Create; override;
  end;

  TModularBasicPeakFilter = class(TCustomModularBandwidthIIRFilter)
  public
    constructor Create; override;
  end;

  TModularBasicAllpassFilter = class(TCustomModularBandwidthIIRFilter)
  public
    constructor Create; override;
  end;

  TModularBasicLowShelfFilter = class(TCustomModularBandwidthIIRFilter)
  public
    constructor Create; override;
  end;

  TModularBasicLowShelfAFilter = class(TCustomModularBandwidthIIRFilter)
  public
    constructor Create; override;
  end;

  TModularBasicLowShelfBFilter = class(TCustomModularBandwidthIIRFilter)
  public
    constructor Create; override;
  end;

  TModularBasicHighShelfFilter = class(TCustomModularBandwidthIIRFilter)
  public
    constructor Create; override;
  end;

  TModularBasicHighShelfAFilter = class(TCustomModularBandwidthIIRFilter)
  public
    constructor Create; override;
  end;

  TModularBasicHighShelfBFilter = class(TCustomModularBandwidthIIRFilter)
  public
    constructor Create; override;
  end;

  TCustomModularOrderFilter = class(TCustomModularFilter)
  protected
    procedure SetupPins; override;
  end;

implementation

uses
  SysUtils;

{ TCustomModularFilter }

constructor TCustomModularFilter.Create;
begin
 inherited;
 SetupPins;
end;

destructor TCustomModularFilter.Destroy;
begin
 FreeAndNil(FFilter);
 inherited;
end;

procedure TCustomModularFilter.SetupPins;
begin
 // setup input pin
 with FPinsInput.Add do
  begin
   Datatype    := mdtSingle;
   DisplayName := 'Input';
  end;

 // setup output pin
 with FPinsOutput.Add do
  begin
   Datatype := mdtSingle;
   DisplayName := 'Output';
  end;
end;

procedure TCustomModularFilter.ProcessModule;
var
  Sample : Integer;
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
begin
 inherited;

 // some asserts to make sure everything is working as it should
 assert(FPinsInput.Count = 1);
 assert(FPinsOutput.Count = 1);
 assert(FPinsInput[0].Buffersize = FPinsOutput[0].Buffersize);

 Input  := FPinsInput[0].BufferAsSingleArray;
 Output := FPinsOutput[0].BufferAsSingleArray;

 // process samples
 for Sample := 0 to FPinsInput[0].Buffersize - 1
  do Output[Sample] := FFilter.ProcessSample64(Input[Sample]);

end;

{ TCustomModularBandwidthIIRFilter }

procedure TCustomModularBandwidthIIRFilter.SetupPins;
begin
 inherited;

 // setup frequency pin
 with FPinsInput.Add do
  begin
   Datatype := mdtSingle;
   DisplayName := 'Frequency';
  end;

 // setup gain pin
 with FPinsInput.Add do
  begin
   Datatype := mdtSingle;
   DisplayName := 'Gain';
  end;

 // setup bandwidth pin
 with FPinsInput.Add do
  begin
   Datatype := mdtSingle;
   DisplayName := 'Bandwidth';
  end;
end;

{ TModularBasicGainFilter }

constructor TModularBasicGainFilter.Create;
begin
 inherited;
 FFilter := TBasicGainFilter.Create;
 FName := 'Gain Filter';
end;

{ TModularBasicPeakFilter }

constructor TModularBasicPeakFilter.Create;
begin
 inherited;
 FFilter := TBasicPeakFilter.Create;
 FName := 'Peak Filter';
end;

{ TModularBasicAllpassFilter }

constructor TModularBasicAllpassFilter.Create;
begin
 inherited;
 FFilter := TBasicAllpassFilter.Create;
 FName := 'Allpass Filter';
end;

{ TModularBasicLowShelfFilter }

constructor TModularBasicLowShelfFilter.Create;
begin
 inherited;
 FFilter := TBasicLowShelfFilter.Create;
 FName := 'Lowshelf Filter';
end;

{ TModularBasicLowShelfAFilter }

constructor TModularBasicLowShelfAFilter.Create;
begin
 inherited;
 FFilter := TBasicLowShelfAFilter.Create;
 FName := 'Lowshelf (Type A) Filter';
end;

{ TModularBasicLowShelfBFilter }

constructor TModularBasicLowShelfBFilter.Create;
begin
 inherited;
 FFilter := TBasicLowShelfBFilter.Create;
 FName := 'Lowshelf (Type B) Filter';
end;

{ TModularBasicHighShelfFilter }

constructor TModularBasicHighShelfFilter.Create;
begin
 inherited;
 FFilter := TBasicHighShelfFilter.Create;
 FName := 'HighShelf Filter';
end;

{ TModularBasicHighShelfAFilter }

constructor TModularBasicHighShelfAFilter.Create;
begin
 inherited;
 FFilter := TBasicHighShelfAFilter.Create;
 FName := 'HighShelf (Type A) Filter';
end;

{ TModularBasicHighShelfBFilter }

constructor TModularBasicHighShelfBFilter.Create;
begin
 inherited;
 FFilter := TBasicHighShelfBFilter.Create;
 FName := 'HighShelf (Type B) Filter';
end;

{ TCustomModularOrderFilter }

procedure TCustomModularOrderFilter.SetupPins;
begin
 // setup frequency pin
 with FPinsInput.Add do
  begin
   Datatype := mdtSingle;
   DisplayName := 'Frequency';
  end;

 // setup gain pin
 with FPinsInput.Add do
  begin
   Datatype := mdtSingle;
   DisplayName := 'Gain';
  end;

 // setup order pin
 with FPinsInput.Add do
  begin
   Datatype := mdtInteger;
   DisplayName := 'Order';
  end;
end;

end.
