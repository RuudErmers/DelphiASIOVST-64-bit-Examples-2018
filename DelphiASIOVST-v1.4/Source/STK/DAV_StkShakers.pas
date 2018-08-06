unit DAV_StkShakers;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ PhISEM and PhOLIES class.

  PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic
  approach for simulating collisions of multiple independent sound producing
  objects. This class is a meta-model that can simulate a Maraca, Shekere,
  Cabasa, Bamboo Wind Chimes, Water Drops, Tambourine, Sleighbells, and a Guiro.

  PhOLIES (Physically-Oriented Library of Imitated Environmental Sounds) is a
  similar approach for the synthesis of environmental sounds. This class
  implements simulations of breaking sticks, crunchy snow (or not), a wrench,
  sandpaper, and more.

  Control Change Numbers:
    - Shake Energy = 2
    - System Decay = 4
    - Number Of Objects = 11
    - Resonance Frequency = 1
    - Shake Energy = 128
    - Instrument Selection = 1071
      - Maraca = 0
      - Cabasa = 1
      - Shekere = 2
      - Guiro = 3
      - Water Drops = 4
      - Bamboo Chimes = 5
      - Tambourine = 6
      - Sleigh Bells = 7
      - Sticks = 8
      - Crunch = 9
      - Wrench = 10
      - Sand Paper = 11
      - Coke Can = 12
      - Next Mug = 13
      - Penny + Mug = 14
      - Nickle + Mug = 15
      - Dime + Mug = 16
      - Quarter + Mug = 17
      - Franc + Mug = 18
      - Peso + Mug = 19
      - Big Rocks = 20
      - Little Rocks = 21
      - Tuned Bamboo Chimes = 22
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, Math;

const
  CMaxFreqs = 8;
  CNumInstr = 24;

type
  TStkShakers = class(TStkControlableInstrument)
  protected
    FShakeEnergy    : Single;
    FSndLevel       : Single;
    FBaseGain       : Single;
    FSoundDecay     : Single;
    FSystemDecay    : Single;
    FNObjects       : Single;
    FCollLikely     : Single;
    FTotalEnergy    : Single;
    FRatchet        : Single;
    FRatchetDelta   : Single;
    FCenterFreqs    : array[0..CMaxFreqs - 1] of Single;
    FTCenterFreqs   : array[0..CMaxFreqs - 1] of Single;
    FGains          : array[0..CMaxFreqs - 1] of Single;
    FFreqRand       : array[0..CMaxFreqs - 1] of Single;
    FResons         : array[0..CMaxFreqs - 1] of Single;
    FInputs         : array[0..CMaxFreqs - 1] of Single;
    FCoeffs         : array[0..CMaxFreqs - 1, 0..1] of Single;
    FOutputs        : array[0..CMaxFreqs - 1, 0..1] of Single;
    FFinalZCoeffs   : array[0..2] of Single;
    FFinalZ         : array[0..2] of Single;
    FDecayScale     : array[0..CNumInstr - 1] of Single;
    FDefDecays      : array[0..CNumInstr - 1] of Single;
    FDefObjs        : array[0..CNumInstr - 1] of Single;
    FFreqalloc      : array[0..CMaxFreqs - 1] of Integer;
    FNFreqs         : Integer;
    FRatchetPos     : Integer;
    FLastRatchetPos : Integer;
    FInstType       : Integer;
  //  int setupName(char* instr);
    function SetFreqAndReson(Which: Integer; Freq, Reson: Single): Integer;
    procedure SetDecays(SndDecay, SysDecay: Single);
    procedure SetFinalZs(z0, z1, z2: Single);
    function SetupNum(Inst: Integer): Integer;
    function WuterTick: Single;
    function TunedBambooTick: Single;
    function RatchetTick: Single;

    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;
  public
    // Class constructor.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Start a note with the given instrument and amplitude.
  {
    Use the instrument numbers above, converted to frequency values
    as if MIDI note numbers, to select a particular instrument.
  }
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;
  end;

implementation

function NoiseTick: Single;
 //  Return random Single float between -1.0 and 1.0
begin
  Result := 2 * random - 1;
end;

// Maraca
const
  CMaracasSoundDecay = 0.95;
  CMaracasSystemDecay = 0.999;
  CMaracasGain = 20.0;
  CMaracasNumBeans = 25;
  CMaracasCenterFreq = 3200.0;
  CMaracasReson = 0.96;

// Shekere
  CShekereSoundDecay = 0.96;
  CShekereSystemDecay = 0.999;
  CShekereGain = 20.0;
  CShekereNumBeans = 64;
  CShekereCenterFreq = 5500.0;
  CShekereReson = 0.6;

// Sandpaper
  CSandpaperSoundDecay = 0.999;
  CSandpaperSystemDecay = 0.999;
  CSandpaperGain = 0.5;
  CSandpaperNumGains = 128;
  CSandpaperCenterFreq = 4500.0;
  CSandpaperReson = 0.6;

// Cabasa
  CCabasaSoundDecay = 0.96;
  CCabasaSystemDecay = 0.997;
  CCabasaGain = 40.0;
  CCabasaNumBeads = 512;
  CCabasaCenterFreq = 3000.0;
  CCabasaReson = 0.7;

// Bamboo Wind Chimes
  CBambooSoundDecay = 0.95;
  CBambooSystemDecay = 0.9999;
  CBambooGain = 2.0;
  CBambooNumTubes = 1.25;
  CBambooCenterFreq0 = 2800.0;
  CBambooCenterFreq1 = 0.8 * 2800.0;
  CBambooCenterFreq2 = 1.2 * 2800.0;
  CBambooReson = 0.995;

// Tuned Bamboooo Wind Chimes (Anklung)
  CTunedBambooSoundDecay = 0.95;
  CTunedBambooSystemDecay = 0.9999;
  CTunedBambooGain = 1.0;
  CTunedBambooNumTubes = 1.25;
  CTunedBambooCenterFreq0 = 1046.6;
  CTunedBambooCenterFreq1 = 1174.8;
  CTunedBambooCenterFreq2 = 1397.0;
  CTunedBambooCenterFreq3 = 1568.0;
  CTunedBambooCenterFreq4 = 1760.0;
  CTunedBambooCenterFreq5 = 2093.3;
  CTunedBambooCenterFreq6 = 2350.0;
  CTunedBambooReson = 0.996;

// Water Drops
  CWaterDropsSoundDecay = 0.95;
  CWaterDropsSystemDecay = 0.996;
  CWaterDropsGain = 1.0;
  CWaterDropsNumSources = 10;
  CWaterDropsCenterFreq0 = 450.0;
  CWaterDropsCenterFreq1 = 600.0;
  CWaterDropsCenterFreq2 = 750.0;
  CWaterDropsReson = 0.9985;
  CWaterDropsFreqSweep = 1.0001;

// Tambourine
  CTambourineSoundDecay = 0.95;
  CTambourineSystemDecay = 0.9985;
  CTambourineGain = 5.0;
  CTambourineNumTimbRels = 32;
  CTambourineShellFreq = 2300;
  CTambourineShellGain = 0.1;
  CTambourineShellReson = 0.96;
  CTambourineCymbFreq1 = 5600;
  CTambourineCymbFreq2 = 8100;
  CTambourineCymbReson = 0.99;

// Sleighbells
  CSleighbellsSoundDecay = 0.97;
  CSleighbellsSystemDecayY = 0.9994;
  CSleighbellsGain = 1.0;
  CSleighbellsNumBells = 32;
  CSleighbellsCymbFreq0 = 2500;
  CSleighbellsCymbFreq1 = 5300;
  CSleighbellsCymbFreq2 = 6500;
  CSleighbellsCymbFreq3 = 8300;
  CSleighbellsCymbFreq4 = 9800;
  CSleighbellsCymbReson = 0.99;

// Guiro
  CGuiroSoundDecay = 0.95;
  CGuiroGain = 10.0;
  CGuiroNumParts = 128;
  CGuiroGourdFreq = 2500.0;
  CGuiroGourdReson = 0.97;
  CGuiroGourdFreq2 = 4000.0;
  CGuiroGourdReson2 = 0.97;

// Wrench
  CWrenchSoundDecay = 0.95;
  CWrenchGain = 5;
  CWrenchNumParts = 128;
  CWrenchFreq = 3200.0;
  CWrenchReson = 0.99;
  CWrenchFreq2 = 8000.0;
  CWrenchReson2 = 0.992;

// Cokecan
  CCokeCanSoundDecay = 0.97;
  CCokeCanSystemDecay = 0.999;
  CCokeCanGain = 0.8;
  CCokeCanNumParts = 48;
  CCokeCanHelmFreq = 370;
  CCokeCanHelmRes = 0.99;
  CCokeCanMetalFreq0 = 1025;
  CCokeCanMetalFreq1 = 1424;
  CCokeCanMetalFreq2 = 2149;
  CCokeCanMetalFreq3 = 3596;
  CCokeCanMetalRes = 0.992;

// PhOLIES (Physically-Oriented Library of Imitated Environmental
// Sounds), Perry Cook,=1997-8

// Stix1
  CStix1SoundDecay = 0.96;
  CStix1SystemDecay = 0.998;
  CStix1Gain = 30.0;
  CStix1NumBeans = 2;
  CStix1CenterFreq = 5500.0;
  CStix1Reson = 0.6;

// Crunch1
  CCrunch1SoundDecay = 0.95;
  CCrunch1SystemDecay = 0.99806;
  CCrunch1Gain = 20.0;
  CCrunch1NumBeads = 7;
  CCrunch1CenterFreq = 800.0;
  CCrunch1Reson = 0.95;

// Nextmug
  CNextMugSoundDecay = 0.97;
  CNextMugSystemDecay = 0.9995;
  CNextMugGain = 0.8;
  CNextMugNumParts = 3;
  CNextMugFreq0 = 2123;
  CNextMugFreq1 = 4518;
  CNextMugFreq2 = 8856;
  CNextMugFreq3 = 10753;
  CNextMugRes = 0.997;

  CPennyFreq0 = 11000;
  CPennyFreq1 = 5200;
  CPennyFreq2 = 3835;
  CPennyRes = 0.999;

  CNickelFreq0 = 5583;
  CNickelFreq1 = 9255;
  CNickelFreq2 = 9805;
  CNickelRes = 0.9992;

  CDimeFreq0 = 4450;
  CDimeFreq1 = 4974;
  CDimeFreq2 = 9945;
  CDimeRes = 0.9993;

  CQuarterFreq0 = 1708;
  CQuarterFreq1 = 8863;
  CQuarterFreq2 = 9045;
  CQuarterRes = 0.9995;

  CFrancFreq0 = 5583;
  CFrancFreq1 = 11010;
  CFrancFreq2 = 1917;
  CFrancRes = 0.9995;

  CPesoFreq0 = 7250;
  CPesoFreq1 = 8150;
  CPesoFreq2 = 10060;
  CPesoRes = 0.9996;

// Big Gravel
  CBigGravelSoundDecay = 0.98;
  CBigGravelSystemDecay = 0.9965;
  CBigGravelGain = 20.0;
  CBigGravelNumParts = 23;
  CBigGravelFreq = 6460;
  CBigGravelRes = 0.932;

// Little Gravel
  CLittleGravelSoundDecay = 0.98;
  CLittleGravelSystemDecay = 0.99586;
  CLittleGravelGain = 20.0;
  CLittleGravelNumParts = 1600;
  CLittleGravelFreq = 9000;
  CLittleGravelRes = 0.843;

// Finally ... the class code!

constructor TStkShakers.Create(const SampleRate: Single);
var
  i: Integer;
begin
  inherited Create(SampleRate);
  FInstType := 0;
  FShakeEnergy := 0.0;
  FNFreqs := 0;
  FSndLevel := 0.0;

  for i := 0 to CMaxFreqs - 1 do
   begin
    FInputs[i] := 0.0;
    FOutputs[i][0] := 0.0;
    FOutputs[i][1] := 0.0;
    FCoeffs[i][0] := 0.0;
    FCoeffs[i][1] := 0.0;
    FGains[i] := 0.0;
    FCenterFreqs[i] := 0.0;
    FResons[i] := 0.0;
    FFreqRand[i] := 0.0;
    FFreqalloc[i] := 0;
   end;

  FSoundDecay := 0.0;
  FSystemDecay := 0.0;
  FNObjects := 0.0;
  FCollLikely := 0.0;
  FTotalEnergy := 0.0;
  FRatchet := 0.0;
  FRatchetDelta := 0.0005;
  FLastRatchetPos := 0;
  FFinalZ[0] := 0.0;
  FFinalZ[1] := 0.0;
  FFinalZ[2] := 0.0;
  FFinalZCoeffs[0] := 1.0;
  FFinalZCoeffs[1] := 0.0;
  FFinalZCoeffs[2] := 0.0;

  SetupNum(FInstType);
end;

destructor TStkShakers.Destroy;
begin
  inherited Destroy;
end;

function TStkShakers.GetFrequency: Single;
begin
 result := 0;
end;

const
  CMaxShake = 2000.0;

{
char instrs[CNumInstr][10]:=begin
  "Maraca", "CCabasasa", "Sekere", "Guiro",
  "Waterdrp", "CBamboooo", "CTambourineourn", "Sleighbl",
  "Stix1", "Crunch1", "Wrench", "CSandpaper",
  "CokeCan", "NextMug", "PennyMug", "NicklMug",
  "DimeMug", "QuartMug", "FrancMug", "PesoMug",
  "BigRocks", "LitlRoks", "CTunedBamboooo"
end;

int TStkShakers.SsetupName(instr: PChar)
begin
  int Which:=0;

  for (int i=0;i<CNumInstr;i++)  begin
    if ( !strcmp(instr,instrs[i]) )
      Which:=i;
  end;

#if defined(_STK_DEBUG_)
  cerr << "TShakers: Setting instrument to " << instrs[Which] << endl;
#endif

  result:= SetupNum(Which);
end;

}

procedure TStkShakers.SetFinalZs;
begin
  FFinalZCoeffs[0] := z0;
  FFinalZCoeffs[1] := z1;
  FFinalZCoeffs[2] := z2;
end;

procedure TStkShakers.SetDecays;
begin
  FSoundDecay := SndDecay;
  FSystemDecay := SysDecay;
end;

function TStkShakers.SetFreqAndReson(Which: Integer; Freq, Reson: Single): Integer;
begin
  if (Which < CMaxFreqs) then
   begin
    FResons[Which] := Reson;
    FCenterFreqs[Which] := Freq;
    FTCenterFreqs[Which] := Freq;
    FCoeffs[Which][1] := Reson * Reson;
    FCoeffs[Which][0] := -Reson * 2.0 * cos(Freq * 2 * Pi * FSampleRateInv);
    Result := 1;
   end
  else
    Result := 0;
end;

procedure TStkShakers.SetFrequency(const Value: Single);
begin
 inherited;
 // nothing in here yet!
end;

function TStkShakers.SetupNum(Inst: Integer): Integer;
var
  i, rv: Integer;
  temp: Single;
begin
  if (Inst = 1) then
   begin // Cabasa
    rv := Inst;
    FNObjects := CCabasaNumBeads;
    FDefObjs[Inst] := CCabasaNumBeads;
    SetDecays(CCabasaSoundDecay, CCabasaSystemDecay);
    FDefDecays[Inst] := CCabasaSystemDecay;
    FDecayScale[Inst] := 0.97;
    FNFreqs := 1;
    FBaseGain := CCabasaGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FFreqalloc[0] := 0;
    SetFreqAndReson(0, CCabasaCenterFreq, CCabasaReson);
    SetFinalZs(1.0, -1.0, 0.0);
   end
  else if (Inst = 2) then
   begin // Shekere
    rv := Inst;
    FNObjects := CShekereNumBeans;
    FDefObjs[Inst] := CShekereNumBeans;
    SetDecays(CShekereSoundDecay, CShekereSystemDecay);
    FDefDecays[Inst] := CShekereSystemDecay;
    FDecayScale[Inst] := 0.94;
    FNFreqs := 1;
    FBaseGain := CShekereGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FFreqalloc[0] := 0;
    SetFreqAndReson(0, CShekereCenterFreq, CShekereReson);
    SetFinalZs(1.0, 0.0, -1.0);
   end
  else if (Inst = 3) then
   begin //  Guiro
    rv := Inst;
    FNObjects := CGuiroNumParts;
    FDefObjs[Inst] := CGuiroNumParts;
    SetDecays(CGuiroSoundDecay, 1.0);
    FDefDecays[Inst] := 0.9999;
    FDecayScale[Inst] := 1.0;
    FNFreqs := 2;
    FBaseGain := CGuiroGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FGains[1] := temp;
    FFreqalloc[0] := 0;
    FFreqalloc[1] := 0;
    FFreqRand[0] := 0.0;
    FFreqRand[1] := 0.0;
    SetFreqAndReson(0, CGuiroGourdFreq, CGuiroGourdReson);
    SetFreqAndReson(1, CGuiroGourdFreq2, CGuiroGourdReson2);
    FRatchet := 0;
    FRatchetPos := 10;
   end
  else if (Inst = 4) then
   begin //  Water Drops
    rv := Inst;
    FNObjects := CWaterDropsNumSources;
    FDefObjs[Inst] := CWaterDropsNumSources;
    SetDecays(CWaterDropsSoundDecay, CWaterDropsSystemDecay);
    FDefDecays[Inst] := CWaterDropsSystemDecay;
    FDecayScale[Inst] := 0.8;
    FNFreqs := 3;
    FBaseGain := CWaterDropsGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FGains[1] := temp;
    FGains[2] := temp;
    FFreqalloc[0] := 1;
    FFreqalloc[1] := 1;
    FFreqalloc[2] := 1;
    FFreqRand[0] := 0.2;
    FFreqRand[1] := 0.2;
    FFreqRand[2] := 0.2;
    SetFreqAndReson(0, CWaterDropsCenterFreq0, CWaterDropsReson);
    SetFreqAndReson(1, CWaterDropsCenterFreq0, CWaterDropsReson);
    SetFreqAndReson(2, CWaterDropsCenterFreq0, CWaterDropsReson);
    SetFinalZs(1.0, 0.0, 0.0);
   end
  else if (Inst = 5) then
   begin // Bamboooo
    rv := Inst;
    FNObjects := CBambooNumTubes;
    FDefObjs[Inst] := CBambooNumTubes;
    SetDecays(CBambooSoundDecay, CBambooSystemDecay);
    FDefDecays[Inst] := CBambooSystemDecay;
    FDecayScale[Inst] := 0.7;
    FNFreqs := 3;
    FBaseGain := CBambooGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FGains[1] := temp;
    FGains[2] := temp;
    FFreqalloc[0] := 1;
    FFreqalloc[1] := 1;
    FFreqalloc[2] := 1;
    FFreqRand[0] := 0.2;
    FFreqRand[1] := 0.2;
    FFreqRand[2] := 0.2;
    SetFreqAndReson(0, CBambooCenterFreq0, CBambooReson);
    SetFreqAndReson(1, CBambooCenterFreq1, CBambooReson);
    SetFreqAndReson(2, CBambooCenterFreq2, CBambooReson);
    SetFinalZs(1.0, 0.0, 0.0);
   end
  else if (Inst = 6) then
   begin // Tambourine
    rv := Inst;
    FNObjects := CTambourineNumTimbRels;
    FDefObjs[Inst] := CTambourineNumTimbRels;
    SetDecays(CTambourineSoundDecay, CTambourineSystemDecay);
    FDefDecays[Inst] := CTambourineSystemDecay;
    FDecayScale[Inst] := 0.95;
    FNFreqs := 3;
    FBaseGain := CTambourineGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp * CTambourineShellGain;
    FGains[1] := temp * 0.8;
    FGains[2] := temp;
    FFreqalloc[0] := 0;
    FFreqalloc[1] := 1;
    FFreqalloc[2] := 1;
    FFreqRand[0] := 0.0;
    FFreqRand[1] := 0.05;
    FFreqRand[2] := 0.05;
    SetFreqAndReson(0, CTambourineShellFreq, CTambourineShellReson);
    SetFreqAndReson(1, CTambourineCymbFreq1, CTambourineCymbReson);
    SetFreqAndReson(2, CTambourineCymbFreq2, CTambourineCymbReson);
    SetFinalZs(1.0, 0.0, -1.0);
   end
  else if (Inst = 7) then
   begin // Sleighbell
    rv := Inst;
    FNObjects := CSleighbellsNumBells;
    FDefObjs[Inst] := CSleighbellsNumBells;
    SetDecays(CSleighbellsSoundDecay, CSleighbellsSystemDecayY);
    FDefDecays[Inst] := CSleighbellsSystemDecayY;
    FDecayScale[Inst] := 0.9;
    FNFreqs := 5;
    FBaseGain := CSleighbellsGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FGains[1] := temp;
    FGains[2] := temp;
    FGains[3] := temp * 0.5;
    FGains[4] := temp * 0.3;
    for i := 0 to FNFreqs - 1 do
     begin
      FFreqalloc[i] := 1;
      FFreqRand[i] := 0.03;
     end;
    SetFreqAndReson(0, CSleighbellsCymbFreq0, CSleighbellsCymbReson);
    SetFreqAndReson(1, CSleighbellsCymbFreq1, CSleighbellsCymbReson);
    SetFreqAndReson(2, CSleighbellsCymbFreq2, CSleighbellsCymbReson);
    SetFreqAndReson(3, CSleighbellsCymbFreq3, CSleighbellsCymbReson);
    SetFreqAndReson(4, CSleighbellsCymbFreq4, CSleighbellsCymbReson);
    SetFinalZs(1.0, 0.0, -1.0);
   end
  else if (Inst = 8) then
   begin // Stix1
    rv := Inst;
    FNObjects := CStix1NumBeans;
    FDefObjs[Inst] := CStix1NumBeans;
    SetDecays(CStix1SoundDecay, CStix1SystemDecay);
    FDefDecays[Inst] := CStix1SystemDecay;

    FDecayScale[Inst] := 0.96;
    FNFreqs := 1;
    FBaseGain := CStix1Gain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FFreqalloc[0] := 0;
    SetFreqAndReson(0, CStix1CenterFreq, CStix1Reson);
    SetFinalZs(1.0, 0.0, -1.0);
   end
  else if (Inst = 9) then
   begin // Crunch1
    rv := Inst;
    FNObjects := CCrunch1NumBeads;
    FDefObjs[Inst] := CCrunch1NumBeads;
    SetDecays(CCrunch1SoundDecay, CCrunch1SystemDecay);
    FDefDecays[Inst] := CCrunch1SystemDecay;
    FDecayScale[Inst] := 0.96;
    FNFreqs := 1;
    FBaseGain := CCrunch1Gain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FFreqalloc[0] := 0;
    SetFreqAndReson(0, CCrunch1CenterFreq, CCrunch1Reson);
    SetFinalZs(1.0, -1.0, 0.0);
   end
  else if (Inst = 10) then
   begin // Wrench
    rv := Inst;
    FNObjects := CWrenchNumParts;
    FDefObjs[Inst] := CWrenchNumParts;
    SetDecays(CWrenchSoundDecay, 1.0);
    FDefDecays[Inst] := 0.9999;
    FDecayScale[Inst] := 0.98;
    FNFreqs := 2;
    FBaseGain := CWrenchGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FGains[1] := temp;
    FFreqalloc[0] := 0;
    FFreqalloc[1] := 0;
    FFreqRand[0] := 0.0;
    FFreqRand[1] := 0.0;
    SetFreqAndReson(0, CWrenchFreq, CWrenchReson);
    SetFreqAndReson(1, CWrenchFreq2, CWrenchReson2);
    FRatchet := 0;
    FRatchetPos := 10;
   end
  else if (Inst = 11) then
   begin // Sandpaper
    rv := Inst;
    FNObjects := CSandpaperNumGains;
    FDefObjs[Inst] := CSandpaperNumGains;
    SetDecays(CSandpaperSoundDecay, CSandpaperSystemDecay);
    FDefDecays[Inst] := CSandpaperSystemDecay;
    FDecayScale[Inst] := 0.97;
    FNFreqs := 1;
    FBaseGain := CSandpaperGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FFreqalloc[0] := 0;
    SetFreqAndReson(0, CSandpaperCenterFreq, CSandpaperReson);
    SetFinalZs(1.0, 0.0, -1.0);
   end
  else if (Inst = 12) then
   begin // CokeCan
    rv := Inst;
    FNObjects := CCokeCanNumParts;
    FDefObjs[Inst] := CCokeCanNumParts;
    SetDecays(CCokeCanSoundDecay, CCokeCanSystemDecay);
    FDefDecays[Inst] := CCokeCanSystemDecay;
    FDecayScale[Inst] := 0.95;
    FNFreqs := 5;
    FBaseGain := CCokeCanGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FGains[1] := temp * 1.8;
    FGains[2] := temp * 1.8;
    FGains[3] := temp * 1.8;
    FGains[4] := temp * 1.8;
    FFreqalloc[0] := 0;
    FFreqalloc[1] := 0;
    FFreqalloc[2] := 0;
    FFreqalloc[3] := 0;
    FFreqalloc[4] := 0;
    SetFreqAndReson(0, CCokeCanHelmFreq, CCokeCanHelmRes);
    SetFreqAndReson(1, CCokeCanMetalFreq0, CCokeCanMetalRes);
    SetFreqAndReson(2, CCokeCanMetalFreq1, CCokeCanMetalRes);
    SetFreqAndReson(3, CCokeCanMetalFreq2, CCokeCanMetalRes);
    SetFreqAndReson(4, CCokeCanMetalFreq3, CCokeCanMetalRes);
    SetFinalZs(1.0, 0.0, -1.0);
   end
  else if ((Inst > 12) and (Inst < 20)) then
   begin // Nextmug
    rv := Inst;
    FNObjects := CNextMugNumParts;
    FDefObjs[Inst] := CNextMugNumParts;
    SetDecays(CNextMugSoundDecay, CNextMugSystemDecay);
    FDefDecays[Inst] := CNextMugSystemDecay;
    FDecayScale[Inst] := 0.95;
    FNFreqs := 4;
    FBaseGain := CNextMugGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FGains[1] := temp * 0.8;
    FGains[2] := temp * 0.6;
    FGains[3] := temp * 0.4;
    FFreqalloc[0] := 0;
    FFreqalloc[1] := 0;
    FFreqalloc[2] := 0;
    FFreqalloc[3] := 0;
    FFreqalloc[4] := 0;
    FFreqalloc[5] := 0;
    SetFreqAndReson(0, CNextMugFreq0, CNextMugRes);
    SetFreqAndReson(1, CNextMugFreq1, CNextMugRes);
    SetFreqAndReson(2, CNextMugFreq2, CNextMugRes);
    SetFreqAndReson(3, CNextMugFreq3, CNextMugRes);
    SetFinalZs(1.0, 0.0, -1.0);

    if (Inst = 14) then
     begin // Mug + Penny
      FNFreqs := 7;
      FGains[4] := temp;
      FGains[5] := temp * 0.8;
      FGains[6] := temp * 0.5;
      SetFreqAndReson(4, CPennyFreq0, CPennyRes);
      SetFreqAndReson(5, CPennyFreq1, CPennyRes);
      SetFreqAndReson(6, CPennyFreq2, CPennyRes);
     end
    else if (Inst = 15) then
     begin // Mug + Nickel
      FNFreqs := 6;
      FGains[4] := temp;
      FGains[5] := temp * 0.8;
      FGains[6] := temp * 0.5;
      SetFreqAndReson(4, CNickelFreq0, CNickelRes);
      SetFreqAndReson(5, CNickelFreq1, CNickelRes);
      SetFreqAndReson(6, CNickelFreq2, CNickelRes);
     end
    else if (Inst = 16) then
     begin // Mug + Dime
      FNFreqs := 6;
      FGains[4] := temp;
      FGains[5] := temp * 0.8;
      FGains[6] := temp * 0.5;
      SetFreqAndReson(4, CDimeFreq0, CDimeRes);
      SetFreqAndReson(5, CDimeFreq1, CDimeRes);
      SetFreqAndReson(6, CDimeFreq2, CDimeRes);
     end
    else if (Inst = 17) then
     begin // Mug + Quarter
      FNFreqs := 6;
      FGains[4] := temp * 1.3;
      FGains[5] := temp * 1.0;
      FGains[6] := temp * 0.8;
      SetFreqAndReson(4, CQuarterFreq0, CQuarterRes);
      SetFreqAndReson(5, CQuarterFreq1, CQuarterRes);
      SetFreqAndReson(6, CQuarterFreq2, CQuarterRes);
     end
    else if (Inst = 18) then
     begin // Mug + Franc
      FNFreqs := 6;
      FGains[4] := temp * 0.7;
      FGains[5] := temp * 0.4;
      FGains[6] := temp * 0.3;
      SetFreqAndReson(4, CFrancFreq0, CFrancRes);
      SetFreqAndReson(5, CFrancFreq1, CFrancRes);
      SetFreqAndReson(6, CFrancFreq2, CFrancRes);
     end
    else if (Inst = 19) then
     begin // Mug + Peso
      FNFreqs := 6;
      FGains[4] := temp;
      FGains[5] := temp * 1.2;
      FGains[6] := temp * 0.7;
      SetFreqAndReson(4, CPesoFreq0, CPesoRes);
      SetFreqAndReson(5, CPesoFreq1, CPesoRes);
      SetFreqAndReson(6, CPesoFreq2, CPesoRes);
     end
   end
  else if (Inst = 20) then
   begin // Big Rocks
    FNFreqs := 1;
    rv := Inst;
    FNObjects := CBigGravelNumParts;
    FDefObjs[Inst] := CBigGravelNumParts;
    SetDecays(CBigGravelSoundDecay, CBigGravelSystemDecay);
    FDefDecays[Inst] := CBigGravelSystemDecay;
    FDecayScale[Inst] := 0.95;
    FBaseGain := CBigGravelGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FFreqalloc[0] := 1;
    FFreqRand[0] := 0.11;
    SetFreqAndReson(0, CBigGravelFreq, CBigGravelRes);
    SetFinalZs(1.0, 0.0, -1.0);
   end
  else if (Inst = 21) then
   begin // Little Rocks
    FNFreqs := 1;
    rv := Inst;
    FNObjects := CLittleGravelNumParts;
    FDefObjs[Inst] := CLittleGravelNumParts;
    SetDecays(CLittleGravelSoundDecay, CLittleGravelSystemDecay);
    FDefDecays[Inst] := CLittleGravelSystemDecay;
    FDecayScale[Inst] := 0.95;
    FBaseGain := CLittleGravelGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FFreqalloc[0] := 1;
    FFreqRand[0] := 0.18;
    SetFreqAndReson(0, CLittleGravelFreq, CLittleGravelRes);
    SetFinalZs(1.0, 0.0, -1.0);
   end
  else if (Inst = 22) then
   begin // Tuned Bamboo
    rv := Inst;
    FNObjects := CTunedBambooNumTubes;
    FDefObjs[Inst] := CTunedBambooNumTubes;
    SetDecays(CTunedBambooSoundDecay, CTunedBambooSystemDecay);
    FDefDecays[Inst] := CTunedBambooSystemDecay;
    FDecayScale[Inst] := 0.7;
    FNFreqs := 7;
    FBaseGain := CTunedBambooGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FGains[1] := temp;
    FGains[2] := temp;
    FGains[3] := temp;
    FGains[4] := temp;
    FGains[5] := temp;
    FGains[6] := temp;
    FFreqalloc[0] := 0;
    FFreqalloc[1] := 0;
    FFreqalloc[2] := 0;
    FFreqalloc[3] := 0;
    FFreqalloc[4] := 0;
    FFreqalloc[5] := 0;
    FFreqalloc[6] := 0;
    FFreqRand[0] := 0.0;
    FFreqRand[1] := 0.0;
    FFreqRand[2] := 0.0;
    FFreqRand[3] := 0.0;
    FFreqRand[4] := 0.0;
    FFreqRand[5] := 0.0;
    FFreqRand[6] := 0.0;
    SetFreqAndReson(0, CTunedBambooCenterFreq0, CTunedBambooReson);
    SetFreqAndReson(1, CTunedBambooCenterFreq1, CTunedBambooReson);
    SetFreqAndReson(2, CTunedBambooCenterFreq2, CTunedBambooReson);
    SetFreqAndReson(3, CTunedBambooCenterFreq3, CTunedBambooReson);
    SetFreqAndReson(4, CTunedBambooCenterFreq4, CTunedBambooReson);
    SetFreqAndReson(5, CTunedBambooCenterFreq5, CTunedBambooReson);
    SetFreqAndReson(6, CTunedBambooCenterFreq6, CTunedBambooReson);
    SetFinalZs(1.0, 0.0, -1.0);
   end
  else
   begin // Maracas (Inst = 0) or default
    rv := 0;
    FNObjects := CMaracasNumBeans;
    FDefObjs[0] := CMaracasNumBeans;
    SetDecays(CMaracasSoundDecay, CMaracasSystemDecay);
    FDefDecays[0] := CMaracasSystemDecay;
    FDecayScale[Inst] := 0.9;
    FNFreqs := 1;
    FBaseGain := CMaracasGain;
    temp := log10(FNObjects) * FBaseGain / FNObjects;
    FGains[0] := temp;
    FFreqalloc[0] := 0;
    SetFreqAndReson(0, CMaracasCenterFreq, CMaracasReson);
    SetFinalZs(1.0, -1.0, 0.0);
   end;
  Result := rv;
end;

procedure TStkShakers.NoteOn(const Frequency, Amplitude: Single);
var
  notenum: Integer;
begin
  // Yep ... pretty kludgey, but it works!
//  noteNum:=round((12*log10(frequency/220)/log10(2)) + 57.01) mod 32;
  notenum := round(frequency) mod 23;
  //if (FInstType <>  noteNum) then
  FInstType := SetupNum(noteNum);
  FShakeEnergy := FShakeEnergy + amplitude * CMaxShake * 0.1;
  if (FShakeEnergy > CMaxShake) then
    FShakeEnergy := CMaxShake;
  if ((FInstType = 10) or (FInstType = 3)) then
    FRatchetPos := FRatchetPos + 1;
end;

procedure TStkShakers.noteOff(const Amplitude: Single);
begin
  FShakeEnergy := 0.0;
  if ((FInstType = 10) or (FInstType = 3))
   then FRatchetPos := 0;
end;

const
  MIN_ENERGY = 0.3;

function TStkShakers.tick: Single;
var
  Data, temp_rand: Single;
  i: Integer;
begin
  if (FInstType = 4) then
   begin
    if (FShakeEnergy > MIN_ENERGY) then
     begin
      FLastOutput := WuterTick;
      FLastOutput := lastoutput * 0.0001;
     end
    else
     begin
      FLastOutput := 0.0;
     end;
   end
  else if (FInstType = 22) then
    FLastOutput := TunedBambooTick
  else if ((FInstType = 10) or (FInstType = 3)) then
   begin
    if (FRatchetPos > 0) then
     begin
      FRatchet := FRatchet - (FRatchetDelta + (0.002 * FTotalEnergy));
      if (FRatchet < 0.0) then
       begin
        FRatchet := 1.0;
        FRatchetPos := FRatchetPos - 1;
       end;
      FTotalEnergy := FRatchet;
      FLastOutput := RatchetTick;
      FLastOutput := FLastOutput * 0.0001;
     end
    else
      FLastOutput := 0.0;
   end
  else
  if (FShakeEnergy > MIN_ENERGY) then
   begin
    FShakeEnergy := FShakeEnergy * FSystemDecay;
               // Exponential system decay
    if (random(1024) < FNObjects) then
     begin
      FSndLevel := FSndLevel + FShakeEnergy;
      for i := 0 to FNFreqs - 1 do
        if (FFreqalloc[i] > 0) then
         begin
          temp_rand := FTCenterFreqs[i] * (1.0 + (FFreqRand[i] * NoiseTick));
          FCoeffs[i][0] := -FResons[i] * 2.0 * cos(temp_rand * 2 * Pi * FSampleRateInv);
         end;
     end;
    FInputs[0] := FSndLevel * NoiseTick;      // Actual Sound is Random
    for i := 1 to FNFreqs - 1 do
      FInputs[i] := FInputs[0];
    FSndLevel := FSndLevel * FSoundDecay;     // Exponential Sound decay
    FFinalZ[2] := FFinalZ[1];
    FFinalZ[1] := FFinalZ[0];
    FFinalZ[0] := 0;
    for i := 0 to FNFreqs - 1 do
     begin
      FInputs[i] := FInputs[i] - FOutputs[i][0] * FCoeffs[i][0];  // Do
      FInputs[i] := FInputs[i] - FOutputs[i][1] * FCoeffs[i][1];  // resonant
      FOutputs[i][1] := FOutputs[i][0];            // filter
      FOutputs[i][0] := FInputs[i];                // calculations
      FFinalZ[0] := FFinalZ[0] + FGains[i] * FOutputs[i][1];
     end;
    Data := FFinalZCoeffs[0] * FFinalZ[0];     // Extra zero(s) for shape
    Data := Data + FFinalZCoeffs[1] * FFinalZ[1];    // Extra zero(s) for shape
    Data := Data + FFinalZCoeffs[2] * FFinalZ[2];    // Extra zero(s) for shape
    if (Data > 10000.0) then
      Data := 10000.0;
    if (Data < -10000.0) then
      Data := -10000.0;
    FLastOutput := Data * 0.0001;
   end
  else
    FLastOutput := 0.0;//  Single generic_tick  begin

  Result := FLastOutput;
end;

procedure TStkShakers.ControlChange(const Number: Integer; const Value: Single);
var
  temp, norm: Single;
  i: Integer;
begin
  norm := Limit(Value, 0, 1);

  if (number = CMidiBreath) then
   begin // 2 ... energy
    FShakeEnergy := FShakeEnergy + norm * CMaxShake * 0.1;
    if (FShakeEnergy > CMaxShake) then
      FShakeEnergy := CMaxShake;
    if ((FInstType = 10) or (FInstType = 3)) then
     begin
      FRatchetPos := round(abs(Value - FLastRatchetPos));
      FRatchetDelta := 0.0002 * FRatchetPos;
      FLastRatchetPos := round(Value);
     end;
   end
  else if (number = CMidiModFrequency) then
   begin // 4 ... decay
    if ((FInstType <> 3) and (FInstType <> 10)) then
     begin
      FSystemDecay := FDefDecays[FInstType] + ((Value - 64.0) *
        FDecayScale[FInstType] *
        (1.0 -
        FDefDecays[FInstType]) / 64.0);
      FGains[0] := log10(FNObjects) * FBaseGain / FNObjects;
      for i := 1 to FNFreqs - 1 do
        FGains[i] := FGains[0];
      if (FInstType = 6) then
       begin // Tambourine
        FGains[0] := FGains[0] * CTambourineShellGain;
        FGains[1] := FGains[1] * 0.8;
       end
      else if (FInstType = 7) then
       begin // sleighbell
        FGains[3] := FGains[3] * 0.5;
        FGains[4] := FGains[4] * 0.3;
       end
      else if (FInstType = 12) then
        for i := 1 to FNFreqs - 1 do
          FGains[i] := FGains[i] * 1.8// cokecan
      ;
      for i := 0 to FNFreqs - 1 do
        FGains[i] := FGains[i] * ((128 - Value) / 100.0 + 0.36);
     end;
   end
  else if (number = CMidiFootControl) then
   begin // 11 ... number of objects
    if (FInstType = 5) then // CBamboooo
      FNObjects := (Value * FDefObjs[FInstType] / 64.0) + 0.3
    else
      FNObjects := (Value * FDefObjs[FInstType] / 64.0) + 1.1;
    FGains[0] := log10(FNObjects) * FBaseGain / FNObjects;
    for i := 1 to FNFreqs - 1 do
      FGains[i] := FGains[0];
    if (FInstType = 6) then
     begin // Tambourine
      FGains[0] := FGains[0] * CTambourineShellGain;
      FGains[1] := FGains[1] * 0.8;
     end
    else if (FInstType = 7) then
     begin // sleighbell
      FGains[3] := FGains[3] * 0.5;
      FGains[4] := FGains[4] * 0.3;
     end
    else if (FInstType = 12) then
      for i := 1 to FNFreqs - 1 do
        FGains[i] := FGains[i] * 1.8;// cokecan
    if ((FInstType <> 3) and (FInstType <> 10)) then
     begin
    // reverse calculate decay setting
      temp := (64.0 * (FSystemDecay - FDefDecays[FInstType]) /
        (FDecayScale[FInstType] * (1 - FDefDecays[FInstType])) + 64.0);
    // scale gains by decay setting
      for i := 0 to FNFreqs - 1 do
        FGains[i] := FGains[i] * ((128 - temp) / 100.0 + 0.36);
     end;
   end
  else if (number = CMidiModWheel) then
   begin // 1 ... resonance frequency
    for i := 0 to FNFreqs - 1 do
     begin
      if ((FInstType = 6) or (FInstType = 2) or (FInstType = 7)) then
 // limit range a bit for Tambourine
        temp := FCenterFreqs[i] * power(1.008, Value - 64)
      else
        temp := FCenterFreqs[i] * power(1.015, Value - 64);
      FTCenterFreqs[i] := temp;

      FCoeffs[i][0] := -FResons[i] * 2.0 * cos(temp * 2 * Pi / SampleRate);
      FCoeffs[i][1] := FResons[i] * FResons[i];
     end;
   end
  else if (number = CMidiAfterTouchCont) then
   begin // 128
    FShakeEnergy := FShakeEnergy + norm * CMaxShake * 0.1;
    if (FShakeEnergy > CMaxShake) then
      FShakeEnergy := CMaxShake;
    if ((FInstType = 10) or (FInstType = 3)) then
     begin
      FRatchetPos := round(abs(Value - FLastRatchetPos));
      FRatchetDelta := 0.0002 * FRatchetPos;
      FLastRatchetPos := round(Value);
     end;
   end
  else if (number = CMidiShakerInst) then
   begin // 1071
    FInstType := round(norm * 22);  //  Just to be safe
    SetupNum(FInstType);
   end;
end;

// KLUDGE-O-MATIC-O-RAMA

function TStkShakers.WuterTick: Single;
var
  Data: Single;
  j: Integer;
begin
  FShakeEnergy := FShakeEnergy * FSystemDecay;
               // Exponential system decay
  if (random(32767) < FNObjects) then
   begin
    FSndLevel := FShakeEnergy;
    j := random(3);
    if (j = 0) then
     begin
      FCenterFreqs[0] := CWaterDropsCenterFreq1 * (0.75 + (0.25 * NoiseTick));
      FGains[0] := abs(NoiseTick);
     end
    else if (j = 1) then
     begin
      FCenterFreqs[1] := CWaterDropsCenterFreq1 * (1.0 + (0.25 * NoiseTick));
      FGains[1] := abs(NoiseTick);
     end
    else
     begin
      FCenterFreqs[2] := CWaterDropsCenterFreq1 * (1.25 + (0.25 * NoiseTick));
      FGains[2] := abs(NoiseTick);
     end;
   end;

  FGains[0] := FGains[0] * FResons[0];
  if (FGains[0] > 0.001) then
   begin
    FCenterFreqs[0] := FCenterFreqs[0] * CWaterDropsFreqSweep;
    FCoeffs[0][0] := -FResons[0] * 2.0 * cos(FCenterFreqs[0] *
      2 * Pi / SampleRate);
   end;
  FGains[1] := FGains[1] * FResons[1];
  if (FGains[1] > 0.001) then
   begin
    FCenterFreqs[1] := FCenterFreqs[1] * CWaterDropsFreqSweep;
    FCoeffs[1][0] := -FResons[1] * 2.0 * cos(FCenterFreqs[1] *
      2 * Pi / SampleRate);
   end;
  FGains[2] := FGains[2] * FResons[2];
  if (FGains[2] > 0.001) then
   begin
    FCenterFreqs[2] := FCenterFreqs[2] * CWaterDropsFreqSweep;
    FCoeffs[2][0] := -FResons[2] * 2.0 * cos(FCenterFreqs[2] *
      2 * Pi / SampleRate);
   end;

  FSndLevel := FSndLevel * FSoundDecay;        // Each (all) event(s) 
                                 // decay(s) exponentially 
  FInputs[0] := FSndLevel;
  FInputs[0] := FInputs[0] * NoiseTick;     // Actual Sound is Random
  FInputs[1] := FInputs[0] * FGains[1];
  FInputs[2] := FInputs[0] * FGains[2];
  FInputs[0] := FInputs[0] * FGains[0];
  FInputs[0] := FInputs[0] - FOutputs[0][0] * FCoeffs[0][0];
  FInputs[0] := FInputs[0] - FOutputs[0][1] * FCoeffs[0][1];
  FOutputs[0][1] := FOutputs[0][0];
  FOutputs[0][0] := FInputs[0];
  Data := FGains[0] * FOutputs[0][0];
  FInputs[1] := FInputs[1] - FOutputs[1][0] * FCoeffs[1][0];
  FInputs[1] := FInputs[1] - FOutputs[1][1] * FCoeffs[1][1];
  FOutputs[1][1] := FOutputs[1][0];
  FOutputs[1][0] := FInputs[1];
  Data := Data + FGains[1] * FOutputs[1][0];
  FInputs[2] := FInputs[2] - FOutputs[2][0] * FCoeffs[2][0];
  FInputs[2] := FInputs[2] - FOutputs[2][1] * FCoeffs[2][1];
  FOutputs[2][1] := FOutputs[2][0];
  FOutputs[2][0] := FInputs[2];
  Data := Data + FGains[2] * FOutputs[2][0];

  FFinalZ[2] := FFinalZ[1];
  FFinalZ[1] := FFinalZ[0];
  FFinalZ[0] := Data * 4;

  Data := FFinalZ[2] - FFinalZ[0];
  Result := Data;
end;

function TStkShakers.RatchetTick: Single;
var
  Data: Single;
begin
  if (random(1024) < FNObjects) then
    FSndLevel := FSndLevel + 512 * FRatchet * FTotalEnergy;
  FInputs[0] := FSndLevel;
  FInputs[0] := FInputs[0] * NoiseTick * FRatchet;
  FSndLevel := FSndLevel * FSoundDecay;

  FInputs[1] := FInputs[0];
  FInputs[0] := FInputs[0] - FOutputs[0][0] * FCoeffs[0][0];
  FInputs[0] := FInputs[0] - FOutputs[0][1] * FCoeffs[0][1];
  FOutputs[0][1] := FOutputs[0][0];
  FOutputs[0][0] := FInputs[0];
  FInputs[1] := FInputs[1] - FOutputs[1][0] * FCoeffs[1][0];
  FInputs[1] := FInputs[1] - FOutputs[1][1] * FCoeffs[1][1];
  FOutputs[1][1] := FOutputs[1][0];
  FOutputs[1][0] := FInputs[1];

  FFinalZ[2] := FFinalZ[1];
  FFinalZ[1] := FFinalZ[0];
  FFinalZ[0] := FGains[0] * FOutputs[0][1] + FGains[1] * FOutputs[1][1];
  Data := FFinalZ[0] - FFinalZ[2];
  Result := Data;
end;

function TStkShakers.TunedBambooTick: Single;
var
  Data, temp: Single;
  Which, i: Integer;
begin
 Which := 0;
 if (FShakeEnergy > MIN_ENERGY) then
  begin
   FShakeEnergy := FShakeEnergy * FSystemDecay;    // Exponential system decay
   if (random(1024) < FNObjects) then
    begin
     FSndLevel := FSndLevel + FShakeEnergy;
     Which := random(7);
    end;
   temp := FSndLevel * NoiseTick;      // Actual Sound is Random
   for i := 0 to FNFreqs - 1 do
     FInputs[i] := 0;
   FInputs[Which mod 7] := temp;
   FSndLevel := FSndLevel * FSoundDecay;
                  // Exponential Sound decay
   FFinalZ[2] := FFinalZ[1];
   FFinalZ[1] := FFinalZ[0];
   FFinalZ[0] := 0;
   for i := 0 to FNFreqs - 1 do
    begin
     FInputs[i] := FInputs[i] - FOutputs[i][0] * FCoeffs[i][0];  // Do
     FInputs[i] := FInputs[i] - FOutputs[i][1] * FCoeffs[i][1];  // resonant
     FOutputs[i][1] := FOutputs[i][0];            // filter
     FOutputs[i][0] := FInputs[i];                // calculations
     FFinalZ[0] := FFinalZ[0] + FGains[i] * FOutputs[i][1];
    end;
   Data := FFinalZCoeffs[0] * FFinalZ[0];     // Extra zero(s) for shape
   Data := Data + FFinalZCoeffs[1] * FFinalZ[1];    // Extra zero(s) for shape
   Data := Data + FFinalZCoeffs[2] * FFinalZ[2];    // Extra zero(s) for shape
   if (Data > 10000.0) then
     Data := 10000.0
   else
   if (Data < -10000.0) then
     Data := -10000.0;
   Data := Data * 0.0001;
  end
 else
   Data := 0.0;
 Result := Data;
end;

end.

