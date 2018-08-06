unit UIXSynthModel;

interface

uses XOscillator;

type IXSynthModel = interface
    procedure OnVoiceDone(Sender: TObject);
    function GetGlide:single;
    function GetOscFrequency(osc:integer;pitch:single):single;
    function GetLFOValue(lfo:integer):single;
    procedure GetLFODelay(lfo:integer;VAR enabled: boolean;VAR rate:single);
    function GetOscPulseWidth(osc:integer):single;
    function GetOscModDepth(osc,lfo:integer):single;
    function GetVCFModDepth(lfo:integer):single;
    function GetVCAModDepth(lfo:integer):single;
    function GetOscLevel(osc:integer):single;
    function GetRingLevel:single;
    procedure GetConvertedVCA(VAR A,D,S,R,gain:single);
    procedure GetConvertedVCF(var A, D, S, R, gain: single);
    function GetOscWaveshape(osc:integer;VAR noisecolor:integer):TWaveShape;
    function GetCutoff(pitch:single):single;
    function GetResonance:single;
    function GetLevel:single;
    function GetSync(osc:integer):boolean;
    function GetLFOSpeed(lfo:integer):single;
    function GetLFOWaveshape(lfo: integer): TWaveShape;

end;

implementation

end.
