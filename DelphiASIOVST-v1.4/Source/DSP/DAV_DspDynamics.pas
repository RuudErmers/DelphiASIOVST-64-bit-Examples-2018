unit DAV_DspDynamics;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_Classes, DAV_DspFilterButterworth;

type
  {$IFDEF DELPHI7_UP}
  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  IDynamicProcessorKnee                                                   //
  //  ---------------------                                                   //
  //                                                                          //
  //  Interface used to grant access to all classes that features the knee    //
  //  property. (not in use yet!)                                             //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  IDynamicProcessorKnee = interface(IInterface)
    function GetKnee_dB: Double;
    procedure SetKnee_dB(Value: Double);
    property Knee_dB: Double read GetKnee_dB write SetKnee_dB;
  end;
  {$ENDIF}

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomDynamicProcessor                                                 //
  //  -----------------------                                                 //
  //                                                                          //
  //  Base class for all dynamic processors. It features a pure abstract      //
  //  sample process function which is reserved for the sample based DSP      //
  //  processing.                                                             //
  //  Also an interface for calculation of the characteristic curve is        //
  //  provided. The user needs to fill the TranslatePeakToGain with live in   //
  //  order to use the functionallity properly. For feedback compressor       //
  //  designs the CharacteristicCurve function must be overriden as well.     //
  //                                                                          //
  //  Also the threshold property is located in this base class, since it is  //
  //  common for all dynamics processors.                                     //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomDynamicProcessor = class(TDspPersistent, IDspProcessor32, IDspProcessor64)
  private
    function GetKnee_dB: Double;
    procedure SetThreshold(const Value: Double);
    procedure SetKnee_dB(Value: Double);
  protected
    FGain         : Double;
    FThreshold    : Double;
    FThreshold_dB : Double;
    FKnee_dB      : Double;
    procedure CalculateThreshold; virtual;
    procedure KneeChanged; virtual;
    procedure ThresholdChanged; virtual;
  public
    constructor Create; virtual;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); virtual;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); virtual;
    function ProcessSample32(Input: Single): Single; virtual;
    function ProcessSample64(Input: Double): Double; virtual; abstract;
    function GainSample(const Input: Double): Double; virtual;
    procedure InputSample(const Input: Double); virtual; abstract;
    function TranslatePeakToGain(const PeakLevel: Double): Double; virtual; abstract;
    function CharacteristicCurve(const InputLevel: Double): Double; virtual;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; virtual;

    property Threshold_dB: Double read FThreshold_dB write SetThreshold;  // in dB
    property Knee_dB: Double read GetKnee_dB write SetKnee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSimpleDirectGate                                                       //
  //  -----------------                                                       //
  //                                                                          //
  //  Very basic gate that removes anything below the given threshold.        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSimpleDirectGate = class(TCustomDynamicProcessor)
  protected
    // hidden properties
    property Knee_dB;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
  published
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSoftDirectGate                                                         //
  //  ---------------                                                         //
  //                                                                          //
  //  Very basic gate that removes anything softly below the given threshold. //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSoftDirectGate = class(TCustomDynamicProcessor)
  private
    procedure CalculateKneedThreshold;
  protected
    FSoftKnee       : array [0..1] of Double;
    FKneedThreshold : Double;
    procedure SetSoftKnee_dB(const Value: Double);
    procedure CalculateSoftKnee;
    procedure ThresholdChanged; override;
    procedure KneeChanged; override;
  public
    constructor Create; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;

    procedure InputSample(const Input: Double); override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property Threshold_dB;
    property Knee_dB;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomBrickwallLimiter                                                 //
  //  -----------------------                                                 //
  //                                                                          //
  //  Base class for brickwall limiters. Properties introduced here are:      //
  //  AutoMakeUp and MakeUpGain_dB. Also the reciprocal of the threshold is   //
  //  calculated.                                                             //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomBrickwallLimiter = class(TCustomDynamicProcessor)
  private
    FAutoMakeUp    : Boolean;
    procedure SetAutoMakeUp(const Value: Boolean);
    procedure SetMakeUpGain_dB(const Value: Double);
  protected
    FThresholdReciprocal : Double;
    FMakeUpGain          : Double;
    FMakeUpGain_dB       : Double;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; virtual;
    procedure MakeUpGainChanged; virtual;

    // hidden properties
    property Knee_dB;
  public
    constructor Create; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;

    procedure InputSample(const Input: Double); override;

    property AutoMakeUp : Boolean read FAutoMakeUp write SetAutoMakeUp;
    property MakeUpGain_dB : Double read FMakeUpGain_dB write SetMakeUpGain_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TBrickwallLimiter                                                       //
  //  -----------------                                                       //
  //                                                                          //
  //  Very basic limiter that simply clips the samples that lie beyond the    //
  //  given threshold.                                                        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TBrickwallLimiter = class(TCustomBrickwallLimiter)
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function CharacteristicCurve(const InputLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSoftBrickwallLimiter                                                   //
  //  ---------------------                                                   //
  //                                                                          //
  //  Soft limiter that uses a tanh(x) waveshaper to clip the input signal    //
  //  beyond a given threshold. The shape can be adjusted using the knee      //
  //  control.                                                                //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSoftBrickwallLimiter = class(TCustomBrickwallLimiter)
  private
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee;
  protected
    procedure KneeChanged; override;
  public
    constructor Create; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Knee_dB;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSimpleSoftBrickwallLimiter                                             //
  //  ---------------------------                                             //
  //                                                                          //
  //  Soft limiter that uses a simple, but common formula to control the      //
  //  waveshape. It features a knee property that can controls the dip around //
  //  the threshold in dB.                                                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSimpleSoftBrickwallLimiter = class(TCustomBrickwallLimiter)
  private
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee;
  protected
    procedure KneeChanged; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
    property Knee_dB;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomTimeConstantDynamics                                             //
  //  ---------------------------                                             //
  //                                                                          //
  //  Time constans and thus a time variant behaviour is introduced with this //
  //  base class. Also the sample rate needs to be known here.                //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomTimeConstantDynamics = class(TCustomDynamicProcessor)
  private
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
    procedure SetSampleRate(const Value: Double);
    function GetGainReductiondB: Double;
  protected
    FPeak          : Double;
    FLevel         : Double;
    FSampleRate    : Double;
    FSampleRateRez : Double;
    FRelease       : Double;
    FAttack        : Double;
    FReleaseFactor : Double;
    FAttackFactor  : Double;
    procedure SampleRateChanged; virtual;
    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;

    // hidden properties
    property Knee_dB;
  public
    constructor Create; override;
    procedure Reset; virtual;

    property Attack: Double read FAttack write SetAttack;             // in ms
    property Release: Double read FRelease write SetRelease;          // in ms
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property GainReductionFactor: Double read FGain;                  // in dB
    property GainReductiondB: Double read GetGainReductiondB;         // in dB
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomLimiter                                                          //
  //  --------------                                                          //
  //                                                                          //
  //  Base class for time variant limiters. Properties introduced here are:   //
  //  AutoMakeUp and MakeUpGain_dB. Also the reciprocal of the threshold is   //
  //  calculated.                                                             //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomLimiter = class(TCustomTimeConstantDynamics)
  private
    FAutoMakeUp    : Boolean;
    procedure SetAutoMakeUp(const Value: Boolean);
    procedure SetMakeUpGain_dB(const Value: Double);
  protected
    FThresholdReciprocal : Double;
    FMakeUpGain          : Double;
    FMakeUpGain_dB       : Double;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; virtual;
    procedure MakeUpGainChanged; virtual;
  public
    constructor Create; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    procedure InputSample(const Input: Double); override;

    property AutoMakeUp : Boolean read FAutoMakeUp write SetAutoMakeUp;
    property MakeUpGain_dB : Double read FMakeUpGain_dB write SetMakeUpGain_dB;
    property MakeUpGain: Double read FMakeUpGain;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TLimiter                                                                //
  //  --------                                                                //
  //                                                                          //
  //  Very basic hard knee limiter.                                           //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TLimiter = class(TCustomLimiter)
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function CharacteristicCurve(const InputLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
  end;

  TRCLimiter = class(TCustomLimiter)
  protected
    procedure ReleaseChanged; override;
    procedure AttackChanged; override;
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function CharacteristicCurve(const InputLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSoftKneeLimiter                                                        //
  //  ----------------                                                        //
  //                                                                          //
  //  Soft limiter that uses a tanh(x) waveshaper to clip the input signal    //
  //  beyond a given threshold. The shape can be adjusted using the knee      //
  //  control.                                                                //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomKneeLimiter = class(TCustomLimiter)
  protected
    procedure KneeChanged; override;
  public
    property Knee_dB;
  end;

  TSoftKneeLimiter = class(TCustomKneeLimiter)
  private
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee;
  protected
    procedure KneeChanged; override;
  public
    constructor Create; override;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
    property Knee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSimpleSoftKneeLimiter                                                  //
  //  ----------------------                                                  //
  //                                                                          //
  //  Soft limiter that uses a simple, but common formula to control the      //
  //  waveshape. It features a knee property that can controls the dip around //
  //  the threshold in dB.                                                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSimpleSoftKneeLimiter = class(TCustomKneeLimiter)
  private
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee;
  protected
    procedure KneeChanged; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
    property Knee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomClassicGate                                                      //
  //  ------------------                                                      //
  //                                                                          //
  //  This base class encapsulates a straight forward classic gate            //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomClassicGate = class(TCustomTimeConstantDynamics)
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
  end;

  TClassicGate = class(TCustomClassicGate)
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure InputSample(const Input : Double); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
  published
    property Attack;
    property Release;
    property SampleRate;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomClassicRangeGate                                                 //
  //  -----------------------                                                 //
  //                                                                          //
  //  This base class encapsulates a gate that allows to attenuate the        //
  //  signal below the given threshold.                                       //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomClassicRangeGate = class(TCustomClassicGate)
  private
    procedure SetRange_dB(const Value: Double);
  protected
    FRange_dB : Double;
    FRange    : Double;
    procedure CalculateRange; virtual;
  public
    constructor Create; override;
    property Range_dB: Double read FRange_dB write SetRange_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TClassicSoftRangeGate                                                   //
  //  ---------------------                                                   //
  //                                                                          //
  //  An implementation of a RangeGate with soften edges.                     //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TClassicSoftRangeGate = class(TCustomClassicRangeGate)
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure InputSample(const Input : Double); override;
  published
    property Attack;
    property Release;
    property SampleRate;
    property Range_dB;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftKneeGate                                                     //
  //  -------------------                                                     //
  //                                                                          //
  //  The base class for all soft knee gates. Features a soft knee property.  //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftKneeGate = class(TCustomClassicGate)
  protected
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee; virtual;
    procedure KneeChanged; override;
  public
    constructor Create; override;
    property Knee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TClassicSoftKneeGate                                                    //
  //  --------------------                                                    //
  //                                                                          //
  //  The basic implementation of a classic soft knee.                        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TClassicSoftKneeGate = class(TCustomSoftKneeGate)
  private
    procedure CalculateKneedThreshold;
  protected
    FKneedThreshold : Double;
    procedure CalculateSoftKnee; override;
    procedure ThresholdChanged; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure InputSample(const Input : Double); override;
  published
    property Attack;
    property Release;
    property SampleRate;
    property Knee_dB;
    property Threshold_dB;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomTimeConstantRatioDynamics                                        //
  //  --------------------------------                                        //
  //                                                                          //
  //  This base class introduces a ratio property and calculate its           //
  //  reciprocal value. Furthermore the reciprocal of the Threshold is        //
  //  calculated.                                                             //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomTimeConstantRatioDynamics = class(TCustomTimeConstantDynamics)
  private
    procedure SetRatio(const Value: Double);
    procedure CalculateInverseRatio;
  protected
    FRatio               : Double;
    FRatioReciprocal     : Double;
    FThresholdReciprocal : Double;
    procedure RatioChanged; virtual;
    procedure ThresholdChanged; override;
  public
    constructor Create; override;

    property Ratio: Double read FRatio write SetRatio;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomGate                                                             //
  //  -----------                                                             //
  //                                                                          //
  //  The base class for all full featured gates. Includes a Hold property,   //
  //  a Range property and a Knee property.                                   //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomGate = class(TCustomTimeConstantRatioDynamics)
  private
    procedure CalculateSoftKnee;
    procedure SetHold(Value: Double);
    procedure SetRange_dB(const Value: Double);
  protected
    FHold        : Double;
    FSoftKnee    : array [0..1] of Double;
    FRange_dB    : Double;
    FRangeFactor : Double;
    FHoldSmplCnt : Integer;
    FHoldSamples : Double;
    procedure CalculateHoldSamples;
    procedure CalculateRangeFactor;
    procedure KneeChanged; override;
    procedure RangeChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    procedure InputSample(const Input: Double); override;

    property Hold: Double read FHold write SetHold;              // in s
    property Range_dB: Double read FRange_dB write SetRange_dB;  // in dB
    property Knee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TAdvancedGate                                                           //
  //  -------------                                                           //
  //                                                                          //
  //  This class features several additional properties necessary for a full  //
  //  featured gate.                                                          //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TAdvancedGate = class(TCustomGate)
  private
    FThresholdKneeFactor  : Double;
    FRangeThresholdFactor : Double;
    function GetHighCut: Double;
    function GetLowCut: Double;
    procedure CalculateRangeThresholdFactor;
    procedure CalculateThresholdKneeFactor;
    procedure SetHighCut(const Value: Double);
    procedure SetLowCut(const Value: Double);
  protected
    FSideChain : Double;
    FDuck      : Boolean;
    FLowCut    : TButterworthHighpassFilter;
    FHighCut   : TButterworthLowpassFilter;
    procedure SampleRateChanged; override;
    procedure ThresholdChanged; override;
    procedure RangeChanged; override;
    procedure KneeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure InputSample(const Input: Double); override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property Duck: Boolean read FDuck write FDuck;       // not implemented yet
    property SideChainLowCut : Double read GetLowCut write SetLowCut;     // in Hz
    property SideChainHighCut : Double read GetHighCut write SetHighCut;  // in Hz
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomCompressor                                                       //
  //  -----------------                                                       //
  //                                                                          //
  //  The base class for all compressor implementations.                      //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomCompressor = class(TCustomTimeConstantRatioDynamics)
  private
    procedure SetAutoMakeUp(const Value: Boolean);
    procedure SetMakeUpGain_dB(const Value: Double);
  protected
    FAutoMakeUp    : Boolean;
    FMakeUpGain_dB : Double;
    FMakeUpGain    : Double;
    procedure RatioChanged; override;
    procedure ThresholdChanged; override;
    procedure CalculateMakeUpGain; virtual;
    procedure AutoMakeUpChanged; virtual;
    procedure MakeUpGainChanged; virtual;
  public
    constructor Create; override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input : Double): Double; override;
    procedure InputSample(const Input: Double); override;

    property AutoMakeUp : Boolean read FAutoMakeUp write SetAutoMakeUp;
    property MakeUpGain_dB : Double read FMakeUpGain_dB write SetMakeUpGain_dB; //in dB
    property MakeUpGain : Double read FMakeUpGain;
  end;

  TSimpleCompressor = class(TCustomCompressor)
  protected
    FMakeUpGain    : TDAV2DoubleArray;
    procedure CalculateMakeUpGain; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  end;

  TCustomKneeCompressor = class(TCustomCompressor)
  protected
    procedure KneeChanged; override;
  public
    property Knee_dB;
  end;

  TSoftKneeCompressor = class(TCustomKneeCompressor)
  protected
    FMakeUpGain : TDAV2DoubleArray;
    procedure CalculateMakeUpGain; override;
    procedure ThresholdChanged; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property Knee_dB;
  end;

  TCustomFeedbackCompressor = class(TCustomCompressor)
  protected
    FPreviousAbsSample : Double;
  public
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  end;

  TSimpleFeedbackCompressor = class(TCustomFeedbackCompressor)
  protected
    FMakeUpGains    : TDAV2DoubleArray;
    procedure RatioChanged; override;
    procedure CalculateMakeUpGain; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function CharacteristicCurve(const InputLevel: Double): Double; override;
  end;

  TSoftKneeFeedbackCompressor = class(TSimpleFeedbackCompressor)
  public
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input : Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  end;

  TSimpleRMSCompressor = class(TSimpleCompressor)
  private
    FRMSTime    : Double;
    procedure SetRMSTime(const Value: Double);
    procedure UpdateRMSBuffer;
  protected
    FRMSSize    : Integer;
    FRMSPos     : Integer;
    FRMSFactor  : Double;
    FRMSBuffer  : TDAVDoubleDynArray;
    FCurrentRMS : Double;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    procedure InputSample(const Input: Double); override;
  published
    property RMSTime : Double read FRMSTime write SetRMSTime;  // in ms
  end;

  TCompressor = class(TSimpleRMSCompressor)
  private
    function GetHighCut: Double;
    function GetLowCut: Double;
    procedure SetHighCut(const Value: Double);
    procedure SetLowCut(const Value: Double);
  protected
    FLowCut  : TButterworthHighpassFilter;
    FHighCut : TButterworthLowpassFilter;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure InputSample(const Input : Double); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property SideChainLowCut : Double read GetLowCut write SetLowCut;     // in Hz
    property SideChainHighCut : Double read GetHighCut write SetHighCut;  // in Hz
  end;

(*
  TSoftKneeFeedbackLimiter = class(TSoftKneeLimiter)
  protected
    FOversample : Integer;
    FFilter     : TButterworthLowCut;
    FAttackFac2 : Double;
    FReleaseFac2  : Double;
    FPeak2      : Double;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
    procedure SampleRateChanged; override;
  public
    function ProcessSample64(Input : Double):Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    constructor Create; override;
  end;
*)

implementation

uses
  SysUtils, Math, DAV_Common, DAV_Math, DAV_Approximations;

{$IFDEF HandleDenormals}
var
  CDenorm64      : Double;
  CDenorm32      : Single;
{$ENDIF}

(*
var
  CHalf64        : Double;
  CQuarter64     : Double;
  CHalf32        : Single;
  CQuarter32     : Single;
*)

const
  CSoftKnee : array [0..7] of Single = (-8.21343513178931783E-2,
    6.49732456739820052E-1, -2.13417801862571777, 4.08642207062728868,
    -1.51984215742349793, 5.48668824216034384E-2, 2.42162975514835621E-1,
    6.93292707161004662E-1);

{ TCustomDynamicProcessor }

constructor TCustomDynamicProcessor.Create;
begin
  inherited;
  FThreshold_dB := -40;
  FThreshold    := dB_to_Amp(FThreshold_dB);
  FKnee_dB      := 0;
  FGain         := 1;
end;

procedure TCustomDynamicProcessor.CalculateThreshold;
begin
  FThreshold := dB_to_Amp(FThreshold_dB);
end;

function TCustomDynamicProcessor.CharacteristicCurve(const InputLevel: Double): Double;
begin
 Result := TranslatePeakToGain(abs(InputLevel)) * InputLevel;
end;

function TCustomDynamicProcessor.CharacteristicCurve_dB(const InputLevel_dB: Double): Double;
begin
 Result := Amp_to_dB({$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
   CharacteristicCurve(dB_to_Amp(InputLevel_dB)));
end;

function TCustomDynamicProcessor.GainSample(const Input: Double): Double;
begin
 Result := FGain * Input;
end;

function TCustomDynamicProcessor.GetKnee_dB: Double;
begin
 Result := FKnee_dB;
end;

procedure TCustomDynamicProcessor.KneeChanged;
begin
 raise Exception.Create('The knee property is not in use in this class');
end;

procedure TCustomDynamicProcessor.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TCustomDynamicProcessor.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TCustomDynamicProcessor.ProcessSample32(Input: Single): Single;
begin
 Result := ProcessSample64(Input);
end;

procedure TCustomDynamicProcessor.SetKnee_dB(Value: Double);
begin
 if Value < 0 then Value := 0;
 if FKnee_dB <> Value then
  begin
   FKnee_dB := Value;
   KneeChanged;
  end;
end;

procedure TCustomDynamicProcessor.SetThreshold(const Value: Double);
begin
 if FThreshold_dB <> Value then
  begin
   FThreshold_dB := Value;
   ThresholdChanged;
  end;
end;

procedure TCustomDynamicProcessor.ThresholdChanged;
begin
 CalculateThreshold;
 Changed;
end;

{ TSimpleDirectGate }

function TSimpleDirectGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
  if PeakLevel < FThreshold
   then Result := 0
   else Result := 1;
end;

procedure TSimpleDirectGate.InputSample(const Input: Double);
begin
 FGain := TranslatePeakToGain(abs(Input));
end;

function TSimpleDirectGate.ProcessSample32(Input: Single): Single;
begin
 if abs(Input) < FThreshold
  then Result := 0
  else Result := Input;
end;

function TSimpleDirectGate.ProcessSample64(Input: Double): Double;
begin
 if abs(Input) < FThreshold
  then Result := 0
  else Result := Input;
end;

{ TSoftDirectGate }

constructor TSoftDirectGate.Create;
begin
 inherited;
 FKnee_dB := 1;
 CalculateSoftKnee;
end;

procedure TSoftDirectGate.KneeChanged;
begin
 CalculateSoftKnee;
 Changed;
end;

procedure TSoftDirectGate.CalculateSoftKnee;
const
  CdBScale : Double = 0.16609640474436811739351597147447;
begin
 FSoftKnee[0] := FKnee_dB * CdBScale;
 FSoftKnee[1] := 1 / FSoftKnee[0];
 CalculateKneedThreshold;
end;

procedure TSoftDirectGate.SetSoftKnee_dB(const Value: Double);
begin
 if FKnee_dB <> Value then
  begin
   FKnee_dB := Value;
   CalculateSoftKnee;
  end;
end;

procedure TSoftDirectGate.ThresholdChanged;
begin
 inherited;
 CalculateKneedThreshold;
 Changed;
end;

procedure TSoftDirectGate.CalculateKneedThreshold;
begin
 FKneedThreshold := Power(FThreshold, FSoftKnee[1]);
end;

procedure TSoftDirectGate.InputSample(const Input: Double);
begin
 FGain := TranslatePeakToGain(abs(Input));
end;

function TSoftDirectGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 Result := Power((PeakLevel * Power(Power(PeakLevel, FSoftKnee[1]) + FKneedThreshold, -FSoftKnee[0])), 1 / ((abs(PeakLevel - FThreshold) + (FSoftKnee[0] + PeakLevel - FThreshold))));
end;

procedure TSoftDirectGate.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := GainSample(Data[Sample]);
  end;
end;

procedure TSoftDirectGate.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := GainSample(Data[Sample]);
  end;
end;

function TSoftDirectGate.ProcessSample32(Input: Single): Single;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

function TSoftDirectGate.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

{ TCustomBrickwallLimiter }

constructor TCustomBrickwallLimiter.Create;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 FMakeUpGain          := 1;
 FMakeUpGain_dB       := 0;
end;

procedure TCustomBrickwallLimiter.AutoMakeUpChanged;
begin
 if FAutoMakeUp
  then FMakeUpGain := FThreshold
  else FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
 Changed;
end;

procedure TCustomBrickwallLimiter.MakeUpGainChanged;
begin
 if not FAutoMakeUp
  then FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
 Changed;
end;

procedure TCustomBrickwallLimiter.SetAutoMakeUp(const Value: Boolean);
begin
 if FAutoMakeUp <> Value then
  begin
   FAutoMakeUp := Value;
   AutoMakeUpChanged;
  end;
end;

procedure TCustomBrickwallLimiter.SetMakeUpGain_dB(const Value: Double);
begin
 if FMakeUpGain_dB <> Value then
  begin
   FMakeUpGain_dB := Value;
   MakeUpGainChanged;
  end;
end;

procedure TCustomBrickwallLimiter.ThresholdChanged;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 if FAutoMakeUp
  then FMakeUpGain := FThresholdReciprocal;
 Changed;
end;

procedure TCustomBrickwallLimiter.InputSample(const Input: Double);
begin
 FGain := TranslatePeakToGain(abs(Input));
end;

procedure TCustomBrickwallLimiter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := GainSample(Data[Sample]);
  end;
end;

procedure TCustomBrickwallLimiter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := GainSample(Data[Sample]);
  end;
end;

function TCustomBrickwallLimiter.ProcessSample32(Input: Single): Single;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

function TCustomBrickwallLimiter.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

{ TBrickwallLimiter }

procedure TBrickwallLimiter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   if Data[Sample] > FThreshold
    then Data[Sample] := FThreshold else
   if Data[Sample] < -FThreshold
    then Data[Sample] := -FThreshold;
   Data[Sample] := FMakeUpGain * Data[Sample];
  end;
end;

procedure TBrickwallLimiter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   if Data[Sample] > FThreshold
    then Data[Sample] := FThreshold else
   if Data[Sample] < -FThreshold
    then Data[Sample] := -FThreshold;
   Data[Sample] := FMakeUpGain * Data[Sample];
  end;
end;

function TBrickwallLimiter.ProcessSample32(Input: Single): Single;
begin
 if Input > FThreshold
  then Result := FThreshold else
 if Input < -FThreshold
  then Result := -FThreshold
  else Result := Input;
 Result := FMakeUpGain * Result;
end;

function TBrickwallLimiter.ProcessSample64(Input: Double): Double;
begin
 if Input > FThreshold
  then Result := FThreshold else
 if Input < -FThreshold
  then Result := -FThreshold
  else Result := Input;
 Result := FMakeUpGain * Result;
end;

function TBrickwallLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel > FThreshold
  then Result := FThreshold / PeakLevel
  else Result := 1;
 Result := FMakeUpGain * Result;
end;

function TBrickwallLimiter.CharacteristicCurve(const InputLevel: Double): Double;
begin
 if InputLevel > FThreshold
  then Result := FThreshold else
 if InputLevel < -FThreshold
  then Result := -FThreshold
  else Result := InputLevel;
 Result := FMakeUpGain * Result;
end;

{ TSoftBrickwallLimiter }

constructor TSoftBrickwallLimiter.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TSoftBrickwallLimiter.KneeChanged;
begin
 CalculateSoftKnee;
 Changed;
end;

procedure TSoftBrickwallLimiter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   FGain := TranslatePeakToGain(abs(Data[Sample]));
   Data[Sample] := Data[Sample] * FGain;
  end;
end;

procedure TSoftBrickwallLimiter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   FGain := TranslatePeakToGain(abs(Data[Sample]));
   Data[Sample] := Data[Sample] * FGain;
  end;
end;

function TSoftBrickwallLimiter.ProcessSample32(Input: Single): Single;
begin
 FGain := TranslatePeakToGain(abs(Input));
 Result := Input * FGain;
end;

function TSoftBrickwallLimiter.ProcessSample64(Input: Double): Double;
begin
 FGain := TranslatePeakToGain(abs(Input));
 Result := Input * FGain;
end;

function TSoftBrickwallLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  a, b : Double;
begin
 a := Power(abs(PeakLevel * FThresholdReciprocal), FSoftKnee[1]);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := FMakeUpGain * Power(a + 1 / b, -FSoftKnee[0]);
end;

procedure TSoftBrickwallLimiter.CalculateSoftKnee;
begin
 FSoftKnee[0] := FKnee_dB * 0.4211083378;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

{ TSimpleSoftBrickwallLimiter }

constructor TSimpleSoftBrickwallLimiter.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TSimpleSoftBrickwallLimiter.KneeChanged;
begin
 CalculateSoftKnee;
 Changed;
end;

function TSimpleSoftBrickwallLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 Result := Power(1 + Power(PeakLevel * FThresholdReciprocal, FSoftKnee[1]), -FSoftKnee[0]);
end;

procedure TSimpleSoftBrickwallLimiter.CalculateSoftKnee;
const
  CdBScale : Double = 0.16609640474436811739351597147447;
begin
 FSoftKnee[0] := FKnee_dB * CdBScale;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

{ TCustomTimeConstantDynamics }

constructor TCustomTimeConstantDynamics.Create;
begin
  inherited;
  FSampleRate := 44100;
  FSampleRateRez := 1 / FSampleRate;
  FAttack := 5;
  FRelease := 5;
  FLevel := 0;
  CalculateThreshold;
  CalculateAttackFactor;
  CalculateReleaseFactor;
end;

procedure TCustomTimeConstantDynamics.SetSampleRate(const Value: Double);
begin
  if FSampleRate <> Value then
  begin
    FSampleRate := Value;
    FSampleRateRez := 1 / FSampleRate;
    SampleRateChanged;
  end;
end;

function TCustomTimeConstantDynamics.GetGainReductiondB: Double;
begin
 Result := Amp_to_dB(FGain);
end;

procedure TCustomTimeConstantDynamics.ReleaseChanged;
begin
 CalculateReleaseFactor;
 Changed;
end;

procedure TCustomTimeConstantDynamics.Reset;
begin
 FPeak := CDenorm32;
end;

procedure TCustomTimeConstantDynamics.AttackChanged;
begin
 CalculateAttackFactor;
 Changed;
end;

procedure TCustomTimeConstantDynamics.SampleRateChanged;
begin
 CalculateAttackFactor;
 CalculateReleaseFactor;
 Changed;
end;

procedure TCustomTimeConstantDynamics.SetAttack(const Value: Double);
begin
  if FAttack <> Value then
  begin
    FAttack := abs(Value);
    AttackChanged;
  end;
end;

procedure TCustomTimeConstantDynamics.SetRelease(const Value: Double);
begin
  if FRelease <> Value then
  begin
    FRelease := abs(Value);
    ReleaseChanged;
  end;
end;

procedure TCustomTimeConstantDynamics.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 1
  else FAttackFactor := 1 - Exp( -ln2 / (FAttack * 0.001 * SampleRate));
end;

procedure TCustomTimeConstantDynamics.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 1
  else FReleaseFactor := Exp( -ln2 / (FRelease * 0.001 * SampleRate));
end;

{ TCustomLimiter }

constructor TCustomLimiter.Create;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 FMakeUpGain          := 1;
 FMakeUpGain_dB       := 0;
end;

procedure TCustomLimiter.AutoMakeUpChanged;
begin
 if FAutoMakeUp
  then FMakeUpGain := FThreshold
  else FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
 Changed;
end;

procedure TCustomLimiter.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

procedure TCustomLimiter.MakeUpGainChanged;
begin
 if not FAutoMakeUp
  then FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
 Changed;
end;

procedure TCustomLimiter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

procedure TCustomLimiter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

function TCustomLimiter.ProcessSample32(Input: Single): Single;
begin
 InputSample(Input);
 Result := FGain * Input;
end;

function TCustomLimiter.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := FGain * Input;
end;

procedure TCustomLimiter.SetAutoMakeUp(const Value: Boolean);
begin
 if FAutoMakeUp <> Value then
  begin
   FAutoMakeUp := Value;
   AutoMakeUpChanged;
  end;
end;

procedure TCustomLimiter.SetMakeUpGain_dB(const Value: Double);
begin
 if FMakeUpGain_dB <> Value then
  begin
   FMakeUpGain_dB := Value;
   MakeUpGainChanged;
  end;
end;

procedure TCustomLimiter.ThresholdChanged;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 if FAutoMakeUp
  then FMakeUpGain := FThresholdReciprocal;
 Changed;
end;

{ TLimiter }

function TLimiter.CharacteristicCurve(const InputLevel: Double): Double;
begin
 if InputLevel > FThreshold
  then Result := FThreshold else
 if InputLevel < -FThreshold
  then Result := -FThreshold
  else Result := InputLevel;
 Result := FMakeUpGain * Result;
end;

procedure TLimiter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

procedure TLimiter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

function TLimiter.ProcessSample32(Input: Single): Single;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 Result := Input * TranslatePeakToGain(FPeak);
end;

function TLimiter.ProcessSample64(Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 Result := Input * TranslatePeakToGain(FPeak);
end;

function TLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel > FThreshold
  then Result := FThreshold / PeakLevel
  else Result := 1;
 Result := FMakeUpGain * Result;
end;


{ TRCLimiter }

procedure TRCLimiter.AttackChanged;
begin
 inherited;
 Release := FAttack;
 Changed;
end;

procedure TRCLimiter.ReleaseChanged;
begin
 inherited;
 Attack := FRelease;
 Changed;
end;

function TRCLimiter.CharacteristicCurve(const InputLevel: Double): Double;
begin
 if InputLevel > FThreshold
  then Result := FThreshold else
 if InputLevel < -FThreshold
  then Result := -FThreshold
  else Result := InputLevel;
 Result := FMakeUpGain * Result;
end;

procedure TRCLimiter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

procedure TRCLimiter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

function TRCLimiter.ProcessSample32(Input: Single): Single;
begin
 FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor;

 if FPeak > FThreshold
  then Result := Input * FMakeUpGain * FThreshold / FPeak
  else Result := Input * FMakeUpGain;
end;

function TRCLimiter.ProcessSample64(Input: Double): Double;
begin
 FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor;

 if FPeak > FThreshold
  then Result := Input * FMakeUpGain * FThreshold / FPeak
  else Result := Input * FMakeUpGain;
end;

function TRCLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel > FThreshold
  then Result := FThreshold / PeakLevel
  else Result := 1;
 Result := FMakeUpGain * Result;
end;


{ TCustomKneeLimiter }

procedure TCustomKneeLimiter.KneeChanged;
begin
 Changed;
end;

{ TSoftKneeLimiter }

constructor TSoftKneeLimiter.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TSoftKneeLimiter.KneeChanged;
begin
 CalculateSoftKnee;
 Changed;
end;

procedure TSoftKneeLimiter.CalculateSoftKnee;
begin
 FSoftKnee[0] := FKnee_dB * 0.4211083378;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

procedure TSoftKneeLimiter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

procedure TSoftKneeLimiter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

function TSoftKneeLimiter.ProcessSample32(Input: Single): Single;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 Result := Input * TranslatePeakToGain(FPeak);
end;

function TSoftKneeLimiter.ProcessSample64(Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 Result := Input * TranslatePeakToGain(FPeak);
end;

function TSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  a, b : Double;
begin
 a := Power(abs(PeakLevel * FThresholdReciprocal), FSoftKnee[1]);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := FMakeUpGain * Power(a + 1 / b, -FSoftKnee[0]);
end;

{ TSimpleSoftKneeLimiter }

constructor TSimpleSoftKneeLimiter.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TSimpleSoftKneeLimiter.KneeChanged;
begin
 CalculateSoftKnee;
 Changed;
end;

procedure TSimpleSoftKneeLimiter.CalculateSoftKnee;
const
  CdBScale : Double = 0.16609640474436811739351597147447;
begin
 FSoftKnee[0] := FKnee_dB * CdBScale;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

procedure TSimpleSoftKneeLimiter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

procedure TSimpleSoftKneeLimiter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

function TSimpleSoftKneeLimiter.ProcessSample32(Input: Single): Single;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 Result := Input * Power2(-FSoftKnee[0] * Log2(1 + Power2(FSoftKnee[1] * Log2(CDenorm32 + FPeak * FThresholdReciprocal))));
end;

function TSimpleSoftKneeLimiter.ProcessSample64(Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 Result := Input * Power2(-FSoftKnee[0] * Log2(1 + Power2(FSoftKnee[1] * Log2(CDenorm64 + FPeak * FThresholdReciprocal))));
end;

function TSimpleSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 Result := Power(1 + Power(PeakLevel * FThresholdReciprocal, FSoftKnee[1]), -FSoftKnee[0]);
end;


{ TCustomTimeConstantRatioDynamics }

constructor TCustomTimeConstantRatioDynamics.Create;
begin
 FRatio := 1;
 CalculateInverseRatio;
 inherited;
end;

procedure TCustomTimeConstantRatioDynamics.RatioChanged;
begin
 CalculateInverseRatio;
 Changed;
end;

procedure TCustomTimeConstantRatioDynamics.CalculateInverseRatio;
begin
 FRatioReciprocal :=  1 / Ratio;
end;

procedure TCustomTimeConstantRatioDynamics.SetRatio(const Value: Double);
begin
 if FRatio <> Value then
  begin
   FRatio := Value;
   RatioChanged;
  end;
end;


procedure TCustomTimeConstantRatioDynamics.ThresholdChanged;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 Changed;
end;

{ TCustomClassicGate }

procedure TCustomClassicGate.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

procedure TCustomClassicGate.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   InputSample(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

function TCustomClassicGate.ProcessSample32(Input: Single): Single;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

function TCustomClassicGate.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

{ TClassicGate }

procedure TClassicGate.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

function TClassicGate.ProcessSample32(Input: Single): Single;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 Result := Input * TranslatePeakToGain(FPeak);
end;

function TClassicGate.ProcessSample64(Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 Result := Input * TranslatePeakToGain(FPeak);
end;

function TClassicGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
  if PeakLevel < FThreshold
   then Result := 0
   else Result := 1;
end;

{ TCustomClassicRangeGate }

constructor TCustomClassicRangeGate.Create;
begin
 inherited;
 FRange_dB := -10;
 CalculateRange;
end;

procedure TCustomClassicRangeGate.CalculateRange;
begin
 FRange := dB_to_Amp(FRange_dB);
end;

procedure TCustomClassicRangeGate.SetRange_dB(const Value: Double);
begin
 if FRange_dB <> Value then
  begin
   FRange_dB := Value;
   CalculateRange;
  end;
end;

{ TClassicSoftRangeGate }

procedure TClassicSoftRangeGate.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

function TClassicSoftRangeGate.TranslatePeakToGain(const PeakLevel: Double): Double;
(*
const
  cScale = 1 / 100;
begin
 Result := Power((Power(PeakLevel, 1 / cScale) + Power(FRange, 1 / cScale) * FThreshold) / (Power(PeakLevel, 1 / cScale) + FThreshold), cScale);
*)
begin
 Result := (PeakLevel + FRange * FThreshold) / (PeakLevel + FThreshold);
end;

{ TCustomSoftKneeGate }

constructor TCustomSoftKneeGate.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TCustomSoftKneeGate.KneeChanged;
begin
 CalculateSoftKnee;
 Changed;
end;

procedure TCustomSoftKneeGate.CalculateSoftKnee;
begin
 FSoftKnee[0] := FKnee_dB * 0.4211083378;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

{ TClassicSoftKneeGate }

procedure TClassicSoftKneeGate.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

procedure TClassicSoftKneeGate.ThresholdChanged;
begin
 inherited;
 CalculateKneedThreshold;
 Changed;
end;

procedure TClassicSoftKneeGate.CalculateSoftKnee;
begin
 inherited;
 CalculateKneedThreshold;
end;

procedure TClassicSoftKneeGate.CalculateKneedThreshold;
begin
 FKneedThreshold := Power(FThreshold, FSoftKnee[1]);
end;

function TClassicSoftKneeGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
// Result := Power((PeakLevel * Power(Power(PeakLevel, FSoftKnee[1]) + FKneedThreshold, -FSoftKnee[0])), 1 / ((abs(PeakLevel - FThreshold) + (FSoftKnee[0] + PeakLevel - FThreshold))));
 Result := Power((PeakLevel * Power(Power(PeakLevel, FSoftKnee[1]) + FKneedThreshold, -FSoftKnee[0])), 1 / ((abs(PeakLevel - FThreshold) + (FSoftKnee[0] + PeakLevel - FThreshold))));
end;

{ TCustomGate }

constructor TCustomGate.Create;
begin
  inherited;
  FGain        := 1;
  FRange_dB    := 10;
  FHold        := 0;
  FHoldSamples := 0;
  FHoldSmplCnt := 0;
  CalculateRangeFactor;
  CalculateHoldSamples;
end;

procedure TCustomGate.CalculateHoldSamples;
begin
  FHoldSamples := FHold * FSampleRate;
end;

procedure TCustomGate.CalculateRangeFactor;
begin
 FRangeFactor := dB_to_Amp(FRange_dB);
end;

procedure TCustomGate.CalculateSoftKnee;
begin
 FSoftKnee[0] := abs(FKnee_dB) + 0.1;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

procedure TCustomGate.KneeChanged;
begin
 CalculateSoftKnee;
 Changed;
end;

procedure TCustomGate.SampleRateChanged;
begin
 inherited;
 CalculateHoldSamples;
end;

procedure TCustomGate.SetHold(Value: Double);
begin
 if Value < 0 then Value := 0;
 if FHold <> Value then
  begin
   FHold := Value;
   CalculateHoldSamples;
  end;
end;

procedure TCustomGate.SetRange_dB(const Value: Double);
begin
 if FRange_dB <> Value then
  begin
   FRange_dB := Value;
   RangeChanged;
  end;
end;

procedure TCustomGate.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor else
 if FHoldSmplCnt > FHoldSamples
  then FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor
  else inc(FHoldSmplCnt);

 FGain := TranslatePeakToGain(FPeak);
end;

procedure TCustomGate.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   FGain := TranslatePeakToGain(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

procedure TCustomGate.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   FGain := TranslatePeakToGain(Data[Sample]);
   Data[Sample] := FGain * Data[Sample];
  end;
end;

function TCustomGate.ProcessSample32(Input: Single): Single;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

function TCustomGate.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

procedure TCustomGate.RangeChanged;
begin
 CalculateRangeFactor;
 Changed;
end;

function TCustomGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then Result := Power(FThreshold, 1 - FRatio) * Power(PeakLevel, FRatio - 1) * (1 - FRangeFactor) + FRangeFactor
  else
   begin
    FHoldSmplCnt := 0; // start hold phase
    Result := FRangeFactor + (1 - FRangeFactor);
   end;
end;

{ TAdvancedGate }

constructor TAdvancedGate.Create;
begin
  inherited;
  FGain              := 1;
  FLowCut            := TButterworthHighpassFilter.Create;
  FHighCut           := TButterworthLowpassFilter.Create;
  FLowCut.Frequency  := 20;
  FHighCut.Frequency := 20000;
end;

destructor TAdvancedGate.Destroy;
begin
  FreeAndNil(FLowCut);
  FreeAndNil(FHighCut);
  inherited;
end;

procedure TAdvancedGate.CalculateRangeThresholdFactor;
begin
 FRangeThresholdFactor := Power(FRangeFactor * FThreshold, FSoftKnee[1]);
end;

procedure TAdvancedGate.CalculateThresholdKneeFactor;
begin
 FThresholdKneeFactor := Power(FThreshold, FSoftKnee[1]);
end;

function TAdvancedGate.GetHighCut: Double;
begin
 Result := FHighCut.Frequency;
end;

function TAdvancedGate.GetLowCut: Double;
begin
 Result := FLowCut.Frequency;
end;

procedure TAdvancedGate.InputSample(const Input: Double);
begin
 inherited InputSample(FHighCut.ProcessSample64(FLowCut.ProcessSample64(Input)));
end;

procedure TAdvancedGate.KneeChanged;
begin
 inherited;
 CalculateRangeThresholdFactor;
 CalculateThresholdKneeFactor;
end;

procedure TAdvancedGate.RangeChanged;
begin
 inherited;
 CalculateRangeThresholdFactor;
end;

procedure TAdvancedGate.SampleRateChanged;
begin
 inherited;
 FLowCut.SampleRate := FSampleRate;
 FHighCut.SampleRate := FSampleRate;
end;

procedure TAdvancedGate.SetHighCut(const Value: Double);
begin
 if FHighCut.Frequency <> Value then
  begin
   FHighCut.Frequency := Value;
  end;
end;

procedure TAdvancedGate.SetLowCut(const Value: Double);
begin
 if FLowCut.Frequency <> Value then
  begin
   FLowCut.Frequency := Value;
  end;
end;

procedure TAdvancedGate.ThresholdChanged;
begin
 inherited;
 CalculateThresholdKneeFactor;
 CalculateRangeThresholdFactor;
end;

function TAdvancedGate.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  ScaledPeakLevel : Double;
begin
 ScaledPeakLevel := Power(PeakLevel, FSoftKnee[1]);
 Result := Power((ScaledPeakLevel + FRangeThresholdFactor) / (ScaledPeakLevel + FThresholdKneeFactor), FSoftKnee[0]);
 if PeakLevel > FThreshold
  then FHoldSmplCnt := 0; // start hold phase
end;


{ TCustomCompressor }

procedure TCustomCompressor.CalculateMakeUpGain;
begin
 FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
end;

constructor TCustomCompressor.Create;
begin
  inherited;
  FMakeUpGain_dB := 0;
  FAutoMakeUp := False;
  CalculateMakeUpGain;
end;

procedure TCustomCompressor.ThresholdChanged;
begin
 inherited;
 CalculateMakeUpGain;
end;

procedure TCustomCompressor.RatioChanged;
begin
 inherited;
 CalculateMakeUpGain;
end;

procedure TCustomCompressor.AutoMakeUpChanged;
begin
 if FAutoMakeUp
  then CalculateMakeUpGain;
 Changed;
end;

procedure TCustomCompressor.MakeUpGainChanged;
begin
 if not FAutoMakeUp
  then CalculateMakeUpGain;
 Changed;
end;

procedure TCustomCompressor.SetAutoMakeUp(const Value: Boolean);
begin
 if FAutoMakeUp <> Value then
  begin
   FAutoMakeUp := Value;
   AutoMakeUpChanged;
  end;
end;

procedure TCustomCompressor.SetMakeUpGain_dB(const Value: Double);
begin
 if FMakeUpGain_dB <> Value then
  begin
   FMakeUpGain_dB := Value;
   MakeUpGainChanged;
  end;
end;

procedure TCustomCompressor.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

function TCustomCompressor.ProcessSample32(Input: Single): Single;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

function TCustomCompressor.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := GainSample(Input);
end;

{ TSimpleCompressor }

procedure  TSimpleCompressor.CalculateMakeUpGain;
var
  dbl : Double;
begin
 inherited;
 dbl := Power(FThreshold, 1 - FRatio);
 if FAutoMakeUp
  then FMakeUpGain[0] := 1 / dbl
  else FMakeUpGain[0] := dB_to_Amp(FMakeUpGain_dB);
 FMakeUpGain[1] := FMakeUpGain[0] * dbl;
end;

function TSimpleCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then Result := FMakeUpGain[0]
  else Result := FMakeUpGain[1] * Power(PeakLevel, FRatio - 1);
end;

{ TCustomKneeCompressor }

procedure TCustomKneeCompressor.KneeChanged;
begin
 Changed;
end;

{ TSoftKneeCompressor }

procedure TSoftKneeCompressor.CalculateMakeUpGain;
var
  dbl : Double;
begin
 inherited;
 dbl := Power(FThreshold, 1 - FRatio);
 if FAutoMakeUp
  then FMakeUpGain[0] := 1 / dbl
  else FMakeUpGain[0] := dB_to_Amp(FMakeUpGain_dB);
 FMakeUpGain[1] := FMakeUpGain[0] * dbl;
end;

procedure TSoftKneeCompressor.ThresholdChanged;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 Changed;
end;

function TSoftKneeCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then Result := FMakeUpGain[0]
  else Result := FMakeUpGain[1] * Power(PeakLevel, FRatio - 1);
end;


{ TCustomFeedbackCompressor }

function TCustomFeedbackCompressor.GainSample(const Input: Double): Double;
begin
 Result := FMakeUpGain * FGain * Input;
end;

procedure TCustomFeedbackCompressor.InputSample(const Input: Double);
begin
 if FPreviousAbsSample > FPeak
  then FPeak := FPeak + (FPreviousAbsSample - FPeak) * FAttackFactor
  else FPeak := FPreviousAbsSample + (FPeak - FPreviousAbsSample) * FReleaseFactor;

 FGain := TranslatePeakToGain(abs(FPeak));
 FPreviousAbsSample := abs(Input * FGain);
end;

{ TSimpleFeedbackCompressor }

procedure TSimpleFeedbackCompressor.CalculateAttackFactor;
begin
 if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack * 0.001 * SampleRate * FRatioReciprocal));
end;

procedure TSimpleFeedbackCompressor.CalculateReleaseFactor;
begin
 if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease * 0.001 * SampleRate * FRatioReciprocal));
end;

procedure TSimpleFeedbackCompressor.RatioChanged;
begin
 inherited;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

function TSimpleFeedbackCompressor.ProcessSample32(Input: Single): Single;
begin
 if FPreviousAbsSample > FPeak
  then FPeak := FPeak + (FPreviousAbsSample - FPeak) * FAttackFactor
  else FPeak := FPreviousAbsSample + (FPeak - FPreviousAbsSample) * FReleaseFactor;

 FPreviousAbsSample := Input * TranslatePeakToGain(abs(FPeak));

 Result := FMakeUpGains[0] * FPreviousAbsSample;
 FPreviousAbsSample := abs(FPreviousAbsSample);
end;

function TSimpleFeedbackCompressor.ProcessSample64(Input: Double): Double;
begin
 if FPreviousAbsSample > FPeak
  then FPeak := FPeak + (FPreviousAbsSample - FPeak) * FAttackFactor
  else FPeak := FPreviousAbsSample + (FPeak - FPreviousAbsSample) * FReleaseFactor;

 FPreviousAbsSample := Input * TranslatePeakToGain(abs(FPeak));

 Result := FMakeUpGains[0] * FPreviousAbsSample;
 FPreviousAbsSample := abs(FPreviousAbsSample);
end;

procedure TSimpleFeedbackCompressor.CalculateMakeUpGain;
var
  dbl : Double;
begin
 dbl := Power(FThreshold, FRatio - 1);
 if FAutoMakeUp
  then FMakeUpGains[0] := dbl
  else FMakeUpGains[0] := dB_to_Amp(FMakeUpGain_dB);
 FMakeUpGains[1] := Power(FThreshold, (1 - FRatio) * FRatioReciprocal);
end;

function TSimpleFeedbackCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then Result := 1
  else Result := FMakeUpGains[1] * Power(PeakLevel, 1 - FRatioReciprocal);
end;

function TSimpleFeedbackCompressor.CharacteristicCurve(const InputLevel: Double): Double;
begin
 Result := FMakeUpGains[0] * Power(TranslatePeakToGain(abs(InputLevel)), FRatio) * InputLevel;
end;

{ TSoftKneeFeedbackCompressor }

function TSoftKneeFeedbackCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
(*
var
  a, b, c : Double;
*)
begin
(*
 a := PeakLevel/FThreshold;
 Result := 1 + 0.5 * (1 + abs(a - 1) / (a - 1)) * (FMakeUpGain[1] * Power(PeakLevel, 1 - FRatioReciprocal) - 1);
*)

(*
 b := PeakLevel / FThreshold;
 a := 0.5 * (abs(b - 1) / (b - 1) + 1);
 Result := 1 + a * (Power(b, 1 - FRatioReciprocal) - 1);
*)

(*
 b := PeakLevel / FThreshold;
 a := Power(b, 1 - FRatioReciprocal);
 Result := 1 + 0.5 * (abs(a - 1) + (a - 1));
*)

 if PeakLevel < FThreshold
  then Result := 1
  else Result := FMakeUpGains[1] * Power(PeakLevel, 1 - FRatioReciprocal);

(*
 a := Power(FPeak, FRatioReciprocal);
 Result := (1 - a / (a + 3)) * FThresholdReciprocal;
*)
end;

function TSoftKneeFeedbackCompressor.ProcessSample32(Input: Single): Single;
begin
 if FPreviousAbsSample > FPeak
  then FPeak := FPeak + (FPreviousAbsSample - FPeak) * FAttackFactor
  else FPeak := FPreviousAbsSample + (FPeak - FPreviousAbsSample) * FReleaseFactor;

 FPreviousAbsSample := Input * TranslatePeakToGain(abs(FPeak));

 Result := FMakeUpGains[0] * FPreviousAbsSample;
 FPreviousAbsSample := abs(FPreviousAbsSample);
end;

function TSoftKneeFeedbackCompressor.ProcessSample64(Input: Double): Double;
begin
 if FPreviousAbsSample > FPeak
  then FPeak := FPeak + (FPreviousAbsSample - FPeak) * FAttackFactor
  else FPeak := FPreviousAbsSample + (FPeak - FPreviousAbsSample) * FReleaseFactor;

 FPreviousAbsSample := Input * TranslatePeakToGain(abs(FPeak));

 Result := FMakeUpGains[0] * FPreviousAbsSample;
 FPreviousAbsSample := abs(FPreviousAbsSample);
end;

{ TSimpleRMSCompressor }

constructor TSimpleRMSCompressor.Create;
begin
 inherited;
 FCurrentRMS := 0;
 FRMSTime := 10;
 UpdateRMSBuffer;
end;

procedure TSimpleRMSCompressor.InputSample(const Input: Double);
begin
 // calculate RMS stuff
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;

 inherited InputSample(Sqrt(FCurrentRMS * FRMSFactor));
end;

function TSimpleRMSCompressor.ProcessSample32(Input: Single): Single;
var
  Temp: Single;
begin
 // calculate RMS stuff
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;

 // time constants
 Temp := Sqrt(FCurrentRMS * FRMSFactor);
 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);

 Result := FGain * Input;
end;

function TSimpleRMSCompressor.ProcessSample64(Input: Double): Double;
var
  Temp: Double;
begin
 // calculate RMS stuff
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;

 // time constants
 Temp := Sqrt(FCurrentRMS * FRMSFactor);
 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);

 Result := FGain * Input;
end;

procedure TSimpleRMSCompressor.UpdateRMSBuffer;
var
  i : Integer;
begin
 i := Round(FSampleRate * 0.001 * FRMSTime);
 SetLength(FRMSBuffer, i);
 if i > FRMSSize then FillChar(FRMSBuffer[FRMSSize], (i - FRMSSize) * SizeOf(Double), 0);
 FRMSSize := i; FRMSFactor := 1 / FRMSSize;
 if FRMSPos > FRMSSize then FRMSPos := 0;
end;

procedure TSimpleRMSCompressor.SampleRateChanged;
begin
 inherited;
 UpdateRMSBuffer;
end;

procedure TSimpleRMSCompressor.SetRMSTime(const Value: Double);
begin
 if FRMSTime <> Value then
  begin
   FRMSTime := Value;
   UpdateRMSBuffer;
  end;
end;

{ TCompressor }

constructor TCompressor.Create;
begin
 inherited;
 FLowCut  := TButterworthHighpassFilter.Create;
 FHighCut := TButterworthLowpassFilter.Create;
 FLowCut.Frequency := 20;
 FHighCut.Frequency := 20000;
end;

destructor TCompressor.Destroy;
begin
 FreeAndNil(FLowCut);
 FreeAndNil(FHighCut);
 inherited;
end;

function TCompressor.GetHighCut: Double;
begin
 Result := FHighCut.Frequency;
end;

function TCompressor.GetLowCut: Double;
begin
 Result := FLowCut.Frequency;
end;

function TCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
(*
var
  Soft : Double;
*)
begin
// Soft := FThreshold / (PeakLevel + FThreshold);

 if PeakLevel < FThreshold
  then Result := FMakeUpGain[0]
  else Result := FMakeUpGain[1] * Power(PeakLevel, FRatio - 1);

// Result := sqrt(Soft * Result);
end;

procedure TCompressor.InputSample(const Input: Double);
begin
 inherited InputSample(FHighCut.ProcessSample64(FLowCut.ProcessSample64(Input)));
end;

function TCompressor.ProcessSample32(Input: Single): Single;
var
  Temp: Single;
begin
 // calculate RMS stuff
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;

 // time constants
 Temp := Sqrt(FCurrentRMS * FRMSFactor);
 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);

 Result := FGain * Input;
end;

function TCompressor.ProcessSample64(Input: Double): Double;
var
  Temp: Double;
begin
 // calculate RMS stuff
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;

 // time constants
 Temp := Sqrt(FCurrentRMS * FRMSFactor);
 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);

 Result := FGain * Input;
end;

procedure TCompressor.SetHighCut(const Value: Double);
begin
 if FHighCut.Frequency <> Value
  then FHighCut.Frequency := Value;
end;

procedure TCompressor.SetLowCut(const Value: Double);
begin
 if FLowCut.Frequency <> Value
  then FLowCut.Frequency := Value;
end;

(*
{ TBrickwallLimiter }

function TBrickwallLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then Result := 1
  else Result := FThresholdRatioFactor * Power(PeakLevel, FRatio - 1);
end;

function TBrickwallLimiter.ProcessSample64(Input: Double): Double;
{$IFNDEF PUREPASCAL}
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := CharacteristicCurve(FPeak);

 Result := FGain * Input;
end;
{$ELSE}
asm
 fld Input                        // Input
 fabs                             // abs(Input)
 fld [self.FPeak].Double          // FPeak, abs(Input)
 mov edx, eax                     // edx = self
 fcom st(1)                       // FPeak, abs(Input)
 fstsw ax                         // ax = FPU Status Word
 sahf                             // ax -> EFLAGS register
 jbe @Attack                      // goto Attack
@Release:
 fsub st(0), st(1)                // FPeak - abs(Input), abs(Input)
 fmul [edx.FReleaseFactor].Double   // (FPeak - abs(Input)) * FReleaseFactor, abs(Input)
 faddp                            // (FPeak - abs(Input)) * FReleaseFactor + abs(Input)
 fst [self.FPeak].Double          // FPeak := (FPeak - abs(Input)) * FReleaseFactor + abs(Input)
 jmp @EndAttack
@Attack:
 fxch                             // abs(Input), FPeak
 fsub st(0), st(1)                // abs(Input) - FPeak, FPeak
 fmul [edx.FAttackFactor].Double  // (abs(Input) - FPeak) * FAttackFactor, FPeak
 faddp                            // (abs(Input) - FPeak) * FAttackFactor + FPeak
 fst  [self.FPeak].Double         // FPeak := (abs(Input) - FPeak) * FAttackFactor + FPeak
@EndAttack:

 fld [edx.FThreshold].Double      // FThreshold, FPeak
 fcom st(1)                       // FThreshold, FPeak
 fstsw ax                         // ax = FPU Status Word
 sahf                             // ax -> EFLAGS register
 fstp st(0)                       // FPeak
 jbe @Limit                       // goto Limit
 fstp st(0)                       // --
 fld Input                        // Input
 jmp @Exit
@Limit:

 fld [edx.FRatio].Double          // FRatio, FPeak
 fld1                             // 1, FRatio, FPeak
 fsubp                            // FRatio - 1, FPeak
 fxch
 fldln2                           // {
 fxch                             //
 fyl2x                            //
 fxch                             //
 fmulp   st(1), st                //  P
 fldl2e                           //  O
 fmulp   st(1), st                //  W
 fld     st(0)                    //  E
 frndint                          //  R
 fsub    st(1), st                //
 fxch    st(1)                    //
 f2xm1                            //
 fld1                             //
 faddp   st(1), st                //
 fscale                           // }
 fstp    st(1)

 fmul [edx.FThresholdRatioFactor].Double // FThresholdRatioFactor * Power(FPeak, FRatio - 1)
 fmul Input                              // Input * FThresholdRatioFactor * Power(FPeak, FRatio - 1)


@Exit:
end;
{$ENDIF}

procedure TBrickwallLimiter.ThresholdChanged;
begin
 inherited;
 CalculateThresholdRatioFactor;
end;

procedure TBrickwallLimiter.RatioChanged;
begin
 inherited;
 CalculateThresholdRatioFactor;
end;

procedure TBrickwallLimiter.CalculateThresholdRatioFactor;
begin
 FThresholdRatioFactor := Power(FThreshold, 1 - FRatio);
end;

{ TSoftKneeLimiter }

constructor TSoftKneeLimiter.Create;
begin
 inherited;
 FSoftKnee := 1;
end;

function TSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  InternalRatio, Knee : Double;
begin
 Knee := 0.5 *(1 + Tanh2c(FSoftKnee * log10(PeakLevel / FThreshold)));
 InternalRatio := 1 + Knee * (FRatio - 1);
 Result := Power(FThreshold, 1 - InternalRatio) * Power(PeakLevel, InternalRatio - 1);
end;

function TSoftKneeLimiter.ProcessSample64(Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;
 FGain := CharacteristicCurve(FPeak);
 Result := FGain * Input;
end;

procedure TSoftKneeLimiter.SetSoftKnee(const Value: Double);
begin
 if FSoftKnee <> Value then
  begin
   FSoftKnee := Value;
  end;
end;
*)

(*
{ TSoftKneeFeedbackLimiter }

constructor TSoftKneeFeedbackLimiter.Create;
begin
 inherited;
 FGain := 1;
 FOversample := Round(1E5 / Samplerate + 0.5);
 FFilter := TButterworthLowCut.Create;
 FFilter.Frequency := 13.8;
 FFilter.Order := 1;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TSoftKneeFeedbackLimiter.CalculateAttackFactor;
begin
 if FAttack = 0
  then FAttackFactor := 0
  else FAttackFactor := exp( -ln2 / (FOversample * FAttack * 0.001 * SampleRate));
 FAttackFac2 := exp( -ln2 / (0.48 * SampleRate));
end;

procedure TSoftKneeFeedbackLimiter.CalculateReleaseFactor;
begin
 if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / ( {FOversample *} FRelease * 0.001 * SampleRate));
 FReleaseFac2 := exp( -ln2 / (0.98 * SampleRate));
end;

function TSoftKneeFeedbackLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 // yet to do
 Result := 1;
end;

function TSoftKneeFeedbackLimiter.ProcessSample64(Input: Double): Double;
var
  InternalRatio    : Double;
  PeakdB           : Double;
begin
 Input := FFilter.ProcessSample64(Input);
{

// threshold = -13.4 .. - 13

 Result := FGain * Input;
 if abs(Result)>FPeak2
  then FPeak2 := abs(Result) + (FPeak2 - abs(Result)) * FAttackFac2
  else FPeak2 := abs(Result) + (FPeak2 - abs(Result)) * FReleaseFac2;

 if abs(Result)>FPeak
  then
   begin
    FPeak := abs(Result) + (FPeak - abs(Result)) * FAttackFactor;
    PeakdB := Amp_to_dB(abs(0.3 * FPeak2 + 0.7 * FPeak + 1E-32));
    InternalRatio := - (3 + 3 * (PeakdB - FThreshold_dB - 0.5) / (abs(PeakdB - FThreshold_dB) + 1));
    FGain := dB_to_Amp(PeakdB * InternalRatio - FThreshold_dB * InternalRatio);
    for OversampleCount := 1 to FOversample - 1 do
     begin
      Result := FGain * Input;
      FPeak := abs(Result) + (FPeak - abs(Result)) * FAttackFactor;
      PeakdB := Amp_to_dB(abs(0.3 * FPeak2 + 0.7 * FPeak + 1E-32));
      InternalRatio := - (3 + 3 * (PeakdB - FThreshold_dB - 0.5) / (abs(PeakdB - FThreshold_dB) + 1));
      FGain := dB_to_Amp(PeakdB * InternalRatio - FThreshold_dB * InternalRatio);
     end
   end
  else
   begin
    FPeak := abs(Result) + (FPeak - abs(Result)) * FReleaseFactor;
    PeakdB := Amp_to_dB(abs(0.3 * FPeak2 + 0.7 * FPeak + 1E-32));
    InternalRatio := - (3 + 3 * (PeakdB - FThreshold_dB - 0.5) / (abs(PeakdB - FThreshold_dB) + 1));
    FGain := dB_to_Amp(PeakdB * InternalRatio - FThreshold_dB * InternalRatio);
   end;

}

 Result := FGain * Input;
{
 if abs(Result)>FPeak
  then FPeak := abs(Result) + (FPeak - abs(Result)) * FAttackFactor
  else FPeak := abs(Result) + (FPeak - abs(Result)) * FReleaseFactor;
}
 FPeak := FReleaseFactor * FPeak;
 if abs(Result) > FPeak
  then FPeak := abs(Result) + (FPeak - abs(Result)) * FAttackFactor;

 FPeak2 := FReleaseFac2 * FPeak2;
 if abs(Result) > FPeak2
  then FPeak2 := abs(Result) + (FPeak2 - abs(Result)) * FAttackFac2;

 PeakdB := Amp_to_dB(abs(0.3 * FPeak + 0.7 * FPeak2 + 1E-32));
// InternalRatio := - (3 + 3 * (PeakdB - FThreshold_dB - 0.5) / (abs(PeakdB - FThreshold_dB) + 1));
 InternalRatio := - (2.6 + 2.6 * (PeakdB - FThreshold_dB - 1.5) / (abs(PeakdB - FThreshold_dB) + 2));
 FGain := dB_to_Amp(PeakdB * InternalRatio - FThreshold_dB * InternalRatio);
end;

procedure TSoftKneeFeedbackLimiter.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := FSampleRate;
 FOversample := Round(1E5 / FSampleRate + 1.5);
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;
*)

initialization
(*
  CHalf32    := DAV_Common.CHalf32;
  CHalf64    := DAV_Common.CHalf64;
  CQuarter32 := DAV_Common.CQuarter32;
  CQuarter64 := DAV_Common.CQuarter64;
*)
{$IFDEF HandleDenormals}
  CDenorm32  := DAV_Common.CDenorm32;
  CDenorm64  := DAV_Common.CDenorm64;
{$ENDIF}

  RegisterDspProcessors32([TSimpleDirectGate, TSoftDirectGate,
    TBrickwallLimiter, TSoftBrickwallLimiter, TLimiter, TRCLimiter,
    TSoftKneeLimiter, TSimpleSoftKneeLimiter, TClassicGate,
    TClassicSoftRangeGate, TClassicSoftKneeGate, TAdvancedGate,
    TSimpleCompressor, TSoftKneeCompressor, TSimpleFeedbackCompressor,
    TSoftKneeFeedbackCompressor, TSimpleRMSCompressor, TCompressor]);

  RegisterDspProcessors64([TSimpleDirectGate, TSoftDirectGate,
    TBrickwallLimiter, TSoftBrickwallLimiter, TLimiter, TRCLimiter,
    TSoftKneeLimiter, TSimpleSoftKneeLimiter, TClassicGate,
    TClassicSoftRangeGate, TClassicSoftKneeGate, TAdvancedGate,
    TSimpleCompressor, TSoftKneeCompressor, TSimpleFeedbackCompressor,
    TSoftKneeFeedbackCompressor, TSimpleRMSCompressor, TCompressor]);

end.
