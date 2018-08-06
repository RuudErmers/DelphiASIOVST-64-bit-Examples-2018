unit SEDynamicsModule;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
//  SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_DspDynamics, DAV_SECommon, DAV_SEModule;

type
  TCustomDynamicsSEModuleClass = class of TCustomDynamicsSEModule;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomDynamicsSEModule                                                 //
  //  -----------------------                                                 //
  //                                                                          //
  //  Base class for all dynamics, simple features one input and one output.  //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomDynamicsSEModule = class(TSEModuleBase)
  protected
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure Open; override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleDirectGateSEModule                                         //
  //  -------------------------------                                         //
  //                                                                          //
  //  Base class for the simple direct gate.                                  //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleDirectGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TSimpleDirectGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSimpleDirectGateStaticSEModule = class(TCustomSimpleDirectGateSEModule)
  protected
    FThreshold : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSimpleDirectGateParamStaticSEModule = class(TSimpleDirectGateStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSimpleDirectGateAutomatableSEModule = class(TCustomSimpleDirectGateSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftDirectGateSEModule                                           //
  //  -----------------------------                                           //
  //                                                                          //
  //  Base class for the soft direct gate.                                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftDirectGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TSoftDirectGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSoftDirectGateStaticSEModule = class(TCustomSoftDirectGateSEModule)
  protected
    FThreshold : Single;
    FKnee_dB   : Single;      
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSoftDirectGateParamStaticSEModule = class(TSoftDirectGateStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSoftDirectGateAutomatableSEModule = class(TCustomSoftDirectGateSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override; 
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomBrickwallLimiterSEModule                                         //
  //  -------------------------------                                         //
  //                                                                          //
  //  Base class for all brickwall limiters.                                  //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomBrickwallLimiterSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor : TCustomBrickwallLimiter;
    FAutoMakeUp      : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleBrickwallLimiterSEModule                                   //
  //  -------------------------------------                                   //
  //                                                                          //
  //  Base class for the simple brickwall limiter.                            //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleBrickwallLimiterSEModule = class(TCustomBrickwallLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TBrickwallLimiterStaticSEModule = class(TCustomSimpleBrickwallLimiterSEModule)
  protected
    FThreshold : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TBrickwallLimiterParamStaticSEModule = class(TBrickwallLimiterStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TBrickwallLimiterAutomatableSEModule = class(TCustomSimpleBrickwallLimiterSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftBrickwallLimiterSEModule                                     //
  //  -----------------------------------                                     //
  //                                                                          //
  //  Base class for the soft brickwall limiter.                              //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftBrickwallLimiterSEModule = class(TCustomBrickwallLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TBrickwallSoftLimiterStaticSEModule = class(TCustomSoftBrickwallLimiterSEModule)
  protected
    FThreshold : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TBrickwallSoftLimiterParamStaticSEModule = class(TBrickwallSoftLimiterStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TBrickwallSoftLimiterAutomatableSEModule = class(TCustomSoftBrickwallLimiterSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleSoftBrickwallLimiterSEModule                               //
  //  -----------------------------------------                               //
  //                                                                          //
  //  Base class for the simple soft brickwall limiter.                       //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleSoftBrickwallLimiterSEModule = class(TCustomBrickwallLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TBrickwallSimpleSoftLimiterStaticSEModule = class(TCustomSimpleSoftBrickwallLimiterSEModule)
  protected
    FThreshold : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TBrickwallSimpleSoftLimiterParamStaticSEModule = class(TBrickwallSimpleSoftLimiterStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TBrickwallSimpleSoftLimiterAutomatableSEModule = class(TCustomSimpleSoftBrickwallLimiterSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomRangeGateSEModule                                                //
  //  ------------------------                                                //
  //                                                                          //
  //  Base class for the classic gate.                                        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomRangeGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TClassicSoftRangeGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TRangeGateStaticSEModule = class(TCustomRangeGateSEModule)
  protected
    FThreshold : Single;
    FKnee      : Single;
    FRange     : Single;
    FAttack    : Single;
    FRelease   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TRangeGateParamStaticSEModule = class(TRangeGateStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TRangeGateAutomatableSEModule = class(TCustomRangeGateSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee      : PDAVSingleFixedArray;
    FRange     : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomClassicGateSEModule                                              //
  //  --------------------------                                              //
  //                                                                          //
  //  Base class for the classic gate.                                        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomClassicGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TClassicGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TClassicGateStaticSEModule = class(TCustomClassicGateSEModule)
  protected
    FThreshold : Single;
    FKnee      : Single;
    FAttack    : Single;
    FRelease   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TClassicGateParamStaticSEModule = class(TClassicGateStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TClassicGateAutomatableSEModule = class(TCustomClassicGateSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee      : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftClassicGateSEModule                                           //
  //  -----------------------------                                           //
  //                                                                          //
  //  Base class for the soft direct gate.                                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftClassicGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TClassicSoftKneeGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSoftClassicGateStaticSEModule = class(TCustomSoftClassicGateSEModule)
  protected
    FThreshold : Single;
    FKnee_dB   : Single;      
    FAttack    : Single;
    FRelease   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSoftClassicGateParamStaticSEModule = class(TSoftClassicGateStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSoftClassicGateAutomatableSEModule = class(TCustomSoftClassicGateSEModule)
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomLimiterSEModule                                                  //
  //  ----------------------                                                  //
  //                                                                          //
  //  Base class for all time constant limiters.                              //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomLimiterSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor : TCustomLimiter;
    FAutoMakeUp      : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleLimiterSEModule                                            //
  //  ----------------------------                                            //
  //                                                                          //
  //  Base class for the simple limiter.                                      //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleLimiterSEModule = class(TCustomLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TLimiterStaticSEModule = class(TCustomSimpleLimiterSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TLimiterParamStaticSEModule = class(TLimiterStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TLimiterAutomatableSEModule = class(TCustomSimpleLimiterSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftLimiterSEModule                                              //
  //  --------------------------                                              //
  //                                                                          //
  //  Base class for the soft  limiter.                                       //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftLimiterSEModule = class(TCustomLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TSoftLimiterStaticSEModule = class(TCustomSoftLimiterSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSoftLimiterParamStaticSEModule = class(TSoftLimiterStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSoftLimiterAutomatableSEModule = class(TCustomSoftLimiterSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleSoftLimiterSEModule                                        //
  //  --------------------------------                                        //
  //                                                                          //
  //  Base class for the simple soft limiter.                                 //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleSoftLimiterSEModule = class(TCustomLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TSimpleSoftLimiterStaticSEModule = class(TCustomSimpleSoftLimiterSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSimpleSoftLimiterParamStaticSEModule = class(TSoftLimiterStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSimpleSoftLimiterAutomatableSEModule = class(TCustomSimpleSoftLimiterSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomCompressorSEModule                                               //
  //  -------------------------                                               //
  //                                                                          //
  //  Base class for all time constant compressors.                           //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomCompressorSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor : TCustomCompressor;
    FAutoMakeUp      : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleCompressorSEModule                                         //
  //  -------------------------------                                         //
  //                                                                          //
  //  Base class for the Simple Soft Compressor.                              //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleCompressorSEModule = class(TCustomCompressorSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TSimpleCompressorStaticSEModule = class(TCustomSimpleCompressorSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FRatio     : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSimpleCompressorParamStaticSEModule = class(TSimpleCompressorStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSimpleCompressorAutomatableSEModule = class(TCustomSimpleCompressorSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FRatio     : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftKneeCompressorSEModule                                         //
  //  -------------------------------                                         //
  //                                                                          //
  //  Base class for the SoftKnee Soft Compressor.                              //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftKneeCompressorSEModule = class(TCustomCompressorSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TSoftKneeCompressorStaticSEModule = class(TCustomSoftKneeCompressorSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FRatio     : Single;
    FKnee      : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSoftKneeCompressorParamStaticSEModule = class(TSoftKneeCompressorStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSoftKneeCompressorAutomatableSEModule = class(TCustomSoftKneeCompressorSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FRatio     : PDAVSingleFixedArray;
    FKnee      : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomRMSCompressorSEModule                                            //
  //  ----------------------------                                            //
  //                                                                          //
  //  Base class for the RMS Soft Compressor.                                 //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomRMSCompressorSEModule = class(TCustomCompressorSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TRMSCompressorStaticSEModule = class(TCustomRMSCompressorSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FRatio     : Single;
    FRMSTime   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TRMSCompressorParamStaticSEModule = class(TRMSCompressorStaticSEModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TRMSCompressorAutomatableSEModule = class(TCustomRMSCompressorSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FRatio     : PDAVSingleFixedArray;
    FRMSTime   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

implementation

uses
  SysUtils;

procedure TCustomDynamicsSEModule.Open;
begin
 inherited Open;
 ChooseProcess;
end;

procedure TCustomDynamicsSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  begin
   ChooseProcess;
   Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
  end;
end;

// describe your module
class procedure TCustomDynamicsSEModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TCustomDynamicsSEModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomDynamicsSEModule.ChooseProcess;
begin
 if Pin[0].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe the pins (plugs)
function TCustomDynamicsSEModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Input';
       VariableAddress := @FInputBuffer;
       Direction       := drIn;
       Datatype        := dtFSample;
       DefaultValue    := '0';
       Flags           := [iofLinearInput];
      end;

  // typical output plug
  1: with Properties^ do
      begin
       Name            := 'Output';
       VariableAddress := @FOutputBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;

  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TCustomSimpleDirectGateSEModule }

constructor TCustomSimpleDirectGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleDirectGate.Create;
end;

destructor TCustomSimpleDirectGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomSimpleDirectGateSEModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TCustomSimpleDirectGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSimpleDirectGateStaticSEModule }

constructor TSimpleDirectGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
end;

class procedure TSimpleDirectGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Direct Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Direct Gate (Static)';
  end;
end;

function TSimpleDirectGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSimpleDirectGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
 end;
 inherited;
end;

procedure TSimpleDirectGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSimpleDirectGateParamStaticSEModule }

class procedure TSimpleDirectGateParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
  inherited;
 with Properties^ do
  begin
   Name := 'Simple Direct Gate';
   ID := 'DAV Simple Direct Gate';
  end;
end;

function TSimpleDirectGateParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do Direction := drIn;
 end;
end;

{ TSimpleDirectGateAutomatableSEModule }

class procedure TSimpleDirectGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Simple Direct Gate (Automatable)';
   ID := 'DAV Simple Direct Gate (Automatable)';
  end;
end;

function TSimpleDirectGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
 end;
end;

procedure TSimpleDirectGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: if (Pin[2].Status <> stRun)
      then OnProcess := SubProcessStatic
      else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TSimpleDirectGateAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

procedure TSimpleDirectGateAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TCustomSoftDirectGateSEModule }

constructor TCustomSoftDirectGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSoftDirectGate.Create;
end;

destructor TCustomSoftDirectGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomSoftDirectGateSEModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TCustomSoftDirectGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSoftDirectGateStaticSEModule }

constructor TSoftDirectGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB := 1;
end;

class procedure TSoftDirectGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Soft Direct Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Soft Direct Gate (Static)';
  end;
end;

function TSoftDirectGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSoftDirectGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
  3: FDynamicProcesor.Knee_dB := FKnee_dB;
 end;
 inherited;
end;

procedure TSoftDirectGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSoftDirectGateParamStaticSEModule }

class procedure TSoftDirectGateParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Soft Direct Gate';
   ID := 'DAV Soft Direct Gate';
  end;
end;

function TSoftDirectGateParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..3: with Properties^ do Direction := drIn;
 end;
end;

{ TSoftDirectGateAutomatableSEModule }

class procedure TSoftDirectGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Soft Direct Gate (Automatable)';
   ID := 'DAV Soft Direct Gate (Automatable)';
  end;
end;

function TSoftDirectGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
 end;
end;

procedure TSoftDirectGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: if (Pin[2].Status <> stRun) and (Pin[3].Status <> stRun)
      then OnProcess := SubProcessStatic
      else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TSoftDirectGateAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Knee   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Knee   := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Knee_dB := 10 * Knee[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

procedure TSoftDirectGateAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TCustomBrickwallLimiterSEModule }

destructor TCustomBrickwallLimiterSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomBrickwallLimiterSEModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

function TCustomBrickwallLimiterSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TCustomBrickwallLimiterSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.AutoMakeUp := FAutoMakeUp;
 end;
 inherited;
end;

procedure TCustomBrickwallLimiterSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TCustomSimpleBrickwallLimiterSEModule }

constructor TCustomSimpleBrickwallLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TBrickwallLimiter.Create;
end;

{ TBrickwallLimiterStaticSEModule }

constructor TBrickwallLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
end;

class procedure TBrickwallLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Brickwall Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Brickwall Limiter (Static)';
  end;
end;

function TBrickwallLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TBrickwallLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
 end;
 inherited;
end;

procedure TBrickwallLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TBrickwallLimiterParamStaticSEModule }

class procedure TBrickwallLimiterParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Brickwall Limiter';
   ID := 'DAV Brickwall Limiter';
  end;
end;

function TBrickwallLimiterParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..3: with Properties^ do Direction := drIn;
 end;
end;

{ TBrickwallLimiterAutomatableSEModule }

class procedure TBrickwallLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Brickwall Limiter (Automatable)';
   ID := 'DAV Brickwall Limiter (Automatable)';
  end;
end;

function TBrickwallLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
 end;
end;

procedure TBrickwallLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: if (Pin[3].Status <> stRun)
      then OnProcess := SubProcessStatic
      else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TBrickwallLimiterAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

procedure TBrickwallLimiterAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TCustomSoftBrickwallLimiterSEModule }

constructor TCustomSoftBrickwallLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSoftBrickwallLimiter.Create;
end;

{ TBrickwallSoftLimiterStaticSEModule }

constructor TBrickwallSoftLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB := 1;
end;

class procedure TBrickwallSoftLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Brickwall Soft Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Brickwall Soft Limiter (Static)';
  end;
end;

function TBrickwallSoftLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TBrickwallSoftLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Knee_dB := FKnee_dB;
 end;
 inherited;
end;

procedure TBrickwallSoftLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TBrickwallSoftLimiterParamStaticSEModule }

class procedure TBrickwallSoftLimiterParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Brickwall Soft Limiter';
   ID := 'DAV Brickwall Soft Limiter';
  end;
end;

function TBrickwallSoftLimiterParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..4: with Properties^ do Direction := drIn;
 end;
end;

{ TBrickwallSoftLimiterAutomatableSEModule }

class procedure TBrickwallSoftLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Brickwall Soft Limiter (Automatable)';
   ID := 'DAV Brickwall Soft Limiter (Automatable)';
  end;
end;

function TBrickwallSoftLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
 end;
end;

procedure TBrickwallSoftLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3..4: if (Pin[3].Status <> stRun) and (Pin[4].Status <> stRun)
         then OnProcess := SubProcessStatic
         else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TBrickwallSoftLimiterAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Knee   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Knee   := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Knee_dB := 10 * Knee[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

procedure TBrickwallSoftLimiterAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TCustomSimpleSoftBrickwallLimiterSEModule }

constructor TCustomSimpleSoftBrickwallLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleSoftBrickwallLimiter.Create;
end;

{ TBrickwallSimpleSoftLimiterStaticSEModule }

constructor TBrickwallSimpleSoftLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB := 1;
end;

class procedure TBrickwallSimpleSoftLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Brickwall Simple Soft Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Brickwall Simple Soft Limiter (Static)';
  end;
end;

function TBrickwallSimpleSoftLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TBrickwallSimpleSoftLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Knee_dB := FKnee_dB;
 end;
 inherited;
end;

procedure TBrickwallSimpleSoftLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TBrickwallSimpleSoftLimiterParamStaticSEModule }

class procedure TBrickwallSimpleSoftLimiterParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Brickwall Simple Soft Limiter';
   ID := 'DAV Brickwall Simple Soft Limiter';
  end;
end;

function TBrickwallSimpleSoftLimiterParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..4: with Properties^ do Direction := drIn;
 end;
end;

{ TBrickwallSimpleSoftLimiterAutomatableSEModule }

class procedure TBrickwallSimpleSoftLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Brickwall Simple Soft Limiter (Automatable)';
   ID := 'DAV Brickwall Simple Soft Limiter (Automatable)';
  end;
end;

function TBrickwallSimpleSoftLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
 end;
end;

procedure TBrickwallSimpleSoftLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3..4: if (Pin[3].Status <> stRun) and (Pin[4].Status <> stRun)
         then OnProcess := SubProcessStatic
         else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TBrickwallSimpleSoftLimiterAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Knee   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Knee   := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Knee_dB := 10 * Knee[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

procedure TBrickwallSimpleSoftLimiterAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;


{ TCustomRangeGateSEModule }

constructor TCustomRangeGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TClassicSoftRangeGate.Create;
end;

destructor TCustomRangeGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomRangeGateSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

procedure TCustomRangeGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TRangeGateStaticSEModule }

constructor TRangeGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FRange     := -10;
 FAttack    :=  10;
 FRelease   := 100;
end;

class procedure TRangeGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Range Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Range Gate (Static)';
  end;
end;

function TRangeGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Range [dB]';
       VariableAddress := @FRange;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TRangeGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
  3: FDynamicProcesor.Range_dB     := FRange;
  4: FDynamicProcesor.Attack       := FAttack;
  5: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TRangeGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TRangeGateParamStaticSEModule }

class procedure TRangeGateParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Range Gate';
   ID := 'DAV Range Gate';
  end;
end;

function TRangeGateParamStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..5: with Properties^ do Direction := drIn;
 end;
end;

{ TRangeGateAutomatableSEModule }

class procedure TRangeGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Range Gate (Automatable)';
   ID := 'DAV Range Gate (Automatable)';
  end;
end;

function TRangeGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Range [dB]';
       VariableAddress := @FRange;
       Direction       := drParameter;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TRangeGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2..5: if (Pin[2].Status <> stRun) and
           (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun)
         then OnProcess := SubProcessStatic
         else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TRangeGateAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Range   : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Range   := PDAVSingleFixedArray(@FRange[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Range_dB := 10 * Range[Sample];
   FDynamicProcesor.Attack := 10 * Attack[Sample];
   FDynamicProcesor.Release := 10 * Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

procedure TRangeGateAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TCustomClassicGateSEModule }

constructor TCustomClassicGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TClassicGate.Create;
end;

destructor TCustomClassicGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomClassicGateSEModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TCustomClassicGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TClassicGateStaticSEModule }

constructor TClassicGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRelease   := 100;
end;

class procedure TClassicGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Classic Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Classic Gate (Static)';
  end;
end;

function TClassicGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TClassicGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
  3: FDynamicProcesor.Attack       := FAttack;
  4: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TClassicGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TClassicGateParamStaticSEModule }

class procedure TClassicGateParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Classic Gate';
   ID := 'DAV Classic Gate';
  end;
end;

function TClassicGateParamStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..4: with Properties^ do Direction := drIn;
 end;
end;

{ TClassicGateAutomatableSEModule }

class procedure TClassicGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Classic Gate (Automatable)';
   ID := 'DAV Classic Gate (Automatable)';
  end;
end;

function TClassicGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TClassicGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2..4: if (Pin[2].Status <> stRun) and
           (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun)
         then OnProcess := SubProcessStatic
         else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TClassicGateAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Attack := 10 * Attack[Sample];
   FDynamicProcesor.Release := 10 * Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

procedure TClassicGateAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TCustomSoftClassicGateSEModule }

constructor TCustomSoftClassicGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TClassicSoftKneeGate.Create;
end;

destructor TCustomSoftClassicGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomSoftClassicGateSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

procedure TCustomSoftClassicGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSoftClassicGateStaticSEModule }

constructor TSoftClassicGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB   :=   1;
 FAttack    :=  10;
 FRelease   := 100;
end;

class procedure TSoftClassicGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Classic Soft Knee Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Classic Soft Knee Gate (Static)';
  end;
end;

function TSoftClassicGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSoftClassicGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
  3: FDynamicProcesor.Knee_dB      := FKnee_dB;
  4: FDynamicProcesor.Attack       := FAttack;
  5: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TSoftClassicGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSoftClassicGateParamStaticSEModule }

class procedure TSoftClassicGateParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Classic Soft Knee Gate';
   ID := 'DAV Classic Soft Knee Gate';
  end;
end;

function TSoftClassicGateParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..5: with Properties^ do Direction := drIn;
 end;
end;

{ TSoftClassicGateAutomatableSEModule }

class procedure TSoftClassicGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Classic Soft Knee Gate (Automatable)';
   ID := 'DAV Classic Soft Knee Gate (Automatable)';
  end;
end;

function TSoftClassicGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TSoftClassicGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2..5: if (Pin[2].Status <> stRun) and
           (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSoftClassicGateAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Knee    : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Knee    := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Knee_dB := 10 * Knee[Sample];
   FDynamicProcesor.Attack := 10 * Attack[Sample];
   FDynamicProcesor.Release := 10 * Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

{ TCustomLimiterSEModule }

destructor TCustomLimiterSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomLimiterSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

function TCustomLimiterSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TCustomLimiterSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.AutoMakeUp := FAutoMakeUp;
 end;
 inherited;
end;

procedure TCustomLimiterSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TCustomSimpleLimiterSEModule }

constructor TCustomSimpleLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TLimiter.Create;
end;

{ TLimiterStaticSEModule }

constructor TLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRelease   := 100;
end;

class procedure TLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Limiter (Static)';
  end;
end;

function TLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Attack       := FAttack;
  5: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TLimiterParamStaticSEModule }

class procedure TLimiterParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Simple Limiter';
   ID := 'DAV Simple Limiter';
  end;
end;

function TLimiterParamStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..5: with Properties^ do Direction := drIn;
 end;
end;

{ TLimiterAutomatableSEModule }

class procedure TLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Simple Limiter (Automatable)';
   ID := 'DAV Simple Limiter (Automatable)';
  end;
end;

function TLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  3..5: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TLimiterAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Attack := 10 * Attack[Sample];
   FDynamicProcesor.Release := 10 * Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

{ TCustomSoftLimiterSEModule }

constructor TCustomSoftLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSoftKneeLimiter.Create;
end;

{ TSoftLimiterStaticSEModule }

constructor TSoftLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRelease   := 100;
 FKnee_dB   :=   1;
end;

class procedure TSoftLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Soft Knee Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Soft Knee Limiter (Static)';
  end;
end;

function TSoftLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSoftLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Knee_dB := FKnee_dB;
  5: FDynamicProcesor.Attack := FAttack;
  6: FDynamicProcesor.Release := FRelease;
 end;
 inherited;
end;

procedure TSoftLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSoftLimiterParamStaticSEModule }

class procedure TSoftLimiterParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Soft Knee Limiter';
   ID := 'DAV Soft Knee Limiter';
  end;
end;

function TSoftLimiterParamStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..6: with Properties^ do Direction := drIn;
 end;
end;

{ TSoftLimiterAutomatableSEModule }

class procedure TSoftLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Soft Knee Limiter (Automatable)';
   ID := 'DAV Soft Knee Limiter (Automatable)';
  end;
end;

function TSoftLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TSoftLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3..6: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSoftLimiterAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Knee    : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Knee    := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Knee_dB := 10 * Knee[Sample];
   FDynamicProcesor.Attack := 10 * Attack[Sample];
   FDynamicProcesor.Release := 10 * Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

{ TCustomSimpleSoftLimiterSEModule }

constructor TCustomSimpleSoftLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleSoftKneeLimiter.Create;
end;

{ TSimpleSoftLimiterStaticSEModule }

constructor TSimpleSoftLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRelease   := 100;
 FKnee_dB   :=   1;
end;

class procedure TSimpleSoftLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'SimpleSoft Knee Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV SimpleSoft Knee Limiter (Static)';
  end;
end;

function TSimpleSoftLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSimpleSoftLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Knee_dB := FKnee_dB;
  5: FDynamicProcesor.Attack := FAttack;
  6: FDynamicProcesor.Release := FRelease;
 end;
 inherited;
end;

procedure TSimpleSoftLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSimpleSoftLimiterParamStaticSEModule }

class procedure TSimpleSoftLimiterParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Simple Soft Knee Limiter';
   ID := 'DAV Simple Soft Knee Limiter';
  end;
end;

function TSimpleSoftLimiterParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..6: with Properties^ do Direction := drIn;
 end;
end;

{ TSimpleSoftLimiterAutomatableSEModule }

class procedure TSimpleSoftLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Simple Soft Knee Limiter (Automatable)';
   ID := 'DAV Simple Soft Knee Limiter (Automatable)';
  end;
end;

function TSimpleSoftLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TSimpleSoftLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3..6: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSimpleSoftLimiterAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Knee    : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Knee    := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Knee_dB      := 10 * Knee[Sample];
   FDynamicProcesor.Attack       := 10 * Attack[Sample];
   FDynamicProcesor.Release      := 10 * Release[Sample];
   Output^[Sample]               := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

{ TCustomCompressorSEModule }

destructor TCustomCompressorSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomCompressorSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

function TCustomCompressorSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TCustomCompressorSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.AutoMakeUp := FAutoMakeUp;
 end;
 inherited;
end;

procedure TCustomCompressorSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TCustomSimpleCompressorSEModule }

constructor TCustomSimpleCompressorSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleCompressor.Create;
end;

{ TSimpleCompressorStaticSEModule }

constructor TSimpleCompressorStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRatio     :=   8;
 FRelease   := 100;
end;

class procedure TSimpleCompressorStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Compressor (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Compressor (Static)';
  end;
end;

function TSimpleCompressorStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '8';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSimpleCompressorStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Ratio        := FRatio;
  5: FDynamicProcesor.Attack       := FAttack;
  6: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TSimpleCompressorStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSimpleCompressorParamStaticSEModule }

class procedure TSimpleCompressorParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Simple Compressor';
   ID := 'DAV Simple Compressor';
  end;
end;

function TSimpleCompressorParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..6: with Properties^ do Direction := drIn;
 end;
end;

{ TSimpleCompressorAutomatableSEModule }

class procedure TSimpleCompressorAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Simple Compressor (Automatable)';
   ID := 'DAV Simple Compressor (Automatable)';
  end;
end;

function TSimpleCompressorAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '8';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TSimpleCompressorAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  3..6: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSimpleCompressorAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Attack       := 10 * Attack[Sample];
   FDynamicProcesor.Release      := 10 * Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

{ TCustomSoftKneeCompressorSEModule }

constructor TCustomSoftKneeCompressorSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSoftKneeCompressor.Create;
end;

{ TSoftKneeCompressorStaticSEModule }

constructor TSoftKneeCompressorStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRatio     :=   8;
 FKnee      :=   1;
 FRelease   := 100;
end;

class procedure TSoftKneeCompressorStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Soft Knee Compressor (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Soft Knee Compressor (Static)';
  end;
end;

function TSoftKneeCompressorStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '8';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSoftKneeCompressorStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Ratio        := FRatio;
  5: FDynamicProcesor.Knee_dB      := FKnee;
  6: FDynamicProcesor.Attack       := FAttack;
  7: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TSoftKneeCompressorStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TSoftKneeCompressorParamStaticSEModule }

class procedure TSoftKneeCompressorParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Soft Knee Compressor';
   ID := 'DAV Soft Knee Compressor';
  end;
end;

function TSoftKneeCompressorParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..7: with Properties^ do Direction := drIn;
 end;
end;

{ TSoftKneeCompressorAutomatableSEModule }

class procedure TSoftKneeCompressorAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'Soft Knee Compressor (Automatable)';
   ID := 'DAV Soft Knee Compressor (Automatable)';
  end;
end;

function TSoftKneeCompressorAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '8';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TSoftKneeCompressorAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  3..7: if (Pin[3].Status > stStop) and
           (Pin[4].Status > stStop) and
           (Pin[5].Status > stStop) and
           (Pin[6].Status > stStop) and
           (Pin[7].Status > stStop)
         then OnProcess := SubProcessAutomated
         else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TSoftKneeCompressorAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Knee    : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Knee    := PDAVSingleFixedArray(@FKnee[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := 10 * Thres[Sample];
   FDynamicProcesor.Knee_dB      := 10 * Knee[Sample];
   FDynamicProcesor.Attack       := 10 * Attack[Sample];
   FDynamicProcesor.Release      := 10 * Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
  end;
end;

{ TCustomRMSCompressorSEModule }

constructor TCustomRMSCompressorSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleRMSCompressor.Create;
end;

{ TRMSCompressorStaticSEModule }

constructor TRMSCompressorStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRatio     :=   8;
 FRMSTime   :=   1;
 FRelease   := 100;
end;

class procedure TRMSCompressorStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'RMS Compressor (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV RMS Compressor (Static)';
  end;
end;

function TRMSCompressorStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '8';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'RMS Time [ms]';
       VariableAddress := @FRMSTime;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TRMSCompressorStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered dynamics parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Ratio        := FRatio;
  5: TSimpleRMSCompressor(FDynamicProcesor).RMSTime := FRMSTime;
  6: FDynamicProcesor.Attack       := FAttack;
  7: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TRMSCompressorStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
end;

{ TRMSCompressorParamStaticSEModule }

class procedure TRMSCompressorParamStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   Name := 'RMS Compressor';
   ID := 'DAV RMS Compressor';
  end;
end;

function TRMSCompressorParamStaticSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2..7: with Properties^ do Direction := drIn;
 end;
end;

{ TRMSCompressorAutomatableSEModule }

class procedure TRMSCompressorAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'RMS Compressor (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV RMS Compressor (Automatable)';
  end;
end;

function TRMSCompressorAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case Index of
  2: with Properties^ do Direction := drIn;
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '-10';
       Result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '8';
       Result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'RMS time [ms]';
       VariableAddress := @FRMSTime;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '1';
       Result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '10';
       Result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtFSample;
       DefaultValue    := '100';
       Result          := True;
      end;
 end;
end;

procedure TRMSCompressorAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  3..7: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun) and
           (Pin[7].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TRMSCompressorAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input    : PDAVSingleFixedArray;
  Output   : PDAVSingleFixedArray;
  DThres   : PDAVSingleFixedArray;
  DRMSTime : PDAVSingleFixedArray;
  DAttack  : PDAVSingleFixedArray;
  DRelease : PDAVSingleFixedArray;
  Sample   : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input    := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output   := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 DThres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 DRMSTime := PDAVSingleFixedArray(@FRMSTime[BufferOffset]);
 DAttack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 DRelease := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  with TSimpleRMSCompressor(FDynamicProcesor) do
   begin
    Threshold_dB := 10 * DThres[Sample];
    RMSTime := 10 * DRMSTime[Sample];
    Attack := 10 * DAttack[Sample];
    Release := 10 * DRelease[Sample];
    Output^[Sample] := FDynamicProcesor.ProcessSample64(Input[Sample])
   end;
end;

end.
