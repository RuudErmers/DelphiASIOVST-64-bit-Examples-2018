unit DAV_DspComponents;

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
//  The code is based on the mda VST plug-ins by Paul Kellett, which is       //
//  located at http://sourceforge.net/projects/mda-vst/                       //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Classes, DAV_DspAmbience, DAV_DspAudioToMidiTrigger,
  DAV_DspBarberpole, DAV_DspBarberpoleTuner, DAV_DspChorus, DAV_DspConvolution,
  DAV_DspCrosstalkCancellation, DAV_DspCrosstalkSimulator,
  DAV_DspDitherNoiseshaper, DAV_DspExciter;

type
  TCustomDspComponent = class(TComponent)
  end;

  TAmbienceComponent = class(TCustomDspComponent)
  protected
    FAmbience : TAmbience;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Ambience: TAmbience read FAmbience;
  end;

  TAudio2MidiTriggerComponent = class(TCustomDspComponent)
  protected
    FAudio2MidiTrigger : TAudio2MidiTrigger;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Audio2MidiTrigger: TAudio2MidiTrigger read FAudio2MidiTrigger;
  end;

  TBarberpoleComponent = class(TCustomDspComponent)
  protected
    FBarberpole : TDspBarberpole32;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Barberpole: TDspBarberpole32 read FBarberpole;
  end;

  TBarberpoleFilterComponent = class(TCustomDspComponent)
  protected
    FBarberpoleFilter : TBarberpoleFilter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BarberpoleFilter: TBarberpoleFilter read FBarberpoleFilter;
  end;

  TBarberpoleTunerComponent = class(TCustomDspComponent)
  protected
    FBarberpoleTuner : TBarberpoleTuner;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BarberpoleTuner: TBarberpoleTuner read FBarberpoleTuner;
  end;

  TChorusComponent = class(TCustomDspComponent)
  protected
    FChorus : TDspChorus32;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Chorus: TDspChorus32 read FChorus;
  end;

  TConvolutionComponent = class(TCustomDspComponent)
  protected
    FConvolution : TConvolution;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Convolution: TConvolution read FConvolution;
  end;

  TCrosstalkCancellationComponent = class(TCustomDspComponent)
  protected
    FCrosstalkCancellation : TCrosstalkCancellation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CrosstalkCancellation: TCrosstalkCancellation read FCrosstalkCancellation;
  end;

  TCrosstalkSimulatorComponent = class(TCustomDspComponent)
  protected
    FCrosstalkSimulator : TCrosstalkSimulator;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CrosstalkSimulator: TCrosstalkSimulator read FCrosstalkSimulator;
  end;

  TDitherNoiseshaperComponent = class(TCustomDspComponent)
  protected
    FDitherNoiseshaper : TDitherNoiseshaper;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DitherNoiseshaper: TDitherNoiseshaper read FDitherNoiseshaper;
  end;

  TExciterComponent = class(TCustomDspComponent)
  protected
    FExciter : TExciter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Exciter: TExciter read FExciter;
  end;


procedure Register;

implementation

uses
  SysUtils;


{ TAmbienceComponent }

constructor TAmbienceComponent.Create(AOwner: TComponent);
begin
 inherited;
 FAmbience := TAmbience.Create;
end;

destructor TAmbienceComponent.Destroy;
begin
 FreeAndNil(FAmbience);
 inherited;
end;


{ TAudio2MidiTriggerComponent }

constructor TAudio2MidiTriggerComponent.Create(AOwner: TComponent);
begin
 inherited;
 FAudio2MidiTrigger := TAudio2MidiTrigger.Create;
end;

destructor TAudio2MidiTriggerComponent.Destroy;
begin
 FreeAndNil(FAudio2MidiTrigger);
 inherited;
end;


{ TBarberpoleComponent }

constructor TBarberpoleComponent.Create(AOwner: TComponent);
begin
 inherited;
 FBarberpole := TDspBarberpole32.Create;
end;

destructor TBarberpoleComponent.Destroy;
begin
 FreeAndNil(FBarberpole);
 inherited;
end;


{ TBarberpoleFilterComponent }

constructor TBarberpoleFilterComponent.Create(AOwner: TComponent);
begin
 inherited;
 FBarberpoleFilter := TBarberpoleFilter.Create;
end;

destructor TBarberpoleFilterComponent.Destroy;
begin
 FreeAndNil(FBarberpoleFilter);
 inherited;
end;


{ TBarberpoleTunerComponent }

constructor TBarberpoleTunerComponent.Create(AOwner: TComponent);
begin
 inherited;
 FBarberpoleTuner := TBarberpoleTuner.Create;
end;

destructor TBarberpoleTunerComponent.Destroy;
begin
 FreeAndNil(FBarberpoleTuner);
 inherited;
end;


{ TChorusComponent }

constructor TChorusComponent.Create(AOwner: TComponent);
begin
 inherited;
 FChorus := TDspChorus32.Create;
end;

destructor TChorusComponent.Destroy;
begin
 FreeAndNil(FChorus);
 inherited;
end;


{ TConvolutionComponent }

constructor TConvolutionComponent.Create(AOwner: TComponent);
begin
 inherited;
 FConvolution := TConvolution.Create;
end;

destructor TConvolutionComponent.Destroy;
begin
 FreeAndNil(FConvolution);
 inherited;
end;


{ TCrosstalkCancellationComponent }

constructor TCrosstalkCancellationComponent.Create(AOwner: TComponent);
begin
 inherited;
 FCrosstalkCancellation := TCrosstalkCancellation.Create;
end;

destructor TCrosstalkCancellationComponent.Destroy;
begin
 FreeAndNil(FCrosstalkCancellation);
 inherited;
end;


{ TCrosstalkSimulatorComponent }

constructor TCrosstalkSimulatorComponent.Create(AOwner: TComponent);
begin
 inherited;
 FCrosstalkSimulator := TCrosstalkSimulator.Create;
end;

destructor TCrosstalkSimulatorComponent.Destroy;
begin
 FreeAndNil(FCrosstalkSimulator);
 inherited;
end;


{ TDitherNoiseshaperComponent }

constructor TDitherNoiseshaperComponent.Create(AOwner: TComponent);
begin
 inherited;
 FDitherNoiseShaper := TDitherNoiseShaper.Create;
end;

destructor TDitherNoiseshaperComponent.Destroy;
begin
 FreeAndNil(FDitherNoiseShaper);
 inherited;
end;


{ TExciterComponent }

constructor TExciterComponent.Create(AOwner: TComponent);
begin
 inherited;
 FExciter := TExciter.Create;
end;

destructor TExciterComponent.Destroy;
begin
 FreeAndNil(FExciter);
 inherited;
end;


procedure Register;
begin
 RegisterComponents('ASIO/VST DSP', [TAmbienceComponent,
   TAudio2MidiTriggerComponent, TBarberpoleComponent,
   TBarberpoleFilterComponent, TBarberpoleTunerComponent, TChorusComponent,
   TConvolutionComponent, TCrosstalkCancellationComponent,
   TCrosstalkSimulatorComponent, TDitherNoiseshaperComponent,
   TExciterComponent]);
end;

end.
