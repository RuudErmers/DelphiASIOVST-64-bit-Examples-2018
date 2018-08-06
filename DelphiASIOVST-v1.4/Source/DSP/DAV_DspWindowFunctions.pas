unit DAV_DspWindowFunctions;

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
  Classes, DAV_Classes, DAV_Types, DAV_DspWindowing;

type
  TWindowSlope  = (wsLeft, wsSymmetric, wsRight);

  TWindowFunctionClass = class of TCustomWindowFunction;

  TCosForwardLoop32   = procedure (Data: PDAVSingleFixedArray; var ParamRec: TParameterRecord; SampleCount : Integer);
  TCosForwardLoop64   = procedure (Data: PDAVDoubleFixedArray; var ParamRec: TParameterRecord; SampleCount : Integer);
  TCosSymmetricLoop32 = procedure (Data: PDAVSingleFixedArray; var ParamRec: TParameterRecord; SampleCount : Integer; EndAdr : PDAVSingleFixedArray);
  TCosSymmetricLoop64 = procedure (Data: PDAVDoubleFixedArray; var ParamRec: TParameterRecord; SampleCount : Integer; EndAdr : PDAVDoubleFixedArray);

  TCustomWindowFunction = class(TDspPersistent)
  private
    procedure SetWinLength(Value: Integer);
    procedure SetWinStart(Value: Integer);
    procedure SetTukey(Value: Single);
    procedure SetBartlett(const Value: Boolean);
  protected
    FBandwidth     : Double;                // Bandwidth (in Bins)
    FBartlett      : Boolean;               // Bartlett
    FEffLength     : Double;                // including Slope, Tukey
    FEffLengthReci : Double;                // reciprocal effective length
    FEffLengthRnd  : Integer;               //  -> effective length in sample
    FFirstMinimum  : Double;                // First Minimum (in Bins)
    FInvert        : Boolean;               // invert window?
    FLength        : Integer;               // window length
    FSidelobe      : Double;                // Highest Sidelobe [dB]
    FSpkCorFak     : Double;                // spectrum correction factor
    FSpkCorFakSq   : Double;                // squared spectrum correction factor
    FStart         : Integer;               // window start
    FTukey         : Single;                // Tukey factor in %
    FWinSlope      : TWindowSlope;          // window slope (see above)
    FZeroDC        : Boolean;               // apply colva DC remove?
    function GetBandwidth   : Single; virtual;
    function GetMinimum     : Single; virtual;
    function GetSideLobe    : Single; virtual;
    function GetSpkCorFak   : Double; virtual;
    function GetSpkCorFakSq : Double; virtual;
    function GetWindowFactor(Pos: Double): Double; virtual; abstract;
    procedure CalcEffectiveLength; virtual;
    procedure SetWinSlope(const Value: TWindowSlope);  virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer); virtual; abstract;
    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer); virtual; abstract;
    procedure CalculateWindowInfo; virtual;

    class function GetWindowFunctionName: AnsiString; virtual; abstract;
  published
    property Start: Integer read FStart write SetWinStart default 0;      // Window Start
    property Length: Integer read FLength write SetWinLength;             // Window Length
    property ZeroDC: Boolean read FZeroDC write FZeroDC default False;    // Colva DC removal after windowing
    property Tukey: Single read FTukey write SetTukey;                    // Tukey percentage
    property Bartlett: Boolean read FBartlett write SetBartlett default False;           // Bartlett
    property Slope: TWindowSlope read FWinSlope write SetWinSlope default wsSymmetric;   // Slope left, sym, right
    property Invert: Boolean read FInvert write FInvert default False;    // Inverse window
    property SpectrumCorrectionFaktor: Double read GetSpkCorFak;          // Calculates the Correction Factor or delivers last
    property CoherentGain: Double read GetSpkCorFakSq;                    // Calculates the squared Correction Factor or delivers last

    property SideLobe : Single read GetSideLobe;                          // SideLobe
    property Bandwidth : Single read GetBandwidth;                        // Bandwidth in Bins
    property FirstMinimum : Single read GetMinimum;                       // First Minimum In Bins
  end;

  TWindowFunctionCustomFactor = class(TCustomWindowFunction)
  public
    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
  end;

  TWindowFunctionRectangle = class(TCustomWindowFunction)
  protected
    function GetWindowFactor(Pos: Double): Double; override;
  public
    constructor Create; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer); override;

    class function GetWindowFunctionName: AnsiString; override;
  end;

  TWindowFunctionTriangle = class(TCustomWindowFunction)
  protected
    function GetWindowFactor(Pos: Double): Double; override;
    procedure CalcEffectiveLength; override;
  public
    constructor Create; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer); override;

    class function GetWindowFunctionName: AnsiString; override;
  end;

  TWindowFunctionCosine = class(TCustomWindowFunction)
  private
  protected
    function GetWindowFactor(Pos: Double): Double; override;
  public
    constructor Create; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer); override;

    class function GetWindowFunctionName: AnsiString; override;
  end;

  TWindowFunctionWithParameter = class(TWindowFunctionCustomFactor)
  private
    procedure SetAlpha(const Value: Single);
  protected
    FAlpha: Single;
    procedure AlphaChanged; virtual;
  published
    property Alpha : Single read FAlpha write SetAlpha;
  end;

  TWindowFunctionLanczos = class(TWindowFunctionWithParameter)
  protected
    function GetWindowFactor(Pos: Double): Double; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;

    class function GetWindowFunctionName: AnsiString; override;
  end;

  TWindowFunctionHanning = class(TCustomWindowFunction)
  private
  protected
    function GetWindowFactor(Pos: Double): Double; override;
  public
    constructor Create; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer); override;

    class function GetWindowFunctionName: AnsiString; override;
  end;

  TWindowFunctionCosineTerm = class(TCustomWindowFunction)
  private
    FCosineTerms  : Integer;
    FCosFwdLoop32 : TCosForwardLoop32;
    FCosFwdLoop64 : TCosForwardLoop64;
    FCosSymLoop32 : TCosSymmetricLoop32;
    FCosSymLoop64 : TCosSymmetricLoop64;
  protected
    FCoefPointer  : PDAVDoubleFixedArray;
    function GetWindowFactor(Pos: Double): Double; override;
    procedure SetCosineTerms(Value: Integer); virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer); override;

    property CosineTerms : Integer read FCosineTerms write SetCosineTerms;
  published
  end;

  TWindowFunctionHamming = class(TWindowFunctionCosineTerm)
  public
    constructor Create; override;
    class function GetWindowFunctionName: AnsiString; override;
  end;

  TWindowFunctionBlackman = class(TWindowFunctionCosineTerm)
  public
    constructor Create; override;
    class function GetWindowFunctionName: AnsiString; override;
  end;

  TWindowFunctionWelch = class(TCustomWindowFunction)
  protected
    function GetWindowFactor(Pos: Double): Double; override;
  public
    constructor Create; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer); override;

    class function GetWindowFunctionName: AnsiString; override;
  end;

var
  GWindowFunctions: array of TWindowFunctionClass;

procedure RegisterWindowFunction(AClass: TWindowFunctionClass);
procedure RegisterWindowFunctions(AClasses: array of TWindowFunctionClass);

implementation

uses
  Math, SysUtils, DAV_Common, DAV_Math, DAV_BlockProcessing;

resourcestring
  RCStrWindowDuplicate = 'Window function registered twice!';

{ TCustomWindowFunction }

constructor TCustomWindowFunction.Create;
begin
 FStart        := 0;
 FLength       := 0;
 FInvert       := False;
 FWinSlope     := wsSymmetric;
 FZeroDC       := False;
 FTukey        := 0;
 FSpkCorFak    := -1;
 FSidelobe     := 0;
 FFirstMinimum := 0;
 FBandwidth    := 0;
 FBartlett     := True;
end;

destructor TCustomWindowFunction.Destroy;
begin
 // Do nothing!
end;

procedure TCustomWindowFunction.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomWindowFunction then
  begin
   TCustomWindowFunction(Dest).FBandwidth      := FBandwidth;
   TCustomWindowFunction(Dest).FBartlett       := FBartlett;
   TCustomWindowFunction(Dest).FEffLength      := FEffLength;
   TCustomWindowFunction(Dest).FEffLengthReci  := FEffLengthReci;
   TCustomWindowFunction(Dest).FEffLengthRnd   := FEffLengthRnd;
   TCustomWindowFunction(Dest).FFirstMinimum   := FFirstMinimum;
   TCustomWindowFunction(Dest).FInvert         := FInvert;
   TCustomWindowFunction(Dest).FLength         := FLength;
   TCustomWindowFunction(Dest).FSidelobe       := FSidelobe;
   TCustomWindowFunction(Dest).FSpkCorFak      := FSpkCorFak;
   TCustomWindowFunction(Dest).FSpkCorFakSq    := FSpkCorFakSq;
   TCustomWindowFunction(Dest).FStart          := FStart;
   TCustomWindowFunction(Dest).FTukey          := FTukey;
   TCustomWindowFunction(Dest).FWinSlope       := FWinSlope;
   TCustomWindowFunction(Dest).FZeroDC         := FZeroDC;
  end else inherited;
end;

procedure TCustomWindowFunction.CalcEffectiveLength;
begin
 // Slope
 if FWinSlope = wsSymmetric
  then
   begin
    FEffLength    := (0.5 - 0.005 * FTukey) * FLength;
    FEffLengthRnd := Round(FEffLength - 0.49999999999);  // FPU friendly trunc
    if FEffLength > 0.5 then
     if FBartlett
      then FEffLengthReci := 1 /  FEffLength
      else FEffLengthReci := 1 / (FEffLength - 0.5)
     else FEffLengthReci := 1;
   end
  else
   begin
    FEffLength    := (1 - 0.01 * FTukey) * FLength;
    FEffLengthRnd := Round(FEffLength);
    if FEffLength > 0 then
     if FBartlett
      then FEffLengthReci := 1 /  FEffLength
      else FEffLengthReci := 1 / (FEffLength - 1)
     else FEffLengthReci := 1;
   end;
end;

procedure TCustomWindowFunction.CalculateWindowInfo;
var
  i            : Integer;
  TempTab      : PDAVDoubleFixedArray;
(*
  {$IFNDEF PUREPASCAL}
  DFT64Res     : TDFT64Results;
  Freq         : Double;
  FrqInc       : Double;
  DCReference  : Double;
  Reference    : Double;
  CurrentValue : Double;
  DirUp        : Boolean;
  SidelobeFreq : Double;
  {$ENDIF}
*)
begin
 GetMem(TempTab, FLength * SizeOf(Double));
 try
  for i := 0 to FLength - 1
   do TempTab[i] := 1;
  ProcessBlock64(@TempTab[0], FLength);

(*
  {$IFNDEF PUREPASCAL}
  FillChar(DFT64Res, SizeOf(TDFT64Results), 0);
  DCReference := Sqr(SumOfF64(@TempTab[0], FLength));

  Reference   := 1 / DCReference;

  // nummerical search of bandwidth
  DirUp        := True;
  Freq         := 0.5 / FLength;
  FrqInc       := Freq;

  F64DFT(Freq, @DFT64Res, @TempTab[0], 0, FLength);
  CurrentValue := (sqr(DFT64Res.Re) + sqr(DFT64Res.Im)) * Reference;

  for i := 0 to 100 do
   begin
    if DirUp then
     if (CurrentValue < 0.5) then
      begin
       DirUp  := False;
       FrqInc := -0.5 * FrqInc;
      end else else
     if (CurrentValue > 0.5) then
      begin
       DirUp  := True;
       FrqInc := -0.5 * FrqInc;
      end;
    Freq := Freq + FrqInc;
    if Freq > 1 then Freq := Freq - 1;
    F64DFT(Freq, @DFT64Res, @TempTab[0], 0, FLength);
    CurrentValue := (sqr(DFT64Res.Re) + sqr(DFT64Res.Im)) * Reference;
   end;
  FBandwidth := 2 * Freq * FLength;

  // nummerical search of first minimum
  Freq          := 1 / FLength;                   // second Bin as normed frequency
  FrqInc        := 1 / 32 * Freq;                 // initial scan width
  FFirstMinimum := Freq;                          // absolute first minimum bin

  F64DFT(Freq, @DFT64Res, @TempTab[0], 0, FLength);
  CurrentValue := sqr(DFT64Res.Re) + sqr(DFT64Res.Im);
  Reference     := 1 / Reference;                 // Reference equals CurrentValue
  for i := 0 to 10000 do
   begin
    if CurrentValue < Reference
     then
      begin
       Reference := CurrentValue;
       FFirstMinimum := 2 * Freq * FLength;
      end
     else
      if FrqInc < 1E-6
       then FrqInc := -0.1 * FrqInc
       else
        begin
         Freq   := Freq - FrqInc;
         FrqInc := 0.2 * FrqInc;
        end;
    Freq := Freq + FrqInc;

    F64DFT(Freq, @DFT64Res, @TempTab[0], 0, FLength);
    CurrentValue := sqr(DFT64Res.Re) + sqr(DFT64Res.Im);

    if FrqInc > 1E-6 then FrqInc := 0.8 * FrqInc;
   end;

  // nummerical search of highest sidelobe
  Freq             := FFirstMinimum * 0.5 / FLength;
  SidelobeFreq     := Freq;
  FrqInc           := 1 / (FLength * 32);
  Reference        := CurrentValue;
  while Freq < 0.5 do
   begin
    F64DFT(Freq, @DFT64Res, @TempTab[0], 0, FLength);
    CurrentValue := (sqr(DFT64Res.Re) + sqr(DFT64Res.Im));
    Freq           := Freq + FrqInc;
    if CurrentValue > Reference then
     begin
      Reference    := CurrentValue;
      SidelobeFreq := Freq;
     end;
   end;

  Freq   := SidelobeFreq - FrqInc;
  FrqInc := FrqInc / 16;
  for i := 0 to 32 do
   begin
    F64DFT(Freq, @DFT64Res, @TempTab[0], 0, FLength);
    CurrentValue := (sqr(DFT64Res.Re) + sqr(DFT64Res.Im));
    Freq         := Freq + FrqInc;
    if CurrentValue > Reference
     then Reference := CurrentValue;
   end;

  FSidelobe := 0.5 * Amp_to_dB(Reference / DCReference);
  {$ENDIF}
*)
 finally
  Dispose(TempTab);
 end;
end;

function TCustomWindowFunction.GetBandwidth: Single;
begin
 Result := FBandwidth;
end;

function TCustomWindowFunction.GetMinimum: Single;
begin
 Result := FFirstMinimum;
end;

function TCustomWindowFunction.GetSideLobe: Single;
begin
 Result := FSidelobe;
end;

function TCustomWindowFunction.GetSpkCorFak: Double;
var i           : Integer;
    CurWinVal   : Double;
begin
 if FSpkCorFak<0 then
  begin
   // No correction factor specified yet, calculate!
   FSpkCorFak   := 0;
   FSpkCorFakSq := 0;
   CurWinVal    := 0;
   for i := 0 to FEffLengthRnd- 1 do
    begin
     CurWinVal    := GetWindowFactor(i * FEffLengthReci);
     FSpkCorFak   := FSpkCorFak + CurWinVal;
     FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
    end;
   if FWinSlope=wsSymmetric then // apply left & right
    begin
     FSpkCorFak   := 2 * FSpkCorFak;
     FSpkCorFakSq := 2 * FSpkCorFakSq;
     if (Round(2 * FEffLength - 0.5) mod 2) > 0 then // remove sample in the middle if necessary
      begin
       FSpkCorFak   := FSpkCorFak - CurWinVal;
       FSpkCorFakSq := FSpkCorFakSq - CurWinVal * CurWinVal;
      end;
    end;
   FSpkCorFakSq := Sqrt(FEffLength / FSpkCorFakSq);
  end;
 Result := FSpkCorFak;
end;

function TCustomWindowFunction.GetSpkCorFakSq: Double;
var
  i         : Integer;
  CurWinVal : Double;
begin
 if FSpkCorFakSq < 0 then
  begin
   FSpkCorFak   := 0;
   FSpkCorFakSq := 0;
   CurWinVal    := 0;
   for i := 0 to FEffLengthRnd - 1 do
    begin
     CurWinVal := GetWindowFactor(i * FEffLengthReci);
     FSpkCorFak := FSpkCorFak + CurWinVal;
     FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
    end;
   if FWinSlope = wsSymmetric then // apply left & right
    begin
     FSpkCorFak := 2 * FSpkCorFak;
     FSpkCorFakSq := 2 * FSpkCorFakSq;
     if (Round(2 * FEffLength - 0.5) mod 2) > 0 then // remove sample in the middle if necessary
      begin
       FSpkCorFak := FSpkCorFak - CurWinVal;
       FSpkCorFakSq := FSpkCorFakSq - CurWinVal * CurWinVal;
      end;
    end;
  end;
 Result := FSpkCorFakSq;
end;

procedure TCustomWindowFunction.SetTukey(Value: Single);
begin
 if Value < 0 then Value := 0 else
 if Value > 100 then Value := 100;
 if FTukey <> Value then
  begin
   FTukey := Value;
   CalcEffectiveLength;
  end;
end;

procedure TCustomWindowFunction.SetWinLength(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FLength <> Value then
  begin
   FLength := Value;
   CalcEffectiveLength;
  end;
end;

procedure TCustomWindowFunction.SetWinStart(Value: Integer);
begin
 if FStart <> Value then
  begin
   FStart := Value;
  end;
end;

procedure TCustomWindowFunction.SetWinSlope(const Value: TWindowSlope);
begin
 if FWinSlope<>Value then
  begin
   FWinSlope := Value;
   CalcEffectiveLength;
  end;
end;

{ TWindowFunctionCustomFactor }

procedure TWindowFunctionCustomFactor.ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  i, p1, p2 : Integer;
  Counter   : Integer;
  StartPos  : Integer;
  EndPos    : Integer;
  CurWinVal : Double;
begin
 if (SampleCount <= 0) then Exit;
 if FLength > 2 * SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := FStart + Length - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := (FStart + Length - 1);

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 FSpkCorFak := 0;
 FSpkCorFakSq := 0;

 case FWinSlope of
   wsLeft:
     begin
      Counter := 0;
      if StartPos + FEffLengthRnd <= SampleCount then
       for i := StartPos to StartPos + FEffLengthRnd - 1 do
        begin
         CurWinVal := GetWindowFactor(Counter * FEffLengthReci);
         FSpkCorFak := FSpkCorFak + CurWinVal;
         FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
         Data[i] := Data[i] * CurWinVal;
         Inc(Counter);
        end
       else // Wrap arround
        begin
         for i := StartPos to SampleCount - 1 do
          begin
           CurWinVal := GetWindowFactor(Counter * FEffLengthReci);
           FSpkCorFak := FSpkCorFak + CurWinVal;
           FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
           Data[i] := Data[i] * CurWinVal;
           Inc(Counter);
          end;
         for i := 0 to FEffLengthRnd - (SampleCount - StartPos) - 1 do
          begin
           CurWinVal := GetWindowFactor(Counter * FEffLengthReci);
           FSpkCorFak := FSpkCorFak + CurWinVal;
           FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
           Data[i] := Data[i] * CurWinVal;
           Inc(Counter);
          end
        end;
      FSpkCorFak := FSpkCorFak + FLength - FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - FEffLengthRnd;
     end;
   wsSymmetric:
     begin
      if (StartPos + FEffLengthRnd<SampleCount) and (EndPos - FEffLengthRnd >= 0) then
       begin // No wrap arround!
        p1 := StartPos;
        p2 := EndPos;
        for i :=  0 to FEffLengthRnd - 1 do
         begin
          CurWinVal := GetWindowFactor(i * FEffLengthReci);
          FSpkCorFak := FSpkCorFak + CurWinVal + CurWinVal;
          FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
          Data[p1] := Data[p1] * CurWinVal;
          Data[p2] := Data[p2] * CurWinVal;
          Inc(p1);
          Dec(p2);
         end;
       end
       else // Wrap around
        begin
         if (StartPos + FEffLengthRnd<=SampleCount) then
          begin // Only right wrap around!
           P1 := EndPos + 1;
           for i := 0 to P1 - 1 do
            begin
             CurWinVal := GetWindowFactor(i*FEffLengthReci);
             FSpkCorFak := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
             Data[StartPos + i] :=  Data[StartPos + i] * CurWinVal;
             Data[EndPos -i]  := Data[EndPos - i] * CurWinVal;
            end;
           for i := P1 to FEffLengthRnd - 1 do
            begin
             CurWinVal := GetWindowFactor(i*FEffLengthReci);
             FSpkCorFak := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
             Data[StartPos + i] :=  Data[StartPos + i] * CurWinVal;
             Data[SampleCount-i + P1-1]  := Data[SampleCount-i + P1-1] * CurWinVal;
            end;
          end else
         if (EndPos - FEffLengthRnd >= 0) then
          begin // Only left wrap around!
           P1 := SampleCount - StartPos;
           for i := 0 to P1 - 1 do
            begin
             CurWinVal := GetWindowFactor(i*FEffLengthReci);
             FSpkCorFak := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
             Data[StartPos + i] :=  Data[StartPos + i] * CurWinVal;
             Data[EndPos - i]  := Data[EndPos - i] * CurWinVal;
            end;
           for i := P1 to FEffLengthRnd - 1 do
            begin
             CurWinVal := GetWindowFactor(i*FEffLengthReci);
             FSpkCorFak := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
             Data[i-P1] :=  Data[i-P1] * CurWinVal;
             Data[EndPos - i]  := Data[EndPos - i] * CurWinVal;
            end;
          end
         else
          begin
           p1 := StartPos;
           p2 := EndPos;
           for i :=  0 to FEffLengthRnd - 1 do
            begin
             CurWinVal := GetWindowFactor(i*FEffLengthReci);
             FSpkCorFak := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
             Data[p1] := Data[p1] * CurWinVal;
             Data[p2] := Data[p2] * CurWinVal;
             inc(p1); if p1 >= SampleCount then p1 := 0;
             Dec(p2); if p2 < 0 then p2 := SampleCount;
            end;
          end;
        end;
      FSpkCorFak := FSpkCorFak + FLength - 2 * FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - 2 * FEffLengthRnd;
     end;
   wsRight:
     begin
      Counter := 0;
      if EndPos - FEffLengthRnd >= 0 then
       for i := EndPos downto EndPos - FEffLengthRnd do
        begin
         CurWinVal := GetWindowFactor(Counter*FEffLengthReci);
         FSpkCorFak := FSpkCorFak + CurWinVal;
         FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
         Data[i] := Data[i] * CurWinVal;
         inc(Counter);
        end
       else // Wrap arround
        begin
         for i := EndPos downto 0 do
          begin
           CurWinVal := GetWindowFactor(Counter * FEffLengthReci);
           FSpkCorFak := FSpkCorFak + CurWinVal;
           FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
           Data[i] := Data[i] * CurWinVal;
           inc(Counter);
          end;
         for i := SampleCount downto SampleCount-(FEffLengthRnd - EndPos) do
          begin
           CurWinVal := GetWindowFactor(Counter * FEffLengthReci);
           FSpkCorFak := FSpkCorFak + CurWinVal;
           FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
           Data[i] := Data[i] * CurWinVal;
           inc(Counter);
          end
        end;
      FSpkCorFak := FSpkCorFak + FLength - FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - FEffLengthRnd;
     end;
 end;

 FSpkCorFakSq := Sqrt(FEffLength / FSpkCorFakSq);
 FillWithZeroes(Data, StartPos, EndPos, SampleCount);
end;

procedure TWindowFunctionCustomFactor.ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  CurWinVal : Double;
  i, p1, p2 : Integer;
  Counter   : Integer;
  StartPos  : Integer;
  EndPos    : Integer;
begin
 if (SampleCount <= 0) then exit;
 if FLength > 2 * SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;
 if FInvert
  then StartPos := (FStart + Length) - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := FStart + Length - 1;

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 FSpkCorFak := 0;
 FSpkCorFakSq := 0;

 case FWinSlope of
   wsLeft:
     begin
      Counter := 0;
      if StartPos + FEffLengthRnd <= SampleCount then
       for i := StartPos to StartPos + FEffLengthRnd - 1 do
        begin
         CurWinVal    := GetWindowFactor(Counter * FEffLengthReci);
         FSpkCorFak   := FSpkCorFak + CurWinVal;
         FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
         Data[i]  := Data[i] * CurWinVal;
         inc(Counter);
        end
       else // Wrap arround
        begin
         for i := StartPos to SampleCount - 1 do
          begin
           CurWinVal    := GetWindowFactor(Counter * FEffLengthReci);
           FSpkCorFak   := FSpkCorFak + CurWinVal;
           FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
           Data[i]  := Data[i] * CurWinVal;
           inc(Counter);
          end;
         for i := 0 to FEffLengthRnd - (SampleCount - StartPos) - 1 do
          begin
           CurWinVal    := GetWindowFactor(Counter * FEffLengthReci);
           FSpkCorFak   := FSpkCorFak + CurWinVal;
           FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
           Data[i]  := Data[i] * CurWinVal;
           inc(Counter);
          end
        end;
      FSpkCorFak   := FSpkCorFak   + FLength - FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - FEffLengthRnd;
     end;
   wsSymmetric:
     begin
      if (StartPos + FEffLengthRnd < SampleCount) and (EndPos - FEffLengthRnd >= 0) then
       begin // No wrap arround!
        p1 := StartPos;
        p2 := EndPos;
        for i :=  0 to FEffLengthRnd - 1 do
         begin
          CurWinVal    := GetWindowFactor(i * FEffLengthReci);
          FSpkCorFak   := FSpkCorFak + CurWinVal + CurWinVal;
          FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
          Data[p1] := Data[p1] * CurWinVal;
          Data[p2] := Data[p2] * CurWinVal;
          inc(p1); Dec(p2);
         end;
       end
       else // Wrap around
        begin
         if (EndPos - FEffLengthRnd >= 0) then
          begin // Only left wrap around!
           P1 := SampleCount - StartPos;
           for i := 0 to P1 - 1 do
            begin
             CurWinVal := GetWindowFactor(i*FEffLengthReci);
             FSpkCorFak := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
             Data[StartPos + i] :=  Data[StartPos + i] * CurWinVal;
             Data[EndPos - i]  := Data[EndPos - i] * CurWinVal;
            end;
           for i := P1 to FEffLengthRnd - 1 do
            begin
             CurWinVal := GetWindowFactor(i*FEffLengthReci);
             FSpkCorFak := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
             Data[i-P1] :=  Data[i-P1] * CurWinVal;
             Data[EndPos - i]  := Data[EndPos - i] * CurWinVal;
            end;
          end else
         if (StartPos + FEffLengthRnd <= SampleCount) then
          begin // Only right wrap around!
           P1 := EndPos + 1;
           for i := 0 to P1 - 1 do
            begin
             CurWinVal := GetWindowFactor(i*FEffLengthReci);
             FSpkCorFak := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
             Data[StartPos + i] :=  Data[StartPos + i] * CurWinVal;
             Data[EndPos - i]  := Data[EndPos - i] * CurWinVal;
            end;
           for i := P1 to FEffLengthRnd - 1 do
            begin
             CurWinVal    := GetWindowFactor(i * FEffLengthReci);
             FSpkCorFak   := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + CurWinVal * CurWinVal;
             Data[StartPos + i]      := Data[StartPos + i] * CurWinVal;
             Data[SampleCount - i + P1 - 1] := Data[SampleCount - i + P1 - 1] * CurWinVal;
            end;
          end
         else
          begin
           p1 := StartPos;
           p2 := EndPos;
           for i :=  0 to FEffLengthRnd - 1 do
            begin
             CurWinVal    := GetWindowFactor(i * FEffLengthReci);
             FSpkCorFak   := FSpkCorFak + CurWinVal + CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq  + CurWinVal * CurWinVal;
             Data[p1] := Data[p1] * CurWinVal;
             Data[p2] := Data[p2] * CurWinVal;
             inc(p1); if p1 >= SampleCount then p1 := 0;
             Dec(p2); if p2 <     0 then p2 := SampleCount;
            end;
          end;
        end;
      FSpkCorFak   := FSpkCorFak   + FLength - 2 * FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - 2 * FEffLengthRnd;
     end;
   wsRight:
     begin
      Counter := 0;
      if EndPos - FEffLengthRnd >= 0 then
       for i := EndPos downto EndPos - FEffLengthRnd do
        begin
         CurWinVal    := GetWindowFactor(Counter * FEffLengthReci);
         FSpkCorFak   := FSpkCorFak + CurWinVal;
         FSpkCorFakSq := FSpkCorFak + CurWinVal * CurWinVal;
         Data[i]  := Data[i] * CurWinVal;
         inc(Counter);
        end
       else // Wrap arround
        begin
         for i := EndPos downto 0 do
          begin
           CurWinVal    := GetWindowFactor(Counter * FEffLengthReci);
           FSpkCorFak   := FSpkCorFak + CurWinVal;
           FSpkCorFakSq := FSpkCorFak + CurWinVal * CurWinVal;
           Data[i]  := Data[i] * CurWinVal;
           inc(Counter);
          end;
         for i := SampleCount downto SampleCount - (FEffLengthRnd - EndPos) do
          begin
           CurWinVal    := GetWindowFactor(Counter * FEffLengthReci);
           FSpkCorFak   := FSpkCorFak + CurWinVal;
           FSpkCorFakSq := FSpkCorFak + CurWinVal * CurWinVal;
           Data[i]  := Data[i] * CurWinVal;
           inc(Counter);
          end
        end;
      FSpkCorFak   := FSpkCorFak   + FLength - FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - FEffLengthRnd;
     end;
 end;

 FSpkCorFakSq := Sqrt(FEffLength / FSpkCorFakSq);
 FillWithZeroes(Data, StartPos, EndPos, SampleCount);
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////// TWindowFunctionRectangle /////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TWindowFunctionRectangle.Create;
begin
 inherited;
 FBandwidth    :=   0.8859;
 FSidelobe     := -13.26;
 FFirstMinimum :=   2;
end;

function TWindowFunctionRectangle.GetWindowFactor(Pos: Double): Double; //inline;
begin
 Result := 1;
end;

class function TWindowFunctionRectangle.GetWindowFunctionName: AnsiString;
begin
 Result := 'Rectangle';
end;

procedure TWindowFunctionRectangle.ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  StartPos  : Integer;
  EndPos    : Integer;
begin
 if (SampleCount <= 0) then exit;

 if FLength > 2 * SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := (FStart + Length) - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := (FStart + Length - 1);

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 FillWithZeroes(Data, StartPos, EndPos, SampleCount);

 FSpkCorFak   := FLength;
 FSpkCorFakSq := Sqrt(FEffLength / FLength);
end;

procedure TWindowFunctionRectangle.ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  StartPos  : Integer;
  EndPos    : Integer;
begin
 if (SampleCount <= 0) then exit;

 if FLength > 2 * SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := (FStart + Length) - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := FStart + Length - 1;

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 FSpkCorFak   := FLength;
 FSpkCorFakSq := Sqrt(FEffLength / FLength);

 FillWithZeroes(Data, StartPos, EndPos, SampleCount);
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////// TWindowFunctionTriangle /////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TWindowFunctionTriangle.Create;
begin
 inherited;
 FBandwidth    := 1.276;
 FFirstMinimum := 4;
 FSidelobe     := -26.53;
end;

class function TWindowFunctionTriangle.GetWindowFunctionName: AnsiString;
begin
 Result := 'Triangle';
end;

function TWindowFunctionTriangle.GetWindowFactor(Pos: Double): Double; //inline;
begin
 Result := Pos;
end;

procedure TWindowFunctionTriangle.CalcEffectiveLength;
begin
 // Slope
 if FWinSlope = wsSymmetric
  then
   begin
    FEffLength    := (0.5 - 0.005 * FTukey) * FLength;
    FEffLengthRnd := Round(FEffLength - 0.49999999999);       // FPU friendly trunc
    if FBartlett
     then FEffLengthReci := 1 / FEffLength else
    if FEffLength > 0.5
     then FEffLengthReci := 1 / (FEffLength - 0.5)
     else FEffLengthReci := 1;
   end
  else
   begin
    FEffLength    := (1 - 0.01 * FTukey) * FLength;
    FEffLengthRnd := Round(FEffLength);
    if FBartlett
     then FEffLengthReci := 1 / (FEffLength - 0.5) else
    if FEffLength > 0
     then FEffLengthReci := 1 / (FEffLength-1)
     else FEffLengthReci := 1;
   end;
end;

procedure TWindowFunctionTriangle.ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  StartPos,
  EndPos,i  : Integer;
  ParamRec  : TParameterRecord;
begin
 if (SampleCount <= 0) then exit;
 if FLength > 2 * SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := FStart + Length - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := FStart + Length - 1;

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 ParamRec.SpectrumCorrectionFactor   := 0;
 ParamRec.SpuaredCorrectionFactor := 0;

 if FWinSlope = wsRight then
  begin
   ParamRec.ComplexPosition.Re := 1;
   ParamRec.ComplexAngle.Re    := -FEffLengthReci;
  end
 else
  begin
   if FBartlett
    then ParamRec.ComplexPosition.Re := 0.5 * FEffLengthReci
    else ParamRec.ComplexPosition.Re := 0;
   ParamRec.ComplexAngle.Re := FEffLengthReci;
   if (FWinSlope=wsSymmetric) and FInvert then
    if (FLength mod 2 = 0)
     then ParamRec.ComplexPosition.Re := 0.5 * ParamRec.ComplexAngle.Re
     else
      begin
       EndPos   := EndPos + 1;
       StartPos := StartPos - 1;
      end;
  end;

 case FWinSlope of
  wsLeft:
   begin
    if StartPos + FEffLengthRnd <= SampleCount
     then DoWinLoopTriangle32Forward(@Data[StartPos], ParamRec, FEffLengthRnd)
     else // Wrap arround
      begin
       DoWinLoopTriangle32Forward(@Data[StartPos], ParamRec, SampleCount - StartPos);
       DoWinLoopTriangle32Forward(@Data[0], ParamRec, FEffLengthRnd - (SampleCount - StartPos));
      end;
    ParamRec.SpectrumCorrectionFactor   := ParamRec.SpectrumCorrectionFactor   + FLength - FEffLengthRnd;
    ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - FEffLengthRnd;
   end;
  wsRight:
   begin
    if EndPos + 1 - FEffLengthRnd >= 0
     then DoWinLoopTriangle32Forward(@Data[EndPos + 1 - FEffLengthRnd], ParamRec, FEffLengthRnd)
     else // Wrap arround
      begin
       DoWinLoopTriangle32Forward(@Data[(EndPos + 1 - FEffLengthRnd + SampleCount)], ParamRec, FEffLengthRnd - (EndPos + 1));
       DoWinLoopTriangle32Forward(@Data[0], ParamRec, EndPos + 1);
      end;
    ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - FEffLengthRnd;
   end;
 wsSymmetric:
   begin
    if (StartPos + FEffLengthRnd < SampleCount) and (EndPos - FEffLengthRnd >= 0)
     then DoWinLoopTriangle32Symmetric(@Data[StartPos], ParamRec, FEffLengthRnd, @Data[EndPos]) // No wrap arround!
     else // Wrap around
      begin
       if (EndPos - FEffLengthRnd >= 0) then
        begin // Only left wrap around!
         i := SampleCount - StartPos;
         DoWinLoopTriangle32Symmetric(@Data[StartPos], ParamRec, i, @Data[EndPos]);
         DoWinLoopTriangle32Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[EndPos - i]);
        end else
       if (StartPos + FEffLengthRnd<=SampleCount) then
        begin // Only right wrap around!
         DoWinLoopTriangle32Symmetric(@Data[StartPos], ParamRec, EndPos + 1, @Data[EndPos]);
         DoWinLoopTriangle32Symmetric(@Data[StartPos + EndPos + 1], ParamRec, FEffLengthRnd - EndPos - 1, @Data[SampleCount-1]);
        end
       else
        begin
         exit;
         i := SampleCount - StartPos;
         if EndPos<i then
          begin
           DoWinLoopTriangle32Symmetric(@Data[StartPos], ParamRec, EndPos, @Data[EndPos]);
           DoWinLoopTriangle32Symmetric(@Data[StartPos + EndPos], ParamRec, i-EndPos, @Data[SampleCount-1]);
           DoWinLoopTriangle32Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[SampleCount-(i-EndPos)]);
          end
         else
          begin
           DoWinLoopTriangle32Symmetric(@Data[StartPos], ParamRec, i, @Data[EndPos]);
           DoWinLoopTriangle32Symmetric(@Data[0], ParamRec, EndPos - i, @Data[EndPos - i]);
           DoWinLoopTriangle32Symmetric(@Data[EndPos - i], ParamRec, FEffLengthRnd - EndPos, @Data[0]);
          end;
        end;
      end;
    ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength-2*FEffLengthRnd;
    ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength-2*FEffLengthRnd;
   end;
 end;

 ParamRec.SpuaredCorrectionFactor := Sqrt(FEffLength/(ParamRec.SpuaredCorrectionFactor));
 FillWithZeroes(Data,StartPos,EndPos,SampleCount);

 FSpkCorFak   := ParamRec.SpectrumCorrectionFactor;
 FSpkCorFakSq := ParamRec.SpuaredCorrectionFactor;
end;

procedure TWindowFunctionTriangle.ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  StartPos,
  EndPos,i    : Integer;
  ParamRec    : TParameterRecord;
begin
 if (SampleCount <= 0) then exit;
 if FLength > 2 * SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := (FStart + Length)-FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd-1
  else EndPos := (FStart + Length-1);

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 ParamRec.SpectrumCorrectionFactor := 0;
 ParamRec.SpuaredCorrectionFactor := 0;
 if FWinSlope=wsRight then
  begin
   ParamRec.ComplexPosition.Re := 1;
   ParamRec.ComplexAngle.Re := -FEffLengthReci;
  end
 else
  begin
   if FBartlett
    then ParamRec.ComplexPosition.Re := 0.5*FEffLengthReci
    else ParamRec.ComplexPosition.Re := 0;
   ParamRec.ComplexAngle.Re := FEffLengthReci;
   if (FWinSlope = wsSymmetric) and FInvert then
    if (FLength mod 2=0)
     then ParamRec.ComplexPosition.Re := 0.5*ParamRec.ComplexAngle.Re
     else
      begin
       EndPos := EndPos + 1;
       StartPos := StartPos -1;
      end;
  end;

 case FWinSlope of
  wsLeft:
   begin
    if StartPos + FEffLengthRnd<=SampleCount
     then DoWinLoopTriangle64Forward(@Data[StartPos], ParamRec, FEffLengthRnd)
     else // Wrap arround
      begin
       DoWinLoopTriangle64Forward(@Data[StartPos], ParamRec, SampleCount - StartPos);
       DoWinLoopTriangle64Forward(@Data[0], ParamRec, FEffLengthRnd - (SampleCount - StartPos));
      end;
    ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - FEffLengthRnd;
   end;
  wsRight:
   begin
    if EndPos + 1 - FEffLengthRnd >= 0
     then DoWinLoopTriangle64Forward(@Data[EndPos + 1 - FEffLengthRnd], ParamRec, FEffLengthRnd)
     else // Wrap arround
      begin
       DoWinLoopTriangle64Forward(@Data[(EndPos + 1 - FEffLengthRnd + SampleCount)], ParamRec, FEffLengthRnd - (EndPos + 1));
       DoWinLoopTriangle64Forward(@Data[0], ParamRec, EndPos + 1);
      end;
    ParamRec.SpectrumCorrectionFactor   := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - FEffLengthRnd;
   end;
 wsSymmetric:
   begin
    if (StartPos + FEffLengthRnd<SampleCount) and (EndPos - FEffLengthRnd >= 0)
     then DoWinLoopTriangle64Symmetric(@Data[StartPos], ParamRec, FEffLengthRnd, @Data[EndPos]) // No wrap arround!
     else // Wrap around
      begin
       if (EndPos - FEffLengthRnd >= 0) then
        begin // Only left wrap around!
         i := SampleCount - StartPos;
         DoWinLoopTriangle64Symmetric(@Data[StartPos], ParamRec, i, @Data[EndPos]);
         DoWinLoopTriangle64Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[EndPos - i]);
        end else
       if (StartPos + FEffLengthRnd<=SampleCount) then
        begin // Only right wrap around!
         DoWinLoopTriangle64Symmetric(@Data[StartPos], ParamRec, EndPos + 1, @Data[EndPos]);
         DoWinLoopTriangle64Symmetric(@Data[StartPos + EndPos + 1], ParamRec, FEffLengthRnd - EndPos - 1, @Data[SampleCount-1]);
        end
       else
        begin
         exit;
         i := SampleCount - StartPos;
         if EndPos<i then
          begin
           DoWinLoopTriangle64Symmetric(@Data[StartPos], ParamRec, EndPos, @Data[EndPos]);
           DoWinLoopTriangle64Symmetric(@Data[StartPos + EndPos], ParamRec, i-EndPos, @Data[SampleCount-1]);
           DoWinLoopTriangle64Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[SampleCount-(i-EndPos)]);
          end
         else
          begin
           DoWinLoopTriangle64Symmetric(@Data[StartPos], ParamRec, i, @Data[EndPos]);
           DoWinLoopTriangle64Symmetric(@Data[0], ParamRec, EndPos -i, @Data[EndPos -i]);
           DoWinLoopTriangle64Symmetric(@Data[EndPos -i], ParamRec, FEffLengthRnd - EndPos, @Data[0]);
          end;
        end;
      end;
    ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength-2*FEffLengthRnd;
    ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength-2*FEffLengthRnd;
   end;
 end;
 ParamRec.SpuaredCorrectionFactor := Sqrt(FEffLength/(ParamRec.SpuaredCorrectionFactor));
 FillWithZeroes(Data,StartPos,EndPos,SampleCount);

 FSpkCorFak   := ParamRec.SpectrumCorrectionFactor;
 FSpkCorFakSq := ParamRec.SpuaredCorrectionFactor;
end;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////// TWindowFunctionCosine //////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TWindowFunctionCosine.Create;
begin
 inherited;
 FBandwidth := 1.189;
 FFirstMinimum := 3;
 FSidelobe := -23;
end;

function TWindowFunctionCosine.GetWindowFactor(Pos: Double): Double; // inline;
begin
 Result := Sin(0.5 * PI * Pos);
end;

class function TWindowFunctionCosine.GetWindowFunctionName: AnsiString;
begin
 Result := 'Cosine';
end;

procedure TWindowFunctionCosine.ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  StartPos  : Integer;
  EndPos, i : Integer;
  ParamRec  : TParameterRecord;
begin
 if (SampleCount <= 0) then Exit;
 if FLength > SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := (FStart + Length - 1) - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := (FStart + Length - 1);

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 with ParamRec do
  begin
   SpectrumCorrectionFactor := 0;
   SpuaredCorrectionFactor := 0;
   GetSinCos(0.5 * PI * FEffLengthReci, ComplexAngle.Im, ComplexAngle.Re);
   if FBartlett then
    case FWinSlope of
     wsLeft  : begin ComplexPosition.Im := -ComplexAngle.Re; ComplexPosition.Re := ComplexAngle.Im; end;
     wsRight : begin ComplexPosition.Im := 0;  ComplexPosition.Re := 1; end;
     wsSymmetric :
      begin
       ComplexPosition.Im := -Sqrt(0.5 * (1 + ComplexAngle.Re));
       ComplexPosition.Re :=  Sqrt(0.5 * (1 - ComplexAngle.Re));
      end;
    end
   else
    if FWinSlope = wsRight
     then begin ComplexPosition.Im :=  0; ComplexPosition.Re :=  1; end
     else begin ComplexPosition.Im := -1; ComplexPosition.Re :=  0; end;
  end;

 case FWinSlope of
  wsLeft:
    begin
     if StartPos + FEffLengthRnd <= SampleCount
      then DoWinLoopCosine32Forward(@Data[StartPos], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        DoWinLoopCosine32Forward(@Data[StartPos], ParamRec, SampleCount - StartPos);
        DoWinLoopCosine32Forward(@Data[0], ParamRec, FEffLengthRnd - (SampleCount - StartPos));
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsRight:
    begin
     if EndPos + 1 - FEffLengthRnd >= 0
      then DoWinLoopCosine32Forward(@Data[EndPos + 1 - FEffLengthRnd], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        DoWinLoopCosine32Forward(@Data[(EndPos + 1 - FEffLengthRnd + SampleCount)], ParamRec, FEffLengthRnd - (EndPos + 1));
        DoWinLoopCosine32Forward(@Data[0], ParamRec, EndPos + 1);
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsSymmetric:
    begin
     if (StartPos + FEffLengthRnd < SampleCount) and (EndPos - FEffLengthRnd >= 0)
      then DoWinLoopCosine32Symmetric(@Data[StartPos], ParamRec, FEffLengthRnd, @Data[EndPos]) // No wrap arround!
      else // Wrap around
       begin
        if (EndPos -FEffLengthRnd >= 0) then
         begin // Only left wrap around!
          i := SampleCount - StartPos;
          DoWinLoopCosine32Symmetric(@Data[StartPos], ParamRec, i, @Data[EndPos]);
          DoWinLoopCosine32Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[EndPos - i]);
         end else
        if (StartPos + FEffLengthRnd<=SampleCount) then
         begin // Only right wrap around!
          DoWinLoopCosine32Symmetric(@Data[StartPos], ParamRec, EndPos + 1, @Data[EndPos]);
          DoWinLoopCosine32Symmetric(@Data[StartPos + EndPos + 1], ParamRec, FEffLengthRnd - EndPos - 1, @Data[SampleCount - 1]);
         end
        else
         begin
          i := SampleCount - StartPos;
          if EndPos<i then
           begin
            DoWinLoopCosine32Symmetric(@Data[StartPos], ParamRec, EndPos, @Data[EndPos]);
            DoWinLoopCosine32Symmetric(@Data[StartPos + EndPos], ParamRec, i - EndPos, @Data[SampleCount - 1]);
            DoWinLoopCosine32Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[SampleCount - (i - EndPos)]);
           end
          else
           begin
            DoWinLoopCosine32Symmetric(@Data[StartPos], ParamRec, i, @Data[EndPos]);
            DoWinLoopCosine32Symmetric(@Data[0], ParamRec,EndPos - i, @Data[EndPos - i]);
            DoWinLoopCosine32Symmetric(@Data[EndPos - i], ParamRec, FEffLengthRnd - EndPos, @Data[0]);
           end;
         end;
       end;
    ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - 2 * FEffLengthRnd;
    ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - 2 * FEffLengthRnd;
   end;
 end;

 ParamRec.SpuaredCorrectionFactor := Sqrt(FEffLength / (ParamRec.SpuaredCorrectionFactor));
 FillWithZeroes(Data, StartPos, EndPos, SampleCount);

 FSpkCorFak   := ParamRec.SpectrumCorrectionFactor;
 FSpkCorFakSq := ParamRec.SpuaredCorrectionFactor;
end;

procedure TWindowFunctionCosine.ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  StartPos  : Integer;
  EndPos, i : Integer;
  ParamRec  : TParameterRecord;
begin
 if (SampleCount <= 0) then Exit;
 if FLength > SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := (FStart + Length - 1) - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := (FStart + Length - 1);

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 with ParamRec do
  begin
   SpectrumCorrectionFactor := 0;
   SpuaredCorrectionFactor := 0;
   GetSinCos(0.5 * PI * FEffLengthReci, ComplexAngle.Im, ComplexAngle.Re);
   if FBartlett then
    case FWinSlope of
     wsLeft  : begin ComplexPosition.Im := -ComplexAngle.Re; ComplexPosition.Re := ComplexAngle.Im; end;
     wsRight : begin ComplexPosition.Im := 0;  ComplexPosition.Re := 1; end;
     wsSymmetric :
      begin
       ComplexPosition.Im := -Sqrt(0.5 * (1 + ComplexAngle.Re));
       ComplexPosition.Re :=  Sqrt(0.5 * (1 - ComplexAngle.Re));
      end;
    end
   else
    if FWinSlope = wsRight
     then begin ComplexPosition.Im :=  0; ComplexPosition.Re :=  1; end
     else begin ComplexPosition.Im := -1; ComplexPosition.Re :=  0; end;
  end;

 case FWinSlope of
  wsLeft:
    begin
     if StartPos + FEffLengthRnd <= SampleCount
      then DoWinLoopCosine64Forward(@Data[StartPos], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        DoWinLoopCosine64Forward(@Data[StartPos], ParamRec, SampleCount - StartPos);
        DoWinLoopCosine64Forward(@Data[0], ParamRec, FEffLengthRnd - (SampleCount - StartPos));
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsRight:
    begin
     if EndPos + 1 - FEffLengthRnd >= 0
      then DoWinLoopCosine64Forward(@Data[EndPos + 1 - FEffLengthRnd], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        DoWinLoopCosine64Forward(@Data[(EndPos + 1 - FEffLengthRnd + SampleCount)], ParamRec, FEffLengthRnd - (EndPos + 1));
        DoWinLoopCosine64Forward(@Data[0], ParamRec, EndPos + 1);
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsSymmetric:
    begin
     if (StartPos + FEffLengthRnd < SampleCount) and (EndPos - FEffLengthRnd >= 0)
      then DoWinLoopCosine64Symmetric(@Data[StartPos], ParamRec, FEffLengthRnd, @Data[EndPos]) // No wrap arround!
      else // Wrap around
       begin
        if (EndPos -FEffLengthRnd >= 0) then
         begin // Only left wrap around!
          i := SampleCount - StartPos;
          DoWinLoopCosine64Symmetric(@Data[StartPos], ParamRec, i, @Data[EndPos]);
          DoWinLoopCosine64Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[EndPos - i]);
         end else
        if (StartPos + FEffLengthRnd<=SampleCount) then
         begin // Only right wrap around!
          DoWinLoopCosine64Symmetric(@Data[StartPos], ParamRec, EndPos + 1, @Data[EndPos]);
          DoWinLoopCosine64Symmetric(@Data[StartPos + EndPos + 1], ParamRec, FEffLengthRnd - EndPos - 1, @Data[SampleCount - 1]);
         end
        else
         begin
          exit;
          i := SampleCount - StartPos;
          if EndPos<i then
           begin
            DoWinLoopCosine64Symmetric(@Data[StartPos], ParamRec, EndPos, @Data[EndPos]);
            DoWinLoopCosine64Symmetric(@Data[StartPos + EndPos], ParamRec, i - EndPos, @Data[SampleCount - 1]);
            DoWinLoopCosine64Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[SampleCount - (i - EndPos)]);
           end
          else
           begin
            DoWinLoopCosine64Symmetric(@Data[StartPos], ParamRec, i, @Data[EndPos]);
            DoWinLoopCosine64Symmetric(@Data[0], ParamRec,EndPos - i, @Data[EndPos - i]);
            DoWinLoopCosine64Symmetric(@Data[EndPos - i], ParamRec, FEffLengthRnd - EndPos, @Data[0]);
           end;
         end;
       end;
    ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - 2 * FEffLengthRnd;
    ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - 2 * FEffLengthRnd;
   end;
 end;

 ParamRec.SpuaredCorrectionFactor := Sqrt(FEffLength / (ParamRec.SpuaredCorrectionFactor));
 FillWithZeroes(Data, StartPos, EndPos, SampleCount);

 FSpkCorFak   := ParamRec.SpectrumCorrectionFactor;
 FSpkCorFakSq := ParamRec.SpuaredCorrectionFactor;
end;

procedure TCustomWindowFunction.SetBartlett(const Value: Boolean);
begin
 if FBartlett <> Value then
  begin
   FBartlett := Value;
   CalcEffectiveLength;
  end;
end;

{ TWindowFunctionWithParameter }

procedure TWindowFunctionWithParameter.SetAlpha(const Value: Single);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TWindowFunctionWithParameter.AlphaChanged;
begin
 // dummy
end;

{ TWindowFunctionLanczos }

constructor TWindowFunctionLanczos.Create;
begin
 inherited;
 FBandwidth    := 1.828;
 FFirstMinimum := 6.3460;
 FSidelobe     := 54.7200;
 FAlpha        := 1;
end;

procedure TWindowFunctionLanczos.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TWindowFunctionLanczos then
  begin
   TWindowFunctionLanczos(Dest).FAlpha := FAlpha;
  end;
end;

function TWindowFunctionLanczos.GetWindowFactor(Pos: Double): Double;
begin
 if (1 - Pos) = 0
  then Result := 1
  else Result := Power(sin(PI * Pos) / (PI * (1 - Pos)), FAlpha);
end;

class function TWindowFunctionLanczos.GetWindowFunctionName: AnsiString;
begin
 Result := 'Lanczos';
end;

(*
procedure TWindowFunctionLanczos.ProcessBlock64(
  Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  StartPos,
  EndPos,i    : Integer;
  ParamRec    : TParameterRecord;
begin
 if (SampleCount <= 0) then Exit;
 if FLength>SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := (FStart + Length-1)-FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd-1
  else EndPos := (FStart + Length-1);

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 with ParamRec do
  begin
   SpectrumCorrectionFactor := 0;
   SpuaredCorrectionFactor := 0;
   GetSinCos(0.5*PI*FEffLengthReci,ComplexAngle.Im,ComplexAngle.Re);
   if FBartlett then
    case FWinSlope of
     wsLeft  : begin ComplexPosition.Im := -ComplexAngle.Re; ComplexPosition.Re := ComplexAngle.Im; end;
     wsRight : begin ComplexPosition.Im := 0;  ComplexPosition.Re := 1; end;
     wsSymmetric :
      begin
       ComplexPosition.Im := -Sqrt(0.5*(1 + ComplexAngle.Re));
       ComplexPosition.Re := Sqrt(0.5*(1-ComplexAngle.Re));
      end;
    end
   else
    if FWinSlope=wsRight
     then begin ComplexPosition.Im :=  0; ComplexPosition.Re :=  1; end
     else begin ComplexPosition.Im := -1; ComplexPosition.Re :=  0; end;
  end;

 if FAlt then
  case Slope of
   wsSymmetric : FenstereDoubles       (0,1,FStart,SampleCount,FLength,@ParamRec.SpectrumCorrectionFactor,FInvert,1,Data,Round(2*FEffLength));
   wsLeft      : FenstereDoublesFlanke (0,1,FStart,SampleCount,FLength,@ParamRec.SpectrumCorrectionFactor,False,1,Data,Round(FEffLength));
   wsRight     : FenstereDoublesFlanke (0,1,FStart,SampleCount,FLength,@ParamRec.SpectrumCorrectionFactor,True,1,Data,Round(FEffLength));
  end
 else
  begin
   case FWinSlope of
    wsLeft:
      begin
       if StartPos + FEffLengthRnd<=SampleCount
        then DoWinLoopLanczos64Forward(@Data[StartPos], ParamRec, FEffLengthRnd)
        else // Wrap arround
         begin
          DoWinLoopLanczos64Forward(@Data[StartPos], ParamRec, SampleCount - StartPos);
          DoWinLoopLanczos64Forward(@Data[0], ParamRec, FEffLengthRnd - (SampleCount - StartPos));
         end;
       ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
      end;
    wsRight:
      begin
       if EndPos + 1 - FEffLengthRnd >= 0
        then DoWinLoopLanczos64Forward(@Data[EndPos + 1 - FEffLengthRnd], ParamRec, FEffLengthRnd)
        else // Wrap arround
         begin
          DoWinLoopLanczos64Forward(@Data[(EndPos + 1 - FEffLengthRnd + SampleCount)], ParamRec, FEffLengthRnd - (EndPos + 1));
          DoWinLoopLanczos64Forward(@Data[0], ParamRec,EndPos + 1);
         end;
       ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
      end;
    wsSymmetric:
      begin
       if (StartPos + FEffLengthRnd<SampleCount) and (EndPos -FEffLengthRnd >= 0)
        then DoWinLoopLanczos64Symmetric(@Data[StartPos], ParamRec, FEffLengthRnd, @Data[EndPos]) // No wrap arround!
        else // Wrap around
         begin
          if (EndPos -FEffLengthRnd >= 0) then
           begin // Only left wrap around!
            i := SampleCount - StartPos;
            DoWinLoopLanczos64Symmetric(@Data[StartPos], ParamRec,i, @Data[EndPos]);
            DoWinLoopLanczos64Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[EndPos -i]);
           end else
          if (StartPos + FEffLengthRnd<=SampleCount) then
           begin // Only right wrap around!
            DoWinLoopLanczos64Symmetric(@Data[StartPos], ParamRec,EndPos + 1, @Data[EndPos]);
            DoWinLoopLanczos64Symmetric(@Data[StartPos + EndPos + 1], ParamRec, FEffLengthRnd - EndPos -1, @Data[SampleCount-1]);
           end
          else
           begin
            exit;
            i := SampleCount - StartPos;
            if EndPos<i then
             begin
              DoWinLoopLanczos64Symmetric(@Data[StartPos], ParamRec,EndPos, @Data[EndPos]);
              DoWinLoopLanczos64Symmetric(@Data[StartPos + EndPos], ParamRec,i-EndPos, @Data[SampleCount-1]);
              DoWinLoopLanczos64Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[SampleCount-(i-EndPos)]);
             end
            else
             begin
              DoWinLoopLanczos64Symmetric(@Data[StartPos], ParamRec,i, @Data[EndPos]);
              DoWinLoopLanczos64Symmetric(@Data[0], ParamRec,EndPos -i, @Data[EndPos -i]);
              DoWinLoopLanczos64Symmetric(@Data[EndPos -i], ParamRec, FEffLengthRnd - EndPos, @Data[0]);
             end;
           end;
         end;
      ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength-2*FEffLengthRnd;
      ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength-2*FEffLengthRnd;
     end;
   end;
  ParamRec.SpuaredCorrectionFactor := Sqrt(FEffLength/(ParamRec.SpuaredCorrectionFactor));
 end;
 FillWithZeroes(Data,StartPos,EndPos,SampleCount);

 FSpkCorFak   := ParamRec.SpectrumCorrectionFactor;
 FSpkCorFakSq := ParamRec.SpuaredCorrectionFactor;
end;
*)

{ TWindowFunctionCosineTerm }

constructor TWindowFunctionCosineTerm.Create;
begin
 inherited;
end;

destructor TWindowFunctionCosineTerm.Destroy;
begin
 inherited;
end;

procedure TWindowFunctionCosineTerm.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TWindowFunctionCosineTerm then
  begin
   TWindowFunctionCosineTerm(Dest).FCosineTerms := FCosineTerms;
   TWindowFunctionCosineTerm(Dest).FCoefPointer := FCoefPointer;
  end;
end;

function TWindowFunctionCosineTerm.GetWindowFactor(Pos: Double): Double;
var
  cs : Double;
  i  : Integer;
begin
 cs     := cos(Pi * Pos);
 i      := FCosineTerms - 1;
 Result := FCoefPointer^[i];
 Dec(i);
 while i >= 0 do
  begin
   Result := Result * cs + FCoefPointer^[i];
   Dec(i)
  end;
end;

procedure TWindowFunctionCosineTerm.ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  StartPos  : Integer;
  EndPos, i : Integer;
  ParamRec  : TParameterRecord;
begin
 if (SampleCount <= 0) then Exit;
 if FLength > SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := (FStart + Length) - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := FStart + Length - 1;

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 with ParamRec do
  begin
   SpectrumCorrectionFactor := 0;
   SpuaredCorrectionFactor  := 0;
   GetSinCos(PI * FEffLengthReci, ComplexAngle.Im, ComplexAngle.Re);
   CoefficientPointer := FCoefPointer;
   if FBartlett then
    case FWinSlope of
     wsLeft  : begin ComplexPosition.Im := ComplexAngle.Re; ComplexPosition.Re := -ComplexAngle.Im; end;
     wsRight : begin ComplexPosition.Im := -1; ComplexPosition.Re := 0; end;
     wsSymmetric :
      begin
       ComplexPosition.Im :=  Sqrt(0.5 * (1 + ComplexAngle.Re));
       ComplexPosition.Re := -Sqrt(0.5 * (1 - ComplexAngle.Re));
      end;
    end
   else
    begin
     if FWinSlope = wsRight
      then begin ComplexPosition.Im := -1; ComplexPosition.Re := 0; end
      else begin ComplexPosition.Im :=  1; ComplexPosition.Re := 0; end;
    end;
  end;

 case FWinSlope of
  wsLeft:
    begin
     if StartPos + FEffLengthRnd <= SampleCount
      then FCosFwdLoop32(@Data[StartPos], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        FCosFwdLoop32(@Data[StartPos], ParamRec, SampleCount - StartPos);
        FCosFwdLoop32(@Data[0], ParamRec, FEffLengthRnd - (SampleCount - StartPos));
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
     ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsRight:
    begin
     if EndPos + 1 - FEffLengthRnd >= 0
      then FCosFwdLoop32(@Data[EndPos + 1 - FEffLengthRnd], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        FCosFwdLoop32(@Data[(EndPos + 1 - FEffLengthRnd + SampleCount)], ParamRec, FEffLengthRnd - (EndPos + 1));
        FCosFwdLoop32(@Data[0], ParamRec,EndPos + 1);
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
     ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsSymmetric:
    begin
     if (StartPos + FEffLengthRnd < SampleCount) and (EndPos - FEffLengthRnd >= 0)
      then FCosSymLoop32(@Data[StartPos], ParamRec, FEffLengthRnd, @Data[EndPos]) // No wrap arround!
      else // Wrap around
       begin
        if (EndPos - FEffLengthRnd >= 0) then
         begin // Only left wrap around!
          i := SampleCount - StartPos;
          FCosSymLoop32(@Data[StartPos], ParamRec,i, @Data[EndPos]);
          FCosSymLoop32(@Data[0], ParamRec, FEffLengthRnd - i, @Data[EndPos - i]);
         end else
        if (StartPos + FEffLengthRnd<=SampleCount) then
         begin // Only right wrap around!
          FCosSymLoop32(@Data[StartPos], ParamRec,EndPos + 1, @Data[EndPos]);
          FCosSymLoop32(@Data[StartPos + EndPos + 1], ParamRec, FEffLengthRnd - EndPos -1, @Data[SampleCount-1]);
         end
        else
         begin
          exit;
          i := SampleCount - StartPos;
          if EndPos<i then
           begin
            FCosSymLoop32(@Data[StartPos], ParamRec,EndPos, @Data[EndPos]);
            FCosSymLoop32(@Data[StartPos + EndPos], ParamRec, i - EndPos, @Data[SampleCount - 1]);
            FCosSymLoop32(@Data[0], ParamRec, FEffLengthRnd - i, @Data[SampleCount-(i-EndPos)]);
           end
          else
           begin
            FCosSymLoop32(@Data[StartPos], ParamRec,i, @Data[EndPos]);
            FCosSymLoop32(@Data[0], ParamRec,EndPos - i, @Data[EndPos - i]);
            FCosSymLoop32(@Data[EndPos -i], ParamRec, FEffLengthRnd - EndPos, @Data[0]);
           end;
         end;
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength -2 * FEffLengthRnd;
     ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - 2 * FEffLengthRnd;
    end;
 end;
 ParamRec.SpuaredCorrectionFactor := Sqrt(FEffLength / (ParamRec.SpuaredCorrectionFactor));
 FillWithZeroes(Data, StartPos, EndPos, SampleCount);

 FSpkCorFak   := ParamRec.SpectrumCorrectionFactor;
 FSpkCorFakSq := ParamRec.SpuaredCorrectionFactor;
end;

procedure TWindowFunctionCosineTerm.ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  StartPos  : Integer;
  EndPos, i : Integer;
  ParamRec  : TParameterRecord;
begin
 if (SampleCount <= 0) then Exit;
 if FLength > SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := (FStart + Length) - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := FStart + Length - 1;

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 with ParamRec do
  begin
   SpectrumCorrectionFactor := 0;
   SpuaredCorrectionFactor  := 0;
   GetSinCos(PI * FEffLengthReci, ComplexAngle.Im, ComplexAngle.Re);
   CoefficientPointer := FCoefPointer;
   if FBartlett then
    case FWinSlope of
     wsLeft  : begin ComplexPosition.Im := ComplexAngle.Re; ComplexPosition.Re := -ComplexAngle.Im; end;
     wsRight : begin ComplexPosition.Im := -1; ComplexPosition.Re := 0; end;
     wsSymmetric :
      begin
       ComplexPosition.Im :=  Sqrt(0.5 * (1 + ComplexAngle.Re));
       ComplexPosition.Re := -Sqrt(0.5 * (1 - ComplexAngle.Re));
      end;
    end
   else
    begin
     if FWinSlope = wsRight
      then begin ComplexPosition.Im := -1; ComplexPosition.Re := 0; end
      else begin ComplexPosition.Im := 1; ComplexPosition.Re :=  0; end;
    end;
  end;

 case FWinSlope of
  wsLeft:
    begin
     if StartPos + FEffLengthRnd<=SampleCount
      then FCosFwdLoop64(@Data[StartPos], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        FCosFwdLoop64(@Data[StartPos], ParamRec, SampleCount - StartPos);
        FCosFwdLoop64(@Data[0], ParamRec, FEffLengthRnd - (SampleCount - StartPos));
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
     ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsRight:
    begin
     if EndPos + 1 - FEffLengthRnd >= 0
      then FCosFwdLoop64(@Data[EndPos + 1 - FEffLengthRnd], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        FCosFwdLoop64(@Data[(EndPos + 1 - FEffLengthRnd + SampleCount)], ParamRec, FEffLengthRnd - (EndPos + 1));
        FCosFwdLoop64(@Data[0], ParamRec, EndPos + 1);
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
     ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsSymmetric:
    begin
     if (StartPos + FEffLengthRnd < SampleCount) and (EndPos - FEffLengthRnd >= 0)
      then FCosSymLoop64(@Data[StartPos], ParamRec, FEffLengthRnd, @Data[EndPos]) // No wrap arround!
      else // Wrap around
       begin
        if (EndPos -FEffLengthRnd >= 0) then
         begin // Only left wrap around!
          i := SampleCount - StartPos;
          FCosSymLoop64(@Data[StartPos], ParamRec, i, @Data[EndPos]);
          FCosSymLoop64(@Data[0], ParamRec, FEffLengthRnd - i, @Data[EndPos - i]);
         end else
        if (StartPos + FEffLengthRnd <= SampleCount) then
         begin // Only right wrap around!
          FCosSymLoop64(@Data[StartPos], ParamRec, EndPos + 1, @Data[EndPos]);
          FCosSymLoop64(@Data[StartPos + EndPos + 1], ParamRec, FEffLengthRnd - EndPos - 1, @Data[SampleCount - 1]);
         end
        else
         begin
          exit;
          i := SampleCount - StartPos;
          if EndPos<i then
           begin
            FCosSymLoop64(@Data[StartPos], ParamRec, EndPos, @Data[EndPos]);
            FCosSymLoop64(@Data[StartPos + EndPos], ParamRec, i - EndPos, @Data[SampleCount - 1]);
            FCosSymLoop64(@Data[0], ParamRec, FEffLengthRnd - i, @Data[SampleCount - (i - EndPos)]);
           end
          else
           begin
            FCosSymLoop64(@Data[StartPos], ParamRec, i, @Data[EndPos]);
            FCosSymLoop64(@Data[0], ParamRec, EndPos - i, @Data[EndPos -i]);
            FCosSymLoop64(@Data[EndPos -i], ParamRec, FEffLengthRnd - EndPos, @Data[0]);
           end;
         end;
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor   + FLength - 2 * FEffLengthRnd;
     ParamRec.SpuaredCorrectionFactor := ParamRec.SpuaredCorrectionFactor + FLength - 2 * FEffLengthRnd;
    end;
 end;

 if ParamRec.SpuaredCorrectionFactor <> 0
  then ParamRec.SpuaredCorrectionFactor := Sqrt(FEffLength/(ParamRec.SpuaredCorrectionFactor));
 FillWithZeroes(Data, StartPos, EndPos, SampleCount);

 FSpkCorFak   := ParamRec.SpectrumCorrectionFactor;
 FSpkCorFakSq := ParamRec.SpuaredCorrectionFactor;
end;

procedure TWindowFunctionCosineTerm.SetCosineTerms(Value: Integer);
begin
 if Value < 2 then Value := 2 else
 if Value > 11 then Value := 11;
 if Value <> FCosineTerms then
  begin
   FCosineTerms := Value;
   case Value of
    2 : begin
         FCosFwdLoop32 := DoWinLoopCos2T32Forward;
         FCosFwdLoop64 := DoWinLoopCos2T64Forward;
         FCosSymLoop32 := DoWinLoopCos2T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos2T64Symmetric;
        end;
    3 : begin
         FCosFwdLoop32 := DoWinLoopCos3T32Forward;
         FCosFwdLoop64 := DoWinLoopCos3T64Forward;
         FCosSymLoop32 := DoWinLoopCos3T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos3T64Symmetric;
        end;
    4 : begin
         FCosFwdLoop32 := DoWinLoopCos4T32Forward;
         FCosFwdLoop64 := DoWinLoopCos4T64Forward;
         FCosSymLoop32 := DoWinLoopCos4T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos4T64Symmetric;
        end;
    5 : begin
         FCosFwdLoop32 := DoWinLoopCos5T32Forward;
         FCosFwdLoop64 := DoWinLoopCos5T64Forward;
         FCosSymLoop32 := DoWinLoopCos5T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos5T64Symmetric;
        end;
    6 : begin
         FCosFwdLoop32 := DoWinLoopCos6T32Forward;
         FCosFwdLoop64 := DoWinLoopCos6T64Forward;
         FCosSymLoop32 := DoWinLoopCos6T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos6T64Symmetric;
        end;
    7 : begin
         FCosFwdLoop32 := DoWinLoopCos7T32Forward;
         FCosFwdLoop64 := DoWinLoopCos7T64Forward;
         FCosSymLoop32 := DoWinLoopCos7T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos7T64Symmetric;
        end;
    8 : begin
         FCosFwdLoop32 := DoWinLoopCos8T32Forward;
         FCosFwdLoop64 := DoWinLoopCos8T64Forward;
         FCosSymLoop32 := DoWinLoopCos8T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos8T64Symmetric;
        end;
    9 : begin
         FCosFwdLoop32 := DoWinLoopCos9T32Forward;
         FCosFwdLoop64 := DoWinLoopCos9T64Forward;
         FCosSymLoop32 := DoWinLoopCos9T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos9T64Symmetric;
        end;
   10 : begin
         FCosFwdLoop32 := DoWinLoopCos10T32Forward;
         FCosFwdLoop64 := DoWinLoopCos10T64Forward;
         FCosSymLoop32 := DoWinLoopCos10T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos10T64Symmetric;
        end;
   11 : begin
         FCosFwdLoop32 := DoWinLoopCos11T32Forward;
         FCosFwdLoop64 := DoWinLoopCos11T64Forward;
         FCosSymLoop32 := DoWinLoopCos11T32Symmetric;
         FCosSymLoop64 := DoWinLoopCos11T64Symmetric;
        end;
   end;
  end;
end;


{ TWindowFunctionHanning }

constructor TWindowFunctionHanning.Create;
begin
 inherited;
 FBandwidth    := 1.4410;
 FSidelobe     := -31.48;
 FFirstMinimum := 4;
end;

function TWindowFunctionHanning.GetWindowFactor(Pos: Double): Double; //inline;
begin
 Result := CHalf64 * (1 - Cos(Pi * Pos));
end;

class function TWindowFunctionHanning.GetWindowFunctionName: AnsiString;
begin
 Result := 'Hanning';
end;

procedure TWindowFunctionHanning.ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  StartPos  : Integer;
  EndPos, i : Integer;
  ParamRec  : TParameterRecord;
begin
 if (SampleCount <= 0) then Exit;
 if FLength > SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := FStart + Length - 1 - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := FStart + Length - 1;

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 with ParamRec do
  begin
   SpectrumCorrectionFactor := 0;
   SpuaredCorrectionFactor  := 0;
   GetSinCos(PI * FEffLengthReci, ComplexAngle.Im, ComplexAngle.Re);
   CoefficientPointer := @CHanning;
   if FBartlett then
    case FWinSlope of
     wsLeft  : begin ComplexPosition.Im :=  ComplexAngle.Re; ComplexPosition.Re := -ComplexAngle.Im; end;
     wsRight : begin ComplexPosition.Im := -1; ComplexPosition.Re := 0; end;
     wsSymmetric :
      begin
       ComplexPosition.Im :=  Sqrt(0.5 * (1 + ComplexAngle.Re));
       ComplexPosition.Re := -Sqrt(0.5 * (1 - ComplexAngle.Re));
      end;
    end
   else
    begin
     if FWinSlope = wsRight
      then begin ComplexPosition.Im := -1; ComplexPosition.Re := 0; end
      else begin ComplexPosition.Im :=  1; ComplexPosition.Re := 0; end;
    end;
  end;

 case FWinSlope of
  wsLeft:
    begin
     if StartPos + FEffLengthRnd <= SampleCount
      then DoWinLoopHanning32Forward(@Data[StartPos], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        DoWinLoopHanning32Forward(@Data[StartPos], ParamRec, SampleCount - StartPos);
        DoWinLoopHanning32Forward(@Data[0], ParamRec, FEffLengthRnd - SampleCount + StartPos);
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsRight:
    begin
     if EndPos + 1 - FEffLengthRnd >= 0
      then DoWinLoopHanning32Forward(@Data[EndPos + 1 - FEffLengthRnd], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        DoWinLoopHanning32Forward(@Data[(EndPos + 1 - FEffLengthRnd + SampleCount)], ParamRec, FEffLengthRnd - (EndPos + 1));
        DoWinLoopHanning32Forward(@Data[0], ParamRec,EndPos + 1);
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsSymmetric:
    begin
     if (StartPos + FEffLengthRnd<SampleCount) and (EndPos - FEffLengthRnd >= 0)
      then DoWinLoopHanning32Symmetric(@Data[StartPos], ParamRec, FEffLengthRnd, @Data[EndPos]) // No wrap arround!
      else // Wrap around
       begin
        if (EndPos - FEffLengthRnd >= 0) then
         begin // Only left wrap around!
          i := SampleCount - StartPos;
          DoWinLoopHanning32Symmetric(@Data[StartPos], ParamRec,i, @Data[EndPos]);
          DoWinLoopHanning32Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[EndPos -i]);
         end else
        if (StartPos + FEffLengthRnd<=SampleCount) then
         begin // Only right wrap around!
          DoWinLoopHanning32Symmetric(@Data[StartPos], ParamRec,EndPos + 1, @Data[EndPos]);
          DoWinLoopHanning32Symmetric(@Data[StartPos + EndPos + 1], ParamRec, FEffLengthRnd - EndPos -1, @Data[SampleCount-1]);
         end
        else
         begin
          exit;
          i := SampleCount - StartPos;
          if EndPos<i then
           begin
            DoWinLoopHanning32Symmetric(@Data[StartPos], ParamRec,EndPos, @Data[EndPos]);
            DoWinLoopHanning32Symmetric(@Data[StartPos + EndPos], ParamRec,i-EndPos, @Data[SampleCount-1]);
            DoWinLoopHanning32Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[SampleCount-(i-EndPos)]);
           end
          else
           begin
            DoWinLoopHanning32Symmetric(@Data[StartPos], ParamRec,i, @Data[EndPos]);
            DoWinLoopHanning32Symmetric(@Data[0], ParamRec,EndPos -i, @Data[EndPos -i]);
            DoWinLoopHanning32Symmetric(@Data[EndPos -i], ParamRec, FEffLengthRnd - EndPos, @Data[0]);
           end;
         end;
       end;
      ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength-2*FEffLengthRnd;
    end;
 end;
 FillWithZeroes(Data, StartPos, EndPos, SampleCount);

 FSpkCorFak   := ParamRec.SpectrumCorrectionFactor;
 FSpkCorFakSq := Sqrt(FEffLength / (ParamRec.SpuaredCorrectionFactor));
end;

procedure TWindowFunctionHanning.ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  StartPos  : Integer;
  EndPos, i : Integer;
  ParamRec  : TParameterRecord;
begin
 if (SampleCount <= 0) then Exit;
 if FLength > SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 if FInvert
  then StartPos := (FStart + Length - 1) - FEffLengthRnd
  else StartPos := FStart;

 if FInvert
  then EndPos := FStart + FEffLengthRnd - 1
  else EndPos := (FStart + Length - 1);

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 with ParamRec do
  begin
   SpectrumCorrectionFactor := 0;
   SpuaredCorrectionFactor := 0;
   GetSinCos(PI * FEffLengthReci, ComplexAngle.Im, ComplexAngle.Re);
   CoefficientPointer := @CHanning;
   if FBartlett then
    case FWinSlope of
     wsLeft  : begin ComplexPosition.Im :=  ComplexAngle.Re; ComplexPosition.Re := -ComplexAngle.Im; end;
     wsRight : begin ComplexPosition.Im := -1; ComplexPosition.Re := 0; end;
     wsSymmetric :
      begin
       ComplexPosition.Im :=  Sqrt(0.5*(1 + ComplexAngle.Re));
       ComplexPosition.Re := -Sqrt(0.5*(1-ComplexAngle.Re));
      end;
    end
   else
    begin
     if FWinSlope=wsRight
      then begin ComplexPosition.Im := -1; ComplexPosition.Re := 0; end
      else begin ComplexPosition.Im :=  1; ComplexPosition.Re := 0; end;
    end;
  end;

 case FWinSlope of
  wsLeft:
    begin
     if StartPos + FEffLengthRnd <= SampleCount
      then DoWinLoopHanning64Forward(@Data[StartPos], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        DoWinLoopHanning64Forward(@Data[StartPos], ParamRec, SampleCount - StartPos);
        DoWinLoopHanning64Forward(@Data[0], ParamRec, FEffLengthRnd - (SampleCount - StartPos));
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsRight:
    begin
     if EndPos + 1 - FEffLengthRnd >= 0
      then DoWinLoopHanning64Forward(@Data[EndPos + 1 - FEffLengthRnd], ParamRec, FEffLengthRnd)
      else // Wrap arround
       begin
        DoWinLoopHanning64Forward(@Data[EndPos + 1 - FEffLengthRnd + SampleCount], ParamRec, FEffLengthRnd - EndPos - 1);
        DoWinLoopHanning64Forward(@Data[0], ParamRec,EndPos + 1);
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength - FEffLengthRnd;
    end;
  wsSymmetric:
    begin
     if (StartPos + FEffLengthRnd<SampleCount) and (EndPos -FEffLengthRnd >= 0)
      then DoWinLoopHanning64Symmetric(@Data[StartPos], ParamRec, FEffLengthRnd, @Data[EndPos]) // No wrap arround!
      else // Wrap around
       begin
        if (EndPos -FEffLengthRnd >= 0) then
         begin // Only left wrap around!
          i := SampleCount - StartPos;
          DoWinLoopHanning64Symmetric(@Data[StartPos], ParamRec,i, @Data[EndPos]);
          DoWinLoopHanning64Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[EndPos -i]);
         end else
        if (StartPos + FEffLengthRnd<=SampleCount) then
         begin // Only right wrap around!
          DoWinLoopHanning64Symmetric(@Data[StartPos], ParamRec,EndPos + 1, @Data[EndPos]);
          DoWinLoopHanning64Symmetric(@Data[StartPos + EndPos + 1], ParamRec, FEffLengthRnd - EndPos -1, @Data[SampleCount-1]);
         end
        else
         begin
          Exit;
          i := SampleCount - StartPos;
          if EndPos < i then
           begin
            DoWinLoopHanning64Symmetric(@Data[StartPos], ParamRec,EndPos, @Data[EndPos]);
            DoWinLoopHanning64Symmetric(@Data[StartPos + EndPos], ParamRec,i-EndPos, @Data[SampleCount-1]);
            DoWinLoopHanning64Symmetric(@Data[0], ParamRec, FEffLengthRnd - i, @Data[SampleCount-(i-EndPos)]);
           end
          else
           begin
            DoWinLoopHanning64Symmetric(@Data[StartPos], ParamRec,i, @Data[EndPos]);
            DoWinLoopHanning64Symmetric(@Data[0], ParamRec,EndPos -i, @Data[EndPos -i]);
            DoWinLoopHanning64Symmetric(@Data[EndPos -i], ParamRec, FEffLengthRnd - EndPos, @Data[0]);
           end;
         end;
       end;
     ParamRec.SpectrumCorrectionFactor := ParamRec.SpectrumCorrectionFactor + FLength-2*FEffLengthRnd;
    end;
 end;
 FillWithZeroes(Data,StartPos,EndPos,SampleCount);

 FSpkCorFak   := ParamRec.SpectrumCorrectionFactor;
 FSpkCorFakSq := Sqrt(FEffLength / (ParamRec.SpuaredCorrectionFactor));
end;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////// TWindowFunctionHamming //////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TWindowFunctionHamming.Create;
begin
 inherited;
 FBandwidth := 1.303;
 FSidelobe := -42.68;
 FFirstMinimum := 4;
 FCoefPointer := @CHamming[0];
 CosineTerms := 2;
end;

class function TWindowFunctionHamming.GetWindowFunctionName: AnsiString;
begin
 Result := 'Hanning';
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////// TWindowFunctionBlackman /////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TWindowFunctionBlackman.Create;
begin
 inherited;
 FCoefPointer  := @CBlackman[0];
 FBandwidth    := 1.644;
 FSidelobe     := -58.11;
 FFirstMinimum := 6;
 CosineTerms   := 3;
end;

class function TWindowFunctionBlackman.GetWindowFunctionName: AnsiString;
begin
 Result := 'Blackman';
end;


{ TWindowFunctionWelch }

constructor TWindowFunctionWelch.Create;
begin
 inherited;
 FSidelobe     := -21.28;
 FBandwidth    := 1.154;
 FFirstMinimum := 2.859;
end;

class function TWindowFunctionWelch.GetWindowFunctionName: AnsiString;
begin
 Result := 'Welch';
end;

function TWindowFunctionWelch.GetWindowFactor(Pos: Double): Double; //inline;
begin
 Result := 2 * Pos - Sqr(Pos);
end;

procedure TWindowFunctionWelch.ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  i, p1, p2   : Integer;
  StartPos,
  EndPos      : Integer;
  CurWinVal   : Double;
  Counter     : Integer;

  procedure DoWinLoopForward(Strt,Stp : Integer);
  var r : Integer;
  begin
   for r := Strt to Stp - 1 do
    begin
     CurWinVal := Counter * FEffLengthReci*(2 - Counter * FEffLengthReci);
     inc(Counter);

     FSpkCorFak := FSpkCorFak + CurWinVal;
     FSpkCorFakSq := FSpkCorFak + CurWinVal * CurWinVal;
     Data[r] := Data[r] * CurWinVal;
    end
  end;

  procedure DoWinLoopBackward(Strt,Stp : Integer);
  var r : Integer;
  begin
   for r := Strt downto Stp - 1 do
    begin
     CurWinVal := Counter * FEffLengthReci * (2 - Counter * FEffLengthReci);
     inc(Counter);

     FSpkCorFak   := FSpkCorFak + CurWinVal;
     FSpkCorFakSq := FSpkCorFak + CurWinVal * CurWinVal;
     Data[r]  := Data[r] * CurWinVal;
    end
  end;

  procedure DoWinLoopSymmetric(Strt,Stp : Integer);
  var r : Integer;
  begin
   for r :=  Strt to Stp - 1 do
    begin
     CurWinVal := Counter * FEffLengthReci * (2 - Counter * FEffLengthReci);
     inc(Counter);

     FSpkCorFak   := FSpkCorFak + 2 * CurWinVal;
     FSpkCorFakSq := FSpkCorFak + 2 * CurWinVal * CurWinVal;
     Data[p1] := Data[p1] * CurWinVal;
     Data[p2] := Data[p2] * CurWinVal;
     Inc(p1); Dec(p2);
    end;
  end;

begin
 if (SampleCount <= 0) then Exit;
 if FLength > SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;
 StartPos := FStart;
 while StartPos >= SampleCount do StartPos := StartPos - SampleCount;
 while StartPos <     0 do StartPos := StartPos + SampleCount;

 EndPos := (FStart + Length - 1);
 while EndPos  >= SampleCount do EndPos := EndPos - SampleCount;
 while EndPos <    0 do EndPos := EndPos + SampleCount;

 FSpkCorFak   := 0;
 FSpkCorFakSq := 0;
 if FBartlett
  then Counter := 1
  else Counter := 0;

 case FWinSlope of
   wsLeft:
     begin
      if StartPos + FEffLengthRnd<=SampleCount
       then DoWinLoopForward(StartPos, StartPos + FEffLengthRnd)
       else // Wrap arround
        begin
         DoWinLoopForward(StartPos,SampleCount);
         DoWinLoopForward(0, FEffLengthRnd - (SampleCount - StartPos));
        end;
      FSpkCorFak := FSpkCorFak + FLength - FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - FEffLengthRnd;
     end;
   wsSymmetric:
     begin
      if (StartPos + FEffLengthRnd<SampleCount) and (EndPos -FEffLengthRnd >= 0) then
       begin // No wrap arround!
        p1 := StartPos;
        p2 := EndPos;
        DoWinLoopSymmetric(0, FEffLengthRnd);
       end
       else // Wrap around
        begin
         if (StartPos + FEffLengthRnd<=SampleCount) then
          begin // Only right wrap around!
           P1 := StartPos;
           P2 := EndPos;
           DoWinLoopSymmetric(0,EndPos + 1);     // Endpos + 1 ???
           P2 := SampleCount;
           DoWinLoopSymmetric(EndPos + 1, FEffLengthRnd);
          end else
         if (EndPos -FEffLengthRnd >= 0) then
          begin // Only left wrap around!
           P1 := StartPos;
           P2 := EndPos;
           DoWinLoopSymmetric(0, SampleCount - StartPos);
           P1 := 0;
           DoWinLoopSymmetric(SampleCount - StartPos, FEffLengthRnd);
          end
         else
          begin
           p1 := StartPos;
           p2 := EndPos;
           for i :=  0 to FEffLengthRnd - 1 do
            begin
             CurWinVal := Counter * FEffLengthReci*(2 - Counter * FEffLengthReci);
             inc(Counter);

             FSpkCorFak := FSpkCorFak + 2 * CurWinVal;
             FSpkCorFakSq := FSpkCorFak + 2 * CurWinVal * CurWinVal;
             Data[p1] := Data[p1] * CurWinVal;
             Data[p2] := Data[p2] * CurWinVal;
             inc(p1); if p1 >= SampleCount then p1 := 0;
             Dec(p2); if p2<0 then p2 := SampleCount;
            end;
          end;
        end;
      FSpkCorFak := FSpkCorFak + FLength-2*FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength-2*FEffLengthRnd;
     end;
   wsRight:
     begin
      if EndPos -FEffLengthRnd >= 0
       then DoWinLoopBackward(EndPos,EndPos -FEffLengthRnd)
       else // Wrap arround
        begin
         DoWinLoopBackward(EndPos,0);
         DoWinLoopBackward(SampleCount,SampleCount-(FEffLengthRnd - EndPos));
        end;
      FSpkCorFak := FSpkCorFak + FLength - FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - FEffLengthRnd;
     end;
 end;
 FSpkCorFakSq := Sqrt(FEffLength/FSpkCorFakSq);
 FillWithZeroes(Data,StartPos,EndPos,SampleCount);
end;

procedure TWindowFunctionWelch.ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  i, p1, p2 : Integer;
  StartPos  : Integer;
  EndPos    : Integer;
  CurWinVal : Double;
  Counter   : Integer;

  procedure DoWinLoopForward(Strt,Stp : Integer);
  var r : Integer;
  begin
   for r := Strt to Stp - 1 do
    begin
     CurWinVal := Counter * FEffLengthReci*(2 - Counter * FEffLengthReci);
     Inc(Counter);

     FSpkCorFak := FSpkCorFak + CurWinVal;
     FSpkCorFakSq := FSpkCorFak + CurWinVal * CurWinVal;
     Data[r] := Data[r] * CurWinVal;
    end
  end;

  procedure DoWinLoopBackward(Strt,Stp : Integer);
  var r : Integer;
  begin
   for r := Strt downto Stp - 1 do
    begin
     CurWinVal := Counter * FEffLengthReci*(2 - Counter * FEffLengthReci);
     Inc(Counter);

     FSpkCorFak := FSpkCorFak + CurWinVal;
     FSpkCorFakSq := FSpkCorFak + CurWinVal * CurWinVal;
     Data[r] := Data[r] * CurWinVal;
    end
  end;

  procedure DoWinLoopSymmetric(Strt,Stp : Integer);
  var r : Integer;
  begin
   for r :=  Strt to Stp - 1 do
    begin
     CurWinVal := Counter * FEffLengthReci*(2 - Counter * FEffLengthReci);
     Inc(Counter);

     FSpkCorFak := FSpkCorFak + 2 * CurWinVal;
     FSpkCorFakSq := FSpkCorFak + 2 * CurWinVal * CurWinVal;
     Data[p1] := Data[p1] * CurWinVal;
     Data[p2] := Data[p2] * CurWinVal;
     inc(p1); Dec(p2);
    end;
  end;

begin
 if (SampleCount <= 0) then Exit;

 if FLength > SampleCount then
  begin
   FLength := SampleCount;
   CalcEffectiveLength;
  end;

 StartPos := FStart;
 EndPos := (FStart + Length - 1);

 // wrap positions
 WrapInt(StartPos, SampleCount);
 WrapInt(EndPos, SampleCount);

 FSpkCorFak := 0;
 FSpkCorFakSq := 0;
 if FBartlett
  then Counter := 1
  else Counter := 0;

 case FWinSlope of
   wsLeft:
     begin
      if StartPos + FEffLengthRnd<=SampleCount
       then DoWinLoopForward(StartPos,StartPos + FEffLengthRnd)
       else // Wrap arround
        begin
         DoWinLoopForward(StartPos,SampleCount);
         DoWinLoopForward(0, FEffLengthRnd - (SampleCount - StartPos));
        end;
       FSpkCorFak := FSpkCorFak + FLength - FEffLengthRnd;
       FSpkCorFakSq := FSpkCorFakSq + FLength - FEffLengthRnd;
     end;
   wsSymmetric:
     begin
      if (StartPos + FEffLengthRnd<SampleCount) and (EndPos -FEffLengthRnd >= 0) then
       begin // No wrap arround!
        p1 := StartPos;
        p2 := EndPos;
        DoWinLoopSymmetric(0, FEffLengthRnd);
       end
       else // Wrap around
        begin
         if (StartPos + FEffLengthRnd<=SampleCount) then
          begin // Only right wrap around!
           P1 := StartPos;
           P2 := EndPos;
           DoWinLoopSymmetric(0,EndPos + 1);     // Endpos + 1 ???
           P2 := SampleCount;
           DoWinLoopSymmetric(EndPos + 1, FEffLengthRnd);
          end else
         if (EndPos -FEffLengthRnd >= 0) then
          begin // Only left wrap around!
           P1 := StartPos;
           P2 := EndPos;
           DoWinLoopSymmetric(0, SampleCount - StartPos);
           P1 := 0;
           DoWinLoopSymmetric(SampleCount - StartPos, FEffLengthRnd);
          end
         else
          begin
           p1 := StartPos;
           p2 := EndPos;
           for i :=  0 to FEffLengthRnd - 1 do
            begin
             CurWinVal := Counter * FEffLengthReci*(2 - Counter * FEffLengthReci);
             Counter := Counter + 1;

             FSpkCorFak := FSpkCorFak + 2 * CurWinVal;
             FSpkCorFakSq := FSpkCorFakSq + 2 * CurWinVal * CurWinVal;
             Data[p1] := Data[p1] * CurWinVal;
             Data[p2] := Data[p2] * CurWinVal;
             inc(p1); if p1 >= SampleCount then p1 := 0;
             Dec(p2); if p2<0 then p2 := SampleCount;
            end;
          end;
        end;
      FSpkCorFak   := FSpkCorFak   + FLength - 2 * FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - 2 * FEffLengthRnd;
     end;
   wsRight:
     begin
      if EndPos - FEffLengthRnd >= 0
       then DoWinLoopBackward(EndPos,EndPos -FEffLengthRnd)
       else // Wrap arround
        begin
         DoWinLoopBackward(EndPos,0);
         DoWinLoopBackward(SampleCount, SampleCount - (FEffLengthRnd - EndPos));
        end;
      FSpkCorFak := FSpkCorFak + FLength - FEffLengthRnd;
      FSpkCorFakSq := FSpkCorFakSq + FLength - FEffLengthRnd;
     end;
 end;
 FSpkCorFakSq := Sqrt(FEffLength/FSpkCorFakSq);
 FillWithZeroes(Data,StartPos,EndPos,SampleCount);
end;


procedure RegisterWindowFunction(AClass: TWindowFunctionClass);
var
  i : Integer;
begin
 // check if file format is already registered
 for i := 0 to Length(GWindowFunctions) - 1 do
  if GWindowFunctions[i] = AClass
   then raise Exception.Create(RCStrWindowDuplicate);

 // add file format to list
 SetLength(GWindowFunctions, Length(GWindowFunctions) + 1);
 GWindowFunctions[Length(GWindowFunctions) - 1] := AClass;
end;

procedure RegisterWindowFunctions(AClasses: array of TWindowFunctionClass);
var
  i : Integer;
begin
 for i := 0 to Length(AClasses) - 1
  do RegisterWindowFunction(AClasses[i]);
end;


initialization
  RegisterWindowFunctions([TWindowFunctionRectangle, TWindowFunctionTriangle,
    TWindowFunctionHanning, TWindowFunctionHamming, TWindowFunctionBlackman,
    TWindowFunctionLanczos, TWindowFunctionWelch]);

end.
