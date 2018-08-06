{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_Sonogram;

interface

{$I DAV_Compiler.inc}

uses
  Classes, Graphics, DAV_Types, DAV_Classes, DAV_Complex, DAV_GuiCommon,
  DAV_DspBuildingBlocks, DAV_DspWindowFunctions, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS {$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

type
  TCustomSonogram = class(TDspSampleRatePersistent)
  private
    FBlockBuilder: TCustomBuildingBlocks;
    FColorScheme: array [0 .. 255] of TRGB24;
    FCurrentSlice: Integer;
    FFftOrder: Integer;
    FFft: TFftReal2Complex;
    FLogarithmic: Boolean;
    FWindowClass: TWindowFunctionClass;
    FMaximumLevel: Single;
    FMinimumLevel: Single;
    FLevelRange: Single;
    FLevelRangeInv: Single;
    FMaximumAmp: Single;
    FMinimumAmp: Single;
    FOverlapFactor: Integer;
    FUpperFrequency: Single;
    FLowerFrequency: Single;
    FLowerBin: Integer;
    FUpperBin: Integer;
    FBinRange: Integer;
    FMoving: Boolean;
    FWindowFunction: TCustomWindowFunction;
    FOnBlockProcessed: TNotifyEvent;
    procedure SetFFTOrder(const Value: Integer);
    procedure SetLogarithmic(const Value: Boolean);
    procedure SetMaximumLevel(const Value: Single);
    procedure SetMinimumLevel(const Value: Single);
    procedure SetOverlapFactor(const Value: Integer);
    procedure SetLowerFrequency(const Value: Single);
    procedure SetUpperFrequency(const Value: Single);
    procedure SetWindowClass(const Value: TWindowFunctionClass);

    procedure CalculateMaximumAmp;
    procedure CalculateMinimumAmp;
    procedure CalculateLevelRange;
    procedure CalculateOverlap;
    procedure CalculateBinRange;
    procedure CalculateLowerBin;
    procedure CalculateUpperBin;
  protected
    procedure BuildDefaultColorScheme; virtual;
    procedure DrawMagnitudeSlice; virtual; abstract;
    procedure FFTOrderChanged; virtual;
    procedure LowerFrequencyChanged; virtual;
    procedure LogarithmicChanged; virtual;
    procedure MaximumLevelChanged; virtual;
    procedure MinimumLevelChanged; virtual;
    procedure OverlapFactorChanged; virtual;
    procedure UpperFrequencyChanged; virtual;
    procedure SampleRateChanged; override;
    procedure WindowClassChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; virtual;

    property CurrentSlice: Integer read FCurrentSlice;
    property FFTOrder: Integer read FFftOrder write SetFFTOrder default 10;
    property Logarithmic: Boolean read FLogarithmic write SetLogarithmic
      default True;
    property LowerFrequency: Single read FLowerFrequency
      write SetLowerFrequency;
    property MaximumLevel: Single read FMaximumLevel write SetMaximumLevel;
    property MinimumLevel: Single read FMinimumLevel write SetMinimumLevel;
    property Moving: Boolean read FMoving write FMoving default False;
    property OnBlockProcessed: TNotifyEvent read FOnBlockProcessed
      write FOnBlockProcessed;
    property OverlapFactor: Integer read FOverlapFactor write SetOverlapFactor
      default 4;
    property UpperFrequency: Single read FUpperFrequency
      write SetUpperFrequency;
    property WindowClass: TWindowFunctionClass read FWindowClass
      write SetWindowClass;
    property WindowFunction: TCustomWindowFunction read FWindowFunction;
  end;

  TSonogramDirection = (sdUpDown, sdDownUp);

  TCustomBitmapSonogram = class(TCustomSonogram)
  private
    FBitmap: TBitmap;
    FDirection: TSonogramDirection;
    procedure BitmapChangeHandler(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    property Bitmap: TBitmap read FBitmap;
    property Direction: TSonogramDirection read FDirection write FDirection;
  end;

  TCustomBitmapSonogram32 = class(TCustomBitmapSonogram, IDspSink32)
  private
    FBuffer: PDAVComplexSingleFixedArray;
    FMagnitude: PDAVSingleFixedArray;
    procedure ProcessBlock(Sender: TObject; const Input: PDAVSingleFixedArray);
  protected
    procedure DrawMagnitudeSlice; override;
    procedure FFTOrderChanged; override;

    function Touch(Input: Single): Single;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessSample32(Input: Single); virtual;
    procedure ProcessBlock32(const Input: PDAVSingleFixedArray;
      SampleFrames: Integer); overload;
  end;

  TCustomBitmapSonogram64 = class(TCustomBitmapSonogram, IDspSink64)
  private
    FBuffer: PDAVComplexDoubleFixedArray;
    FMagnitude: PDAVDoubleFixedArray;
    procedure ProcessBlock(Sender: TObject; const Input: PDAVDoubleFixedArray);
  protected
    procedure DrawMagnitudeSlice; override;
    procedure FFTOrderChanged; override;

    function Touch(Input: Double): Double;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessSample64(Input: Double); virtual;
    procedure ProcessBlock64(const Input: PDAVDoubleFixedArray;
      SampleFrames: Integer); overload;
  end;

  TBitmapSonogram32 = class(TCustomBitmapSonogram32)
  published
    property FFTOrder;
    property WindowClass;
    property Logarithmic;
    property MinimumLevel;
    property MaximumLevel;
    property LowerFrequency;
    property UpperFrequency;
    property OverlapFactor;
    property Bitmap;
  end;

  TBitmapSonogram64 = class(TCustomBitmapSonogram64)
  published
    property FFTOrder;
    property WindowClass;
    property Logarithmic;
    property MinimumLevel;
    property MaximumLevel;
    property LowerFrequency;
    property UpperFrequency;
    property OverlapFactor;
    property Bitmap;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_Approximations, DAV_DspWindowing;

constructor TCustomSonogram.Create;
begin
  inherited;

  FOverlapFactor := 8;
  FLogarithmic := True;
  FMaximumLevel := 0;
  FMinimumLevel := -96;
  FMoving := False;
  FLowerFrequency := 20;
  FUpperFrequency := 20000;
  FWindowClass := TWindowFunctionHamming;

  FWindowFunction := TWindowFunctionHamming.Create;
  with FWindowFunction do
  begin
    Length := 1 shl FFftOrder;
    Start := 0;
    Tukey := 0;
    Slope := wsSymmetric;
  end;

  CalculateLowerBin;
  CalculateUpperBin;
  CalculateBinRange;
  CalculateMaximumAmp;
  CalculateMinimumAmp;
  CalculateLevelRange;

  BuildDefaultColorScheme;
end;

destructor TCustomSonogram.Destroy;
begin
  FreeAndNil(FWindowFunction);
  inherited;
end;

procedure TCustomSonogram.BuildDefaultColorScheme;
var
  Index: Integer;
  Scale, S: Single;
  R, G, B: Single;
begin
  S := 0.5 / (Length(FColorScheme) - 1);
  for Index := 0 to Length(FColorScheme) - 1 do
  begin
    Scale := Index * S;
    HLSToRGB(0.6 + Scale, 0.1 + Scale, 0.5 + Scale, R, G, B);
    FColorScheme[Index].R := IntLimit(Round(255 * R), 0, 255);
    FColorScheme[Index].G := IntLimit(Round(255 * G), 0, 255);
    FColorScheme[Index].B := IntLimit(Round(255 * B), 0, 255);
  end;
end;

procedure TCustomSonogram.SetFFTOrder(const Value: Integer);
begin
  if FFftOrder <> Value then
  begin
    FFftOrder := Value;
    FFTOrderChanged;
  end;
end;

procedure TCustomSonogram.SetLogarithmic(const Value: Boolean);
begin
  FLogarithmic := Value;
end;

procedure TCustomSonogram.SetLowerFrequency(const Value: Single);
begin
  if FLowerFrequency <> Value then
  begin
    FLowerFrequency := Value;
    LowerFrequencyChanged;
  end;
end;

procedure TCustomSonogram.SetMaximumLevel(const Value: Single);
begin
  if Value <= FMinimumLevel then
    Exit;

  if FMaximumLevel <> Value then
  begin
    FMaximumLevel := Value;
    MaximumLevelChanged;
  end;
end;

procedure TCustomSonogram.SetMinimumLevel(const Value: Single);
begin
  if Value >= FMaximumLevel then
    Exit;

  if FMinimumLevel <> Value then
  begin
    FMinimumLevel := Value;
    MinimumLevelChanged;
  end;
end;

procedure TCustomSonogram.SetOverlapFactor(const Value: Integer);
begin
  if FOverlapFactor <> Value then
  begin
    FOverlapFactor := Value;
    OverlapFactorChanged;
  end;
end;

procedure TCustomSonogram.SetUpperFrequency(const Value: Single);
begin
  if FUpperFrequency <> Value then
  begin
    FUpperFrequency := Value;
    UpperFrequencyChanged;
  end;
end;

procedure TCustomSonogram.SetWindowClass(const Value: TWindowFunctionClass);
begin
  if FWindowClass <> Value then
  begin
    FWindowClass := Value;
    WindowClassChanged;
  end;
end;

procedure TCustomSonogram.WindowClassChanged;
var
  OldWindow: TCustomWindowFunction;
begin
  OldWindow := FWindowFunction;
  FWindowFunction := FWindowClass.Create;

  with FWindowFunction do
  begin
    // Assign(OldWindow);
    Length := 1 shl FFftOrder;
    Start := 0;
    Tukey := 0;
    Slope := wsSymmetric;
  end;
  FreeAndNil(OldWindow);
end;

procedure TCustomSonogram.SampleRateChanged;
begin
  CalculateLowerBin;
  CalculateUpperBin;
  CalculateBinRange;
  inherited;
end;

procedure TCustomSonogram.OverlapFactorChanged;
begin
  CalculateOverlap;
end;

procedure TCustomSonogram.Reset;
begin
  FBlockBuilder.Reset;
end;

procedure TCustomSonogram.LogarithmicChanged;
begin
  Changed;
end;

procedure TCustomSonogram.LowerFrequencyChanged;
begin
  CalculateLowerBin;
  CalculateBinRange;
  Changed;
end;

procedure TCustomSonogram.UpperFrequencyChanged;
begin
  CalculateUpperBin;
  CalculateBinRange;
  Changed;
end;

procedure TCustomSonogram.MaximumLevelChanged;
begin
  CalculateMaximumAmp;
  CalculateLevelRange;
  Changed;
end;

procedure TCustomSonogram.MinimumLevelChanged;
begin
  CalculateMinimumAmp;
  CalculateLevelRange;
  Changed;
end;

procedure TCustomSonogram.CalculateBinRange;
begin
  FBinRange := FUpperBin - FLowerBin;
end;

procedure TCustomSonogram.CalculateLevelRange;
begin
  FLevelRange := FMaximumLevel - FMinimumLevel;
  FLevelRangeInv := 1 / FLevelRange;
end;

procedure TCustomSonogram.CalculateLowerBin;
begin
  FLowerBin := Round((1 shl FFftOrder) * FLowerFrequency / SampleRate)
end;

procedure TCustomSonogram.CalculateOverlap;
begin
  with FBlockBuilder do
    OverlapSize := BlockSize - (BlockSize div FOverlapFactor);
  Changed;
end;

procedure TCustomSonogram.CalculateUpperBin;
begin
  FUpperBin := Round((1 shl FFftOrder) * FUpperFrequency / SampleRate)
end;

procedure TCustomSonogram.CalculateMaximumAmp;
begin
  FMaximumAmp := dB_to_Amp(FMaximumLevel);
end;

procedure TCustomSonogram.CalculateMinimumAmp;
begin
  FMinimumAmp := dB_to_Amp(FMinimumLevel);
end;

procedure TCustomSonogram.FFTOrderChanged;
begin
  CalculateUpperBin;
  CalculateLowerBin;
  CalculateBinRange;
  Changed;
end;

{ TCustomBitmapSonogram }

constructor TCustomBitmapSonogram.Create;
begin
  inherited;
  FBitmap := TBitmap.Create;
  with FBitmap do
  begin
    BeginUpdate;
    try
      Width := 256;
      Height := 256;
      PixelFormat := pf24bit;
      with Canvas do
      begin
        Brush.Color := clBlack;
        FillRect(ClipRect);
      end;
    finally
      EndUpdate;
    end;
    OnChange := BitmapChangeHandler;
  end;
end;

destructor TCustomBitmapSonogram.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TCustomBitmapSonogram.BitmapChangeHandler(Sender: TObject);
begin
  if FCurrentSlice >= FBitmap.Height then
    FCurrentSlice := 0;
end;

{ TCustomBitmapSonogram32 }

constructor TCustomBitmapSonogram32.Create;
begin
  FBlockBuilder := TBuildingBlocksCircular32.Create;
  with TBuildingBlocksCircular32(FBlockBuilder) do
    OnProcess := ProcessBlock;

{$IFDEF Use_IPPS}
  FFft := TFftReal2ComplexIPPSFloat32.Create(10);
{$ELSE} {$IFDEF Use_CUDA}
  FFft := TFftReal2ComplexCUDA32.Create(10);
{$ELSE}
  FFft := TFftReal2ComplexNativeFloat32.Create(10);
  FFft.DataOrder := doPackedComplex;
{$ENDIF}{$ENDIF}
  FFft.AutoScaleType := astDivideBySqrtN; // FwdByN;
  FFftOrder := 10;

  inherited;

  FFTOrderChanged;
end;

destructor TCustomBitmapSonogram32.Destroy;
begin
  Dispose(FBuffer);
  Dispose(FMagnitude);
  FreeAndNil(FBlockBuilder);
  FreeAndNil(FFft);
  inherited;
end;

procedure TCustomBitmapSonogram32.DrawMagnitudeSlice;
var
  Pixel, Bin: Integer;
  Scale: Single;
  ScnLine: array [0 .. 1] of PRGB24Array;
begin
  if FMoving then
  begin
    ScnLine[0] := FBitmap.ScanLine[FBitmap.Height - 1];
    for Pixel := FBitmap.Height - 1 downto 1 do
    begin
      ScnLine[1] := FBitmap.ScanLine[Pixel - 1];
      Move(ScnLine[1]^, ScnLine[0]^, 3 * FBitmap.Width);
      ScnLine[0] := ScnLine[1];
    end;

    // FBitmap.Canvas.Draw(0, 1, FBitmap);
    ScnLine[0] := FBitmap.ScanLine[0];
  end
  else if FDirection = sdUpDown then
    ScnLine[0] := FBitmap.ScanLine[FCurrentSlice]
  else
    ScnLine[0] := FBitmap.ScanLine[FBitmap.Height - FCurrentSlice - 1];

  Scale := FBinRange / FBitmap.Width;
  for Pixel := 0 to FBitmap.Width - 1 do
  begin
    Bin := FLowerBin + Round(Pixel * Scale);
    ScnLine[0]^[Pixel] := FColorScheme
      [IntLimit(Round(255 * FMagnitude^[Bin]), 0, 255)];
  end;
end;

procedure TCustomBitmapSonogram32.FFTOrderChanged;
begin
  inherited;
  FFft.Order := FFftOrder;

  ReallocMem(FBuffer, FFft.BinCount * SizeOf(TComplex32));
  FillChar(FBuffer^, FFft.BinCount * SizeOf(TComplex32), 0);

  FBlockBuilder.BlockSize := FFft.FFTSize;
  FWindowFunction.Length := FFft.FFTSize;
  CalculateOverlap;

  ReallocMem(FMagnitude, FFft.BinCount * SizeOf(Single));
  FillChar(FMagnitude^, FFft.BinCount * SizeOf(Single), 0);
end;

procedure TCustomBitmapSonogram32.ProcessBlock(Sender: TObject;
  const Input: PDAVSingleFixedArray);
var
  Bin: Integer;
begin
  FWindowFunction.ProcessBlock32(Input, FFft.FFTSize);
  FFft.PerformFFT(FBuffer, Input);

  // calculated log magnitude
  FMagnitude^[0] := Touch(Sqr(FBuffer[0].Re));
  for Bin := 0 to FFft.BinCount - 2 do
    FMagnitude^[Bin] := Touch(Sqr(FBuffer[Bin].Re) + Sqr(FBuffer[Bin].Im));
  FMagnitude^[FFft.BinCount - 1] := Touch(Sqr(FBuffer[FFft.BinCount - 1].Re));

  DrawMagnitudeSlice;

  Inc(FCurrentSlice);
  if FCurrentSlice >= FBitmap.Height then
    FCurrentSlice := 0;

  if Assigned(FOnBlockProcessed) then
    FOnBlockProcessed(Self);
end;

procedure TCustomBitmapSonogram32.ProcessBlock32(const Input
  : PDAVSingleFixedArray; SampleFrames: Integer);
begin
  TBuildingBlocksCircular32(FBlockBuilder).ProcessBlock32(@Input[0],
    SampleFrames);
end;

procedure TCustomBitmapSonogram32.ProcessSample32(Input: Single);
begin
  TBuildingBlocksCircular32(FBlockBuilder).ProcessSample32(Input);
end;

function TCustomBitmapSonogram32.Touch(Input: Single): Single;
begin
  if FLogarithmic then
    Result := ((0.5 * FastAmptodBMinError3(Input)) - FMinimumLevel) *
      FLevelRangeInv
  else
    Result := Sqrt(Input);
end;

{ TCustomBitmapSonogram64 }

constructor TCustomBitmapSonogram64.Create;
begin
  FBlockBuilder := TBuildingBlocksCircular64.Create;
  with TBuildingBlocksCircular64(FBlockBuilder) do
    OnProcess := ProcessBlock;

{$IFDEF Use_IPPS}
  FFft := TFftReal2ComplexIPPSFloat64.Create(10);
{$ELSE} {$IFDEF Use_CUDA}
  FFft := TFftReal2ComplexCUDA64.Create(10);
{$ELSE}
  FFft := TFftReal2ComplexNativeFloat64.Create(10);
  FFft.DataOrder := doPackedComplex;
{$ENDIF}{$ENDIF}
  FFft.AutoScaleType := astDivideBySqrtN;
  FFftOrder := 10;

  inherited;

  FFTOrderChanged;
end;

destructor TCustomBitmapSonogram64.Destroy;
begin
  Dispose(FBuffer);
  Dispose(FMagnitude);
  FreeAndNil(FBlockBuilder);
  FreeAndNil(FFft);
  inherited;
end;

procedure TCustomBitmapSonogram64.DrawMagnitudeSlice;
var
  Pixel, Bin: Integer;
  Scale: Double;
  ScnLine: array [0 .. 1] of PRGB24Array;
begin
  if FMoving then
  begin
    ScnLine[0] := FBitmap.ScanLine[FBitmap.Height - 1];
    for Pixel := FBitmap.Height - 1 downto 1 do
    begin
      ScnLine[1] := FBitmap.ScanLine[Pixel - 1];
      Move(ScnLine[1]^, ScnLine[0]^, 3 * FBitmap.Width);
      ScnLine[0] := ScnLine[1];
    end;
  end
  else
    ScnLine[0] := FBitmap.ScanLine[FCurrentSlice];

  Scale := FBinRange / FBitmap.Width;
  for Pixel := 0 to FBitmap.Width - 1 do
  begin
    Bin := FLowerBin + Round(Pixel * Scale);
    ScnLine[0]^[Pixel] := FColorScheme
      [IntLimit(Round(255 * FMagnitude^[Bin]), 0, 255)];
  end;
end;

procedure TCustomBitmapSonogram64.FFTOrderChanged;
begin
  inherited;
  FFft.Order := FFftOrder;

  ReallocMem(FBuffer, FFft.BinCount * SizeOf(TComplex64));
  FillChar(FBuffer^, FFft.BinCount * SizeOf(TComplex64), 0);

  FBlockBuilder.BlockSize := FFft.FFTSize;
  FWindowFunction.Length := FFft.FFTSize;
  CalculateOverlap;

  ReallocMem(FMagnitude, FFft.BinCount * SizeOf(Double));
  FillChar(FMagnitude^, FFft.BinCount * SizeOf(Double), 0);
end;

procedure TCustomBitmapSonogram64.ProcessBlock(Sender: TObject;
  const Input: PDAVDoubleFixedArray);
var
  Bin: Integer;
begin
  FFft.PerformFFT(FBuffer, Input);

  // calculated log magnitude
  FMagnitude^[0] := Touch(Sqr(FBuffer[0].Re));
  for Bin := 0 to FFft.BinCount - 2 do
    FMagnitude^[Bin] := Touch(Sqr(FBuffer[Bin].Re) + Sqr(FBuffer[Bin].Im));
  FMagnitude^[FFft.BinCount - 1] := Touch(Sqr(FBuffer[FFft.BinCount - 1].Re));

  DrawMagnitudeSlice;

  Inc(FCurrentSlice);
  if FCurrentSlice >= FBitmap.Height then
    FCurrentSlice := 0;
end;

procedure TCustomBitmapSonogram64.ProcessBlock64(const Input
  : PDAVDoubleFixedArray; SampleFrames: Integer);
begin
  TBuildingBlocksCircular64(FBlockBuilder).ProcessBlock64(@Input[0],
    SampleFrames);
end;

procedure TCustomBitmapSonogram64.ProcessSample64(Input: Double);
begin
  TBuildingBlocksCircular64(FBlockBuilder).ProcessSample64(Input);
end;

function TCustomBitmapSonogram64.Touch(Input: Double): Double;
begin
  if FLogarithmic then
    Result := ((0.5 * FastAmptodBMinError3(Input)) - FMinimumLevel) *
      FLevelRangeInv
  else
    Result := Sqrt(Input);
end;

end.
