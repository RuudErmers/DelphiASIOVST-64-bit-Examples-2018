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

unit DAV_DifferentialEvolution;

// The code is based on code by Laurent de Soras, which was itself based
// on an implementation by Olli Niemitalo and Magnus Jonsson
// see http://ldesoras.free.fr/prod.html for more details
// It was reviewed and rewritten from scratch by Christian-W. Budde

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, SysUtils, Classes;

type
  TDifferentialEvolutionPopulation = TDAVDoubleDynArray;
  TDifferentialEvolutionEvent = function(Sender: TObject;
    var Population: TDifferentialEvolutionPopulation): Double of object;

  TEvaluatedPopulation = class(TObject)
  private
    FData: TDifferentialEvolutionPopulation;
    FCost: Double;
    FValidCostFlag: Boolean;
    function GetData(Index: Integer): Double;
    function GetDataCount: Integer;
    procedure SetDataCount(const Value: Integer);
  protected
    procedure DataCountChanged; virtual;
  public
    constructor Create; overload;
    constructor Create(VariableCount: Integer); overload;
    constructor Create(const DiffEvolPopulation
      : TDifferentialEvolutionPopulation; Cost: Double); overload;

    property Cost: Double read FCost write FCost;
    property ValidCostFlag: Boolean read FValidCostFlag write FValidCostFlag;
    property Data[Index: Integer]: Double read GetData;
    property DataCount: Integer read GetDataCount write SetDataCount;
  end;

  TDifferentialEvolution = class(TComponent)
  private
    FPopulationCount: Integer; // Number of populations
    FVariableCount: Integer; // Number of variables in population
    FBestPopulationIndex: Integer; // Negative if not yet determinated
    FGainBest: Double;
    FGainBase: Double;
    FGainR1: Double;
    FGainR2: Double;
    FGainR3: Double;
    FCrossOver: Double;
    FHasBestPopulation: Boolean;
    FMinConstraints: TDifferentialEvolutionPopulation;
    FMaxConstraints: TDifferentialEvolutionPopulation;
    FBestPopulation: TDifferentialEvolutionPopulation;
    FCurrentGeneration: array of TEvaluatedPopulation;
    FNextGeneration: array of TEvaluatedPopulation;
    function GetBestPopulationData(Index: Integer): Double;
    function GetMaxConstraints(Index: Integer): Double;
    function GetMinConstraints(Index: Integer): Double;
    function GetPopulation(Index: Integer): TDifferentialEvolutionPopulation;
    procedure SetBestPopulationData(Index: Integer; const Value: Double);
    procedure SetCrossOver(const Value: Double);
    procedure SetGainBest(const Value: Double);
    procedure SetGainR1(const Value: Double);
    procedure SetGainR2(const Value: Double);
    procedure SetGainR3(const Value: Double);
    procedure SetMaxConstraints(Index: Integer; const Value: Double);
    procedure SetMinConstraints(Index: Integer; const Value: Double);
    procedure SetPopulationCount(const Value: Integer);
    procedure SetVariableCount(const Value: Integer);
    procedure SetPopulation(Index: Integer;
      const Value: TDifferentialEvolutionPopulation);
  protected
    FOnCalculateCosts: TDifferentialEvolutionEvent;
    FOnInitPopulation: TDifferentialEvolutionEvent;
    FAutoInitialize: Boolean;
    function FindBest: Double; virtual;
    procedure CalculateGainR0; virtual;
    procedure PopulationCountChanged; virtual;
    procedure RandomizePopulation; virtual;
    procedure RecreatePopulation; virtual;
    procedure VariableCountChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(InitializePopulation: Boolean = True);
    function Evolve: Double;
    function GetBestPopulation: TDifferentialEvolutionPopulation;
    function GetBestCost: Double;
    property MinConstraints[Index: Integer]: Double read GetMinConstraints
      write SetMinConstraints;
    property MaxConstraints[Index: Integer]: Double read GetMaxConstraints
      write SetMaxConstraints;
    property BestPopulation[Index: Integer]: Double read GetBestPopulationData
      write SetBestPopulationData;
    property Population[Index: Integer]: TDifferentialEvolutionPopulation
      read GetPopulation write SetPopulation;
  published
    property OnCalculateCosts: TDifferentialEvolutionEvent
      read FOnCalculateCosts write FOnCalculateCosts;
    property OnInitPopulation: TDifferentialEvolutionEvent
      read FOnInitPopulation write FOnInitPopulation;
    property PopulationCount: Integer read FPopulationCount
      write SetPopulationCount;
    property VariableCount: Integer read FVariableCount write SetVariableCount;
    property GainBest: Double read FGainBest write SetGainBest;
    property GainR1: Double read FGainR1 write SetGainR1;
    property GainR2: Double read FGainR2 write SetGainR2;
    property GainR3: Double read FGainR3 write SetGainR3;
    property CrossOver: Double read FCrossOver write SetCrossOver;
    property AutoInitialize: Boolean read FAutoInitialize write FAutoInitialize;
  end;

implementation

uses
  Math;

resourcestring
  RCStrCrossOverBoundError = 'CrossOver must be 0 <= x <= 1';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrConstraintsError = 'Min < Max, please!';
  RCStrVariableCountMismatch = 'Variable count mismatch';

  { TEvaluatedPopulation }

constructor TEvaluatedPopulation.Create;
begin
  inherited;
  FCost := 0;
  FValidCostFlag := False;
end;

constructor TEvaluatedPopulation.Create(VariableCount: Integer);
begin
  Create;
  SetLength(FData, VariableCount);
end;

constructor TEvaluatedPopulation.Create(const DiffEvolPopulation
  : TDifferentialEvolutionPopulation; Cost: Double);
begin
  inherited Create;
  FCost := Cost;
  FData := DiffEvolPopulation;
  FValidCostFlag := True;
end;

function TEvaluatedPopulation.GetData(Index: Integer): Double;
begin
  if (Index > 0) and (Index <= Length(FData)) then
    Result := FData[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TEvaluatedPopulation.GetDataCount: Integer;
begin
  Result := Length(FData);
end;

procedure TEvaluatedPopulation.SetDataCount(const Value: Integer);
begin
  if Value <> DataCount then
  begin
    SetLength(FData, Value);
    DataCountChanged;
  end;
end;

procedure TEvaluatedPopulation.DataCountChanged;
begin
  // nothing in here yet
end;

{ TDifferentialEvolution }

constructor TDifferentialEvolution.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Randomize;
  FGainR1 := -0.7;
  FGainR2 := 0.7;
  FGainR3 := 1;
  FGainBest := 0;
  FCrossOver := 1;
  FHasBestPopulation := False;
  CalculateGainR0;

  FBestPopulationIndex := -1;
  FPopulationCount := 100;
  PopulationCountChanged;

  FVariableCount := 1;
  VariableCountChanged;
end;

destructor TDifferentialEvolution.Destroy;
var
  Generation: Integer;
begin
  for Generation := 0 to Length(FCurrentGeneration) - 1 do
    if Assigned(FCurrentGeneration[Generation]) then
      FCurrentGeneration[Generation].Free;

  for Generation := 0 to Length(FNextGeneration) - 1 do
    if Assigned(FNextGeneration[Generation]) then
      FNextGeneration[Generation].Free;

  inherited;
end;

// Name: Evolve
// ------------
//
// Description:
// Compute the next generation, trying to find a population which cost is
// lower than the previous one. Note: coefficient for the original
// population is 1.0 minus sum of gain_*. The coefficients can be
// negative as well as positive, and be higher than 1.
//
// Input/output parameters:
// - func: Object implementing the cost function. It shall accept
// populations of the size given at the DiffEvol creation.
//
// Returns: The new best cost.

function TDifferentialEvolution.Evolve: Double;
var
  NewBestPopIndex: Integer;
  NewBestCost: Double;
  RandomPopIndex: array [0 .. 2] of Integer;
  PopIndex: Integer;
  VarIndex: Integer;
  VarCount: Integer;
  Index: Integer;
begin
  if (FBestPopulationIndex < 0) then
    FindBest;

  NewBestPopIndex := FBestPopulationIndex;
  NewBestCost := FCurrentGeneration[FBestPopulationIndex].FCost;

  for PopIndex := 0 to FPopulationCount - 1 do
  begin
    // Find 3 different populations randomly
    repeat
      RandomPopIndex[0] := Random(FPopulationCount);
    until (RandomPopIndex[0] <> PopIndex) and
      (RandomPopIndex[0] <> FBestPopulationIndex);

    repeat
      RandomPopIndex[1] := Random(FPopulationCount);
    until (RandomPopIndex[1] <> PopIndex) and
      (RandomPopIndex[1] <> FBestPopulationIndex) and
      (RandomPopIndex[1] <> RandomPopIndex[0]);

    repeat
      RandomPopIndex[2] := Random(FPopulationCount);
    until (RandomPopIndex[2] <> PopIndex) and
      (RandomPopIndex[2] <> FBestPopulationIndex) and
      (RandomPopIndex[2] <> RandomPopIndex[1]) and
      (RandomPopIndex[2] <> RandomPopIndex[0]);

    // Generate trial vector with crossing-over
    VarIndex := Random(FVariableCount);
    VarCount := 0;

    repeat
      FNextGeneration[PopIndex].FData[VarIndex] := FCurrentGeneration[PopIndex]
        .FData[VarIndex] * FGainBase + FCurrentGeneration[RandomPopIndex[0]]
        .FData[VarIndex] * FGainR1 + FCurrentGeneration[RandomPopIndex[1]].FData
        [VarIndex] * FGainR2 + FCurrentGeneration[RandomPopIndex[2]].FData
        [VarIndex] * FGainR3 + FCurrentGeneration[FBestPopulationIndex].FData
        [VarIndex] * FGainBest;
      Inc(VarIndex);
      if VarIndex >= FVariableCount then
        VarIndex := 0;
      Inc(VarCount);
    until (VarCount >= FVariableCount) or (Random >= FCrossOver);

    while (VarCount < FVariableCount) do
    begin
      FNextGeneration[PopIndex].FData[VarIndex] := FCurrentGeneration[PopIndex]
        .FData[VarIndex];
      Inc(VarIndex);
      if VarIndex >= FVariableCount then
        VarIndex := 0;
      Inc(VarCount);
    end;

    // Evaluate the new PopIndex
    FNextGeneration[PopIndex].FCost := FOnCalculateCosts(Self,
      FNextGeneration[PopIndex].FData);
    FNextGeneration[PopIndex].FValidCostFlag := True;

    if (FNextGeneration[PopIndex].FCost < FCurrentGeneration[PopIndex].FCost)
    then
    begin
      if (FNextGeneration[PopIndex].FCost < NewBestCost) then
      begin // New best
        NewBestPopIndex := PopIndex;
        NewBestCost := FNextGeneration[PopIndex].FCost;
      end;
      Move(FNextGeneration[PopIndex].FData[0],
        FCurrentGeneration[PopIndex].FData[0], FVariableCount * SizeOf(Double));
      FCurrentGeneration[PopIndex].FCost := FNextGeneration[PopIndex].FCost;
      FCurrentGeneration[PopIndex].FValidCostFlag := FNextGeneration[PopIndex]
        .FValidCostFlag;
    end;
  end;

  FBestPopulationIndex := NewBestPopIndex;
  for Index := 0 to FVariableCount - 1 do
    FBestPopulation[Index] := FCurrentGeneration[FBestPopulationIndex]
      .FData[Index];
  Result := NewBestCost;
end;

// Name: GetBestPopulation
// -----------------------
//
// Description:
// Return the current best population. This function must not be called
// before the first call to evolve ().
//
// Returns: A reference on the population. It remains valid until
// subsequent call to evolve() function.

function TDifferentialEvolution.GetBestPopulation
  : TDifferentialEvolutionPopulation;
begin
  Assert(FBestPopulationIndex >= 0);
  Result := FCurrentGeneration[FBestPopulationIndex].FData;
end;

// Name: GetBestCost
// -----------------
//
// Description:
// Return the cost evaluation for the current best population. This
// function must not be called before the first call to evolve ().
//
// Returns: The cost.

function TDifferentialEvolution.GetBestCost: Double;
begin
  Assert(FBestPopulationIndex >= 0);
  Assert(FCurrentGeneration[FBestPopulationIndex].FValidCostFlag);
  Result := FCurrentGeneration[FBestPopulationIndex].FCost;
end;

procedure TDifferentialEvolution.Initialize(InitializePopulation
  : Boolean = True);
var
  PopulationIndex: Integer;
  VariableIndex: Integer;
begin
  // Initialize populations with random values
  FBestPopulationIndex := -1;

  for PopulationIndex := 0 to FPopulationCount - 1 do
  begin
    FCurrentGeneration[PopulationIndex].FValidCostFlag := False;
    FCurrentGeneration[PopulationIndex].FCost := 0;
    FNextGeneration[PopulationIndex].FValidCostFlag := False;
    FNextGeneration[PopulationIndex].FCost := 0;
  end;

  if InitializePopulation then
  begin
    RandomizePopulation;

    // Introduce the "best" population if it is provided
    if FHasBestPopulation then
    begin
      for VariableIndex := 0 to FVariableCount - 1 do
        FCurrentGeneration[0].FData[VariableIndex] :=
          FBestPopulation[VariableIndex];

      // evaluate the best population cost and set best population index
      if Assigned(FOnCalculateCosts) then
      begin
        FBestPopulationIndex := 0;
        FCurrentGeneration[0].FCost := FOnCalculateCosts(Self,
          FCurrentGeneration[0].FData);
        FCurrentGeneration[0].FValidCostFlag := True;
      end;
    end;
  end;
end;

procedure TDifferentialEvolution.RandomizePopulation;
var
  Offset: Double;
  Mul: Double;
  Index: Integer;
  PopIndex: Integer;
begin
  if Assigned(FOnInitPopulation) then
    for PopIndex := 0 to FPopulationCount - 1 do
      FOnInitPopulation(Self, FCurrentGeneration[PopIndex].FData)
  else

    for Index := 0 to FVariableCount - 1 do
    begin
      Assert(FMinConstraints[Index] <= FMaxConstraints[Index]);
      Offset := FMinConstraints[Index];
      Mul := FMaxConstraints[Index] - FMinConstraints[Index];
      for PopIndex := 0 to FPopulationCount - 1 do
        FCurrentGeneration[PopIndex].FData[Index] := Offset + Random * Mul;
    end;

  FBestPopulationIndex := -1;
end;

function TDifferentialEvolution.FindBest;
var
  CurCost: Double;
  Pop, Index: Integer;
begin
  if (FBestPopulationIndex < 0) then
    FBestPopulationIndex := 0;

  with FCurrentGeneration[FBestPopulationIndex] do
    if (not FValidCostFlag) then
    begin
      FCost := FOnCalculateCosts(Self, FData);
      FValidCostFlag := True;
    end;

  Result := FCurrentGeneration[FBestPopulationIndex].FCost;
  for Pop := 0 to FPopulationCount - 1 do
  begin
    if (Pop <> FBestPopulationIndex) then
    begin
      CurCost := FOnCalculateCosts(Self, FCurrentGeneration[Pop].FData);
      FCurrentGeneration[Pop].FCost := CurCost;
      FCurrentGeneration[Pop].FValidCostFlag := True;
      if (CurCost < Result) then
      begin
        FBestPopulationIndex := Pop;
        Result := CurCost;
      end;
    end;
  end;

  for Index := 0 to FVariableCount - 1 do
    FBestPopulation[Index] := FCurrentGeneration[FBestPopulationIndex]
      .FData[Index];
end;

procedure TDifferentialEvolution.SetVariableCount(const Value: Integer);
begin
  Assert(Value > 0);
  if Value <> FVariableCount then
  begin
    FVariableCount := Value;
    VariableCountChanged;
  end;
end;

procedure TDifferentialEvolution.SetPopulation(Index: Integer;
  const Value: TDifferentialEvolutionPopulation);
begin
  if (Index >= 0) and (Index < Length(FCurrentGeneration)) then
  begin
    if Length(Value) = VariableCount then
      Move(Value, FCurrentGeneration[Index].FData[0],
        VariableCount * SizeOf(Double))
    else
      raise Exception.Create(RCStrVariableCountMismatch);
  end
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TDifferentialEvolution.SetPopulationCount(const Value: Integer);
begin
  Assert(Value >= 5);

  if FPopulationCount <> Value then
  begin
    FPopulationCount := Value;
    PopulationCountChanged;
  end;
end;

procedure TDifferentialEvolution.VariableCountChanged;
var
  Index: Integer;
begin
  SetLength(FMinConstraints, FVariableCount);
  SetLength(FMaxConstraints, FVariableCount);
  SetLength(FBestPopulation, FVariableCount);
  for Index := 0 to FVariableCount - 1 do
  begin
    FMinConstraints[Index] := -100;
    FMaxConstraints[Index] := 100;
    FBestPopulation[Index] := 0;
  end;
  RecreatePopulation;
end;

procedure TDifferentialEvolution.PopulationCountChanged;
var
  Index: Integer;
begin
  for Index := 0 to Length(FCurrentGeneration) - 1 do
    if Assigned(FCurrentGeneration[Index]) then
      FreeAndNil(FCurrentGeneration[Index]);
  for Index := 0 to Length(FNextGeneration) - 1 do
    if Assigned(FNextGeneration[Index]) then
      FreeAndNil(FNextGeneration[Index]);

  // Size all the arrays
  SetLength(FCurrentGeneration, FPopulationCount);
  SetLength(FNextGeneration, FPopulationCount);
  RecreatePopulation;
end;

function TDifferentialEvolution.GetMaxConstraints(Index: Integer): Double;
begin
  if (Index < 0) or (Index >= Length(FMaxConstraints)) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  Result := FMaxConstraints[Index];
end;

function TDifferentialEvolution.GetMinConstraints(Index: Integer): Double;
begin
  if (Index < 0) or (Index >= Length(FMinConstraints)) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  Result := FMinConstraints[Index];
end;

function TDifferentialEvolution.GetPopulation(Index: Integer)
  : TDifferentialEvolutionPopulation;
begin
  if (Index >= 0) and (Index < Length(FCurrentGeneration)) then
    Result := FCurrentGeneration[Index].FData
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TDifferentialEvolution.GetBestPopulationData(Index: Integer): Double;
begin
  if (Index < 0) or (Index >= Length(FBestPopulation)) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  Result := FBestPopulation[Index];
end;

procedure TDifferentialEvolution.SetBestPopulationData(Index: Integer;
  const Value: Double);
begin
  if (Index < 0) or (Index >= Length(FBestPopulation)) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  FBestPopulation[Index] := Value;
  FHasBestPopulation := True;
end;

procedure TDifferentialEvolution.SetMinConstraints(Index: Integer;
  const Value: Double);
begin
  if (Index < 0) or (Index >= Length(FMinConstraints)) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  if Value > FMaxConstraints[Index] then
    raise Exception.Create(RCStrConstraintsError);

  if FMinConstraints[Index] <> Value then
  begin
    FMinConstraints[Index] := Value;
    if FAutoInitialize then
      Initialize;
  end;
end;

procedure TDifferentialEvolution.SetMaxConstraints(Index: Integer;
  const Value: Double);
begin
  if (Index < 0) or (Index >= Length(FMaxConstraints)) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  if Value < FMinConstraints[Index] then
    raise Exception.Create(RCStrConstraintsError);

  if FMaxConstraints[Index] <> Value then
  begin
    FMaxConstraints[Index] := Value;
    if FAutoInitialize then
      Initialize;
  end;
end;

procedure TDifferentialEvolution.SetCrossOver(const Value: Double);
begin
  if not((Value >= 0) and (Value <= 1)) then
    raise Exception.Create(RCStrCrossOverBoundError);
  FCrossOver := Value;
end;

procedure TDifferentialEvolution.SetGainBest(const Value: Double);
begin
  if FGainBest <> Value then
  begin
    FGainBest := Value;
    CalculateGainR0;
  end;
end;

procedure TDifferentialEvolution.SetGainR1(const Value: Double);
begin
  if FGainR1 <> Value then
  begin
    FGainR1 := Value;
    CalculateGainR0;
  end;
end;

procedure TDifferentialEvolution.SetGainR2(const Value: Double);
begin
  if FGainR2 <> Value then
  begin
    FGainR2 := Value;
    CalculateGainR0;
  end;
end;

procedure TDifferentialEvolution.SetGainR3(const Value: Double);
begin
  if FGainR3 <> Value then
  begin
    FGainR3 := Value;
    CalculateGainR0;
  end;
end;

procedure TDifferentialEvolution.CalculateGainR0;
begin
  FGainBase := 1 - FGainBest - FGainR1 - FGainR2 - FGainR3;
end;

procedure TDifferentialEvolution.RecreatePopulation;
var
  Index: Integer;
begin
  for Index := 0 to FPopulationCount - 1 do
  begin
    if not Assigned(FCurrentGeneration[Index]) then
      FCurrentGeneration[Index] := TEvaluatedPopulation.Create(FVariableCount)
    else
    begin
      FCurrentGeneration[Index].DataCount := FVariableCount;
      FCurrentGeneration[Index].FValidCostFlag := False;
    end;

    if not Assigned(FNextGeneration[Index]) then
      FNextGeneration[Index] := TEvaluatedPopulation.Create(FVariableCount)
    else
    begin
      FNextGeneration[Index].DataCount := FVariableCount;
      FNextGeneration[Index].FValidCostFlag := False;
    end;
  end;

  if AutoInitialize then
    Initialize;
end;

end.
