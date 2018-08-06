unit DAV_DspPolyphaseFilter;

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
//  The code is based on the HIIR code by Laurent de Soras, which             //
//  can be found at http://ldesoras.free.fr/prod.html#src_hiir                //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspPolyphaseIirDesigner;

type
  TCustomPolyphaseFilter = class(TDspPersistent)
  private
    procedure SetNumberOfCoeffs(const Value: Integer);
    procedure SetTransition(const Value: Double);
  protected
    FCoefficients   : PDAVDoubleFixedArray;
    FNumberOfCoeffs : Integer;
    FTransition     : Double;
    FAttenuation    : Double;
    procedure NumberOfCoeffsChanged; virtual;
    procedure TransitionChanged; virtual;
    procedure ChooseProcedures; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;

    property Coefficients: PDAVDoubleFixedArray read FCoefficients;
  public
    constructor Create; overload; virtual;
    constructor Create(const NumberOfCoefficients: Integer;
      const Transition: Double); overload; virtual;
    destructor Destroy; override;

    procedure SetCoefficients(const Coefficients: TDAVDoubleDynArray); overload; virtual;
    procedure SetCoefficients(const NumberOfCoefficients: Integer = 8; const Transition: Double = 0.1); overload; virtual;

    property NumberOfCoefficients: Integer read FNumberOfCoeffs write SetNumberOfCoeffs;
    property Attenuation: Double read FAttenuation;
    property Transition: Double read FTransition write SetTransition;
  end;

implementation

constructor TCustomPolyphaseFilter.Create;
begin
 inherited Create;
 FCoefficients := nil;
 FTransition := 0.1;
 FNumberOfCoeffs := 8;
 NumberOfCoeffsChanged;
end;

constructor TCustomPolyphaseFilter.Create(const NumberOfCoefficients: Integer;
  const Transition: Double);
begin
 inherited Create;
 FCoefficients := nil;
 FTransition := Transition;
 FNumberOfCoeffs := NumberOfCoefficients;
 NumberOfCoeffsChanged;
end;

destructor TCustomPolyphaseFilter.Destroy;
begin
 Dispose(FCoefficients);
 inherited;
end;

procedure TCustomPolyphaseFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomPolyphaseFilter then
  with TCustomPolyphaseFilter(Dest) do
   begin
    FNumberOfCoeffs := Self.FNumberOfCoeffs;
    FTransition     := Self.FTransition;
    FAttenuation    := Self.FAttenuation;
    NumberOfCoeffsChanged;
   end
 else inherited;
end;

procedure TCustomPolyphaseFilter.SetCoefficients(const Coefficients: TDAVDoubleDynArray);
begin
  // make sure the coefficients are not empty
  Assert(Length(Coefficients) > 0);

  // check if the number of coeffitients changed
  if FNumberOfCoeffs <> Length(Coefficients) then
   begin
    FNumberOfCoeffs := Length(Coefficients);
    NumberOfCoeffsChanged;
   end;

  // actually copy the coefficients
  Move(Coefficients[0], FCoefficients[0], FNumberOfCoeffs * SizeOf(Double));
end;

procedure TCustomPolyphaseFilter.SetCoefficients(
  const NumberOfCoefficients: Integer = 8;
  const Transition: Double = 0.1);
var
  tmpCoeffs: TDAVDoubleDynArray;
begin
  SetLength(tmpCoeffs, NumberOfCoefficients);
  TPolyphaseIirDesigner.ComputeCoeffsSpecOrderTBW(tmpCoeffs, NumberOfCoefficients, Transition);
  FAttenuation := TPolyphaseIirDesigner.ComputeAttenuationFromOrderTBW(FNumberOfCoeffs, FTransition);
  SetCoefficients(tmpCoeffs);
end;

procedure TCustomPolyphaseFilter.NumberOfCoeffsChanged;
begin
 ReallocMem(FCoefficients, FNumberOfCoeffs * SizeOf(Double));
 SetCoefficients(FNumberOfCoeffs, FTransition);
 Changed;
end;

procedure TCustomPolyphaseFilter.TransitionChanged;
begin
 SetCoefficients(FNumberOfCoeffs, FTransition);
 Changed;
end;

procedure TCustomPolyphaseFilter.SetNumberOfCoeffs(const Value: Integer);
begin
 if FNumberOfCoeffs <> Value then
  begin
   FNumberOfCoeffs := Value;
   NumberOfCoeffsChanged;
  end;
end;

procedure TCustomPolyphaseFilter.SetTransition(const Value: Double);
begin
 if FTransition <> Value then
  begin
   FTransition := Value;
   TransitionChanged;
  end;
end;

end.
