unit SpectralSmoothingDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, Classes, Forms, SyncObjs, DAV_Types, DAV_Complex,
  DAV_DspSpectralFilters, DAV_VSTModule, DAV_DspWindowFunctions 
  {$IFDEF Use_IPPS}, DAV_DspWindowFunctionsAdvanced{$ENDIF};

type
  TSpectralSelfFilterModule = class(TVSTModule)
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterWindowFunctionDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterWindowFunctionChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFftOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFftOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FSpectralFilter  : array of TSpectralSmoothing32;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  SysUtils, Math, DAV_VSTModuleWithPrograms;

procedure TSpectralSelfFilterModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSpectralSelfFilterModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSpectralSelfFilterModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(numInputs = numOutputs);

 SetLength(FSpectralFilter, numInputs);
 for Channel := 0 to Length(FSpectralFilter) - 1 do
  begin
   FSpectralFilter[Channel] := TSpectralSmoothing32.Create;
   with FSpectralFilter[Channel] do
    begin
     FftOrder := Round(Log2(BlockModeSize));
     Assert(FftSize = BlockModeSize);
    end;
  end;

 with ParameterProperties[2] do
  begin
   Max := Length(GWindowFunctions) - 1;
   MaxInteger := Length(GWindowFunctions) - 1;
  end;

 Parameter[0] := -30;
 Parameter[1] :=  12;
 Parameter[2] :=   4;
end;

procedure TSpectralSelfFilterModule.ParameterWindowFunctionDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName;
end;

procedure TSpectralSelfFilterModule.ParameterFftOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralFilter) - 1
   do FSpectralFilter[Channel].FFTOrder := Round(Value);

  InitialDelay := 1 shl Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpectralSelfFilterModule.ParameterFftOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TSpectralSelfFilterModule.ParameterWindowFunctionChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralFilter) - 1
   do FSpectralFilter[Channel].WindowFunctionClass := GWindowFunctions[Round(Parameter[Index])];
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpectralSelfFilterModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralFilter) - 1
  do FreeAndNil(FSpectralFilter[Channel]);
end;

procedure TSpectralSelfFilterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralFilter) - 1 do
   begin
    FSpectralFilter[Channel].ProcessBlock(@Inputs[Channel, 0],
      @Outputs[Channel, 0], SampleFrames);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
