unit SimpleSonogramDM;

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
////////////////////////////////////////////////////////////////////////////////

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_Complex, DAV_Classes, DAV_Sonogram, 
  DAV_VSTModule, DAV_DspWindowFunctions, DAV_DspWindowFunctionsAdvanced;

type
  TSonogramDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterIntegerDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOverlapFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWindowDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterWindowChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FSonogram        : TBitmapSonogram32;
  public
    property Sonogram: TBitmapSonogram32 read FSonogram;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
 DAV_Approximations, DAV_VSTParameters, SimpleSonogramGui;

procedure TSonogramDataModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(numInputs = 1);
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSonogramDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSonogramDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSonogram := TBitmapSonogram32.Create;
 with FSonogram do
  begin
   SampleRate   := Self.SampleRate;
   WindowClass  := TWindowFunctionBlackman;
   Logarithmic  := True;
   MaximumLevel := 6;
   MinimumLevel := -96;
  end;

 with ParameterProperties[2] do
  begin
   Max := Length(GWindowFunctions) - 1;
   MaxInteger := Length(GWindowFunctions) - 1;
  end;

 // initialize parameters
 Parameter[0] := 10;
 Parameter[1] :=  8;
 Parameter[2] :=  4;

 // set editor form class
 EditorFormClass := TFmSonogram;
end;

procedure TSonogramDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSonogram);
end;

procedure TSonogramDataModule.ParameterIntegerDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure TSonogramDataModule.ParameterWindowDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName;
end;

procedure TSonogramDataModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FSonogram.FFTOrder := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSonogramDataModule.ParameterOverlapFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FSonogram.OverlapFactor := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSonogramDataModule.ParameterWindowChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FSonogram.WindowClass := GWindowFunctions[Round(Parameter[Index])];
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSonogramDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
 FCriticalSection.Enter;
 try
  FSonogram.ProcessBlock32(@Inputs[0, 0], SampleFrames);
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
 finally
  FCriticalSection.Leave;
 end;
end;

end.
