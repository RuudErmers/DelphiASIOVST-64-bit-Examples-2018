unit TunerDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTModule, DAV_DspTuner;

type
  TTunerDataModule = class(TVSTModule)
    procedure ParameterNoteDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterGuitarStringChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FTuner : TAdvancedTuner;
  public
    property Tuner : TAdvancedTuner read FTuner;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  TunerGUI;

procedure TTunerDataModule.VSTModuleOpen(Sender: TObject);
begin
 FTuner := TAdvancedTuner.Create;
 FTuner.OneCrossingOnly := True;
 FTuner.SampleRate := SampleRate;
 FTuner.Threshold := 0.1;
 FTuner.Attack := 0.1;
 FTuner.Release := 1;
 FTuner.SmoothFactor := 0.99;

 // initialize parameters
 Parameter[0] := 2;

 // set editor form class
 EditorFormClass := TFmTuner;
end;

procedure TTunerDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FTuner);
end;

procedure TTunerDataModule.ParameterGuitarStringChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  CenterFrequency : Single;
begin
 case Round(Parameter[Index]) of
  1 : CenterFrequency := 329.62755691286992973584176104656;
  2 : CenterFrequency := 440;
  3 : CenterFrequency := 587.32953583481512052556602772116;
  4 : CenterFrequency := 783.99087196349858817139906091965;
  5 : CenterFrequency := 987.76660251224822366150908371768;
  6 : CenterFrequency := 1318.5102276514797189433670441862;
  else raise Exception.Create('Current Frequency doesn''t exist');
 end;

 FTuner.MinimumFrequency := 0.5 * CenterFrequency;
 FTuner.MinimumFrequency := 2 * CenterFrequency;
end;

procedure TTunerDataModule.ParameterNoteDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  1 : PreDefined := 'E';
  2 : PreDefined := 'A';
  3 : PreDefined := 'D';
  4 : PreDefined := 'G';
  5 : PreDefined := 'H';
  6 : PreDefined := 'E''';
 end;
end;

procedure TTunerDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FTuner.SampleRate := SampleRate;
end;

end.
