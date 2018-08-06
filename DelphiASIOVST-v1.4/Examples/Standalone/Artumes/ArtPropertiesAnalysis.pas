unit ArtPropertiesAnalysis;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

{$I Artumes.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, 
  ArtItemAnalysis, ArtFrameAnalysisThirdOctave;

type
  TFmAnalysisProperties = class(TForm)
    PnProperties: TPanel;
    BtOK: TButton;
    BtCancel: TButton;
    BtApply: TButton;
    procedure BtOKClick(Sender: TObject);
    procedure BtCancelClick(Sender: TObject);
  private
    FAnalysis : TCustomAnalysis;
    FFrame    : TFrame;
    procedure SetAnalysis(const Value: TCustomAnalysis);
  protected
    procedure AnalysisChanged; virtual;
  public
    property Analysis: TCustomAnalysis read FAnalysis write SetAnalysis;
  end;

var
  FmAnalysisProperties: TFmAnalysisProperties;

implementation

{$R *.dfm}

{ TFmAnalysisProperties }

procedure TFmAnalysisProperties.AnalysisChanged;
begin
 if Assigned(FFrame)
  then FreeAndNil(FFrame);

 if FAnalysis is TCustomAnalysisFractionalOctave then
  begin
   FFrame := TFrAnalysisThirdOctave.Create(PnProperties);
   FFrame.Align := alClient;
   FFrame.Parent := PnProperties;

   with TCustomAnalysisFractionalOctave(FAnalysis), TFrAnalysisThirdOctave(FFrame) do
    begin
     RbFilter.Checked := BandSeparation = bsFilter;
     RbFFT.Checked := BandSeparation = bsFFT;
    end;
  end;
end;

procedure TFmAnalysisProperties.BtCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAnalysisProperties.BtOKClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAnalysisProperties.SetAnalysis(const Value: TCustomAnalysis);
begin
 if FAnalysis <> Value then
  begin
   FAnalysis := Value;
   AnalysisChanged;
  end;
end;

end.
