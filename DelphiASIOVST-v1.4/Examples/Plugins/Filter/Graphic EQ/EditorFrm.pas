unit EditorFrm;

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
  Forms, Controls, StdCtrls, ExtCtrls, DAV_Types, DAV_VSTModule;

type
  TEditorForm = class(TForm)
    Lb10k: TLabel;
    Lb1200: TLabel;
    Lb160: TLabel;
    Lb20: TLabel;
    Lb20k: TLabel;
    Lb2500: TLabel;
    Lb320: TLabel;
    Lb40: TLabel;
    Lb5k: TLabel;
    Lb640: TLabel;
    Lb80: TLabel;
    LbLM: TLabel;
    LbRS: TLabel;
    MiddleL: TShape;
    MiddleR: TShape;
    SB10kL: TScrollBar;
    SB10kR: TScrollBar;
    SB1200L: TScrollBar;
    SB1200R: TScrollBar;
    SB160L: TScrollBar;
    SB160R: TScrollBar;
    SB20kL: TScrollBar;
    SB20kR: TScrollBar;
    SB20L: TScrollBar;
    SB20R: TScrollBar;
    SB2500L: TScrollBar;
    SB2500R: TScrollBar;
    SB320L: TScrollBar;
    SB320R: TScrollBar;
    SB40L: TScrollBar;
    SB40R: TScrollBar;
    SB5kL: TScrollBar;
    SB5kR: TScrollBar;
    SB640L: TScrollBar;
    SB640R: TScrollBar;
    SB80L: TScrollBar;
    SB80R: TScrollBar;
    ShBackText: TShape;
    procedure SBChange(Sender: TObject);
    procedure LbLMClick(Sender: TObject);
    procedure LbRSClick(Sender: TObject);
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses 
  PluginDM;

procedure TEditorForm.LbLMClick(Sender: TObject);
begin
 if LbLM.Caption = 'L' then
  begin
   LbLM.Caption := 'M';
   LbRS.Caption := 'S';
   with TPluginDataModule(Owner)
    do OnProcess := VSTModuleProcessMS;
  end else
 if LbLM.Caption = 'M' then
  begin
   LbLM.Caption := 'L';
   LbRS.Caption := 'R';
   with TPluginDataModule(Owner)
    do OnProcess := VSTModuleProcessLR;
  end;
 with TPluginDataModule(Owner)
  do OnProcess32Replacing := OnProcess;
end;

procedure TEditorForm.LbRSClick(Sender: TObject);
begin
 if LbRS.Caption = 'R' then
  begin
   LbLM.Caption := 'M';
   LbRS.Caption := 'S';
   with TPluginDataModule(Owner)
    do OnProcess := VSTModuleProcessMS;
  end else
 if LbRS.Caption = 'S' then
  begin
   LbLM.Caption := 'L';
   LbRS.Caption := 'R';
   with TPluginDataModule(Owner)
    do OnProcess := VSTModuleProcessLR;
  end;
 with TPluginDataModule(Owner)
  do OnProcess32Replacing := OnProcess;
end;

procedure TEditorForm.SBChange(Sender: TObject);
begin
 with TPluginDataModule(Owner), (Sender As TScrollBar)
  do Parameter[Tag] := Position * 0.1;
end;

end.
