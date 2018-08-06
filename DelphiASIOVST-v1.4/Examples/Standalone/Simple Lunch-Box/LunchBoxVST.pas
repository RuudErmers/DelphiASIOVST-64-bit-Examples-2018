unit LunchBoxVST;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LMessages, LResources, Buttons,
  {$ELSE} Windows, Messages,{$ENDIF} SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TFmVST = class(TForm)
    BtOutputEditor: TButton;
    BtOutputVST: TButton;
    BtRealtimeEditor: TButton;
    BtRealtimeVST: TButton;
    EdOutputVST: TEdit;
    EdRealtimeVST: TEdit;
    GbOutputVST: TGroupBox;
    GbRealtimeVST: TGroupBox;
    LbRealtimeVST: TLabel;
    LbOutputVST: TLabel;
    procedure EdRealtimeVSTChange(Sender: TObject);
    procedure BtOutputEditorClick(Sender: TObject);
    procedure BtRealtimeEditorClick(Sender: TObject);
    procedure EdOutputVSTChange(Sender: TObject);
    procedure BtRealtimeVSTClick(Sender: TObject);
    procedure BtOutputVSTClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmVST: TFmVST;

implementation

uses
  LunchBoxMain;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmVST.BtOutputVSTClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select a VST Plugin';
   if Execute then
    begin
     EdOutputVST.Text := FileName;
    end;
  finally
   Free;
  end;
end;

procedure TFmVST.BtRealtimeVSTClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select a VST Plugin';
   if Execute then
    begin
     EdRealtimeVST.Text := FileName;
    end;
  finally
   Free;
  end;
end;

procedure TFmVST.BtRealtimeEditorClick(Sender: TObject);
begin
 if FmLunchBox.VSTHost[0].Active
  then FmLunchBox.VSTHost[0].ShowEdit;
end;

procedure TFmVST.BtOutputEditorClick(Sender: TObject);
begin
 if FmLunchBox.VSTHost[1].Active
  then FmLunchBox.VSTHost[1].ShowEdit;
end;

procedure TFmVST.EdRealtimeVSTChange(Sender: TObject);
begin
 with FmLunchBox.VSTHost[0] do
  begin
   Active := False;
   if FileExists(EdRealtimeVST.Text) then
    begin
     DLLFileName := EdRealtimeVST.Text;
     Active := True;
    end;
   BtRealtimeEditor.Enabled := Active;
  end;
end;

procedure TFmVST.EdOutputVSTChange(Sender: TObject);
begin
 with FmLunchBox.VSTHost[1] do
  begin
   Active := False;
   if FileExists(EdOutputVST.Text) then
    begin
     DLLFileName := EdOutputVST.Text;
     Active := True;
    end;
   BtOutputEditor.Enabled := Active;
  end;
end;

end.
