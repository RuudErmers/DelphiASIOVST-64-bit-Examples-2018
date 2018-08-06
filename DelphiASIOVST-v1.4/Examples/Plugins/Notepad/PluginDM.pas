unit PluginDM;

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

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, DAV_ChunkClasses, DAV_Types, DAV_VSTModule;

type
  TTextChunk = class(TCustomTextChunk)
  public
    class function GetClassChunkName : TChunkName; override;
  end;

  TPluginDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleAfterProgramChange(Sender: TObject);
    procedure VSTModuleProcess32Replacing(const Inputs,
      Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
  private
    FChunk : TTextChunk;
    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
  public
    property Text: AnsiString read GetText write SetText;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Editor;


{ TTextChunk }

class function TTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'text';
end;


{ TPluginDataModule }

procedure TPluginDataModule.VSTModuleOpen(Sender: TObject);
begin
 // set editor form class
 EditorFormClass := TFmNotepad;

 // create custom chunk
 FChunk := TTextChunk.Create;
end;

procedure TPluginDataModule.VSTModuleClose(Sender: TObject);
begin
 // free custom chunk
 FreeAndNil(FChunk);
end;

function TPluginDataModule.GetText: AnsiString;
begin
  with Programs[CurrentProgram] do
  begin
   // locate the beginning of the chunk
   Chunk.Seek(0, soFromBeginning);

   // check if chunk is valid
   if Chunk.Size > 8
    then FChunk.LoadFromStream(Chunk)
    else FChunk.Text := '';

   // return text
   Result := string(FChunk.Text);
  end;
end;

procedure TPluginDataModule.SetText(const Value: AnsiString);
begin
  with Programs[CurrentProgram] do
  begin
   // locate the beginning of the chunk
   Chunk.Seek(0, soFromBeginning);

   // assign text
   FChunk.Text := Value;

   // save text to stream
   FChunk.SaveToStream(Chunk)
  end;
end;

procedure TPluginDataModule.VSTModuleAfterProgramChange(Sender: TObject);
begin
 // change notepad display
 if EditorForm is TFmNotepad then
  with TFmNotepad(EditorForm)
   do MeNotepad.Lines.Text := Text;
end;

procedure TPluginDataModule.VSTModuleProcess32Replacing(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
  //
end;

end.
