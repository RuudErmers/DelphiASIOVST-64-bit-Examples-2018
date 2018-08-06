unit CafMain;

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

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, StdCtrls, Spin, ExtCtrls, 
  DAV_Types, DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF, 
  DAV_AudioFileAU, DAV_ChannelDataCoder;

type
  TFmConvertAudioFile = class(TForm)
    CbEncoding: TComboBox;
    EdBitsPerSample: TEdit;
    EdChannel: TEdit;
    EdEncoding: TEdit;
    EdSampleFrames: TEdit;
    EdSampleRate: TEdit;
    EdTotalTime: TEdit;
    LbBitsPerSample: TLabel;
    LbChannel: TLabel;
    LbEncoding: TLabel;
    LbNew: TLabel;
    LbNoFileLoaded: TLabel;
    LbOld: TLabel;
    LbSampleFrames: TLabel;
    LbSampleRate: TLabel;
    LbTotalTime: TLabel;
    MainMenu: TMainMenu;
    MiExit: TMenuItem;
    MiFile: TMenuItem;
    MiOpen: TMenuItem;
    MiSave: TMenuItem;
    MiSaveAs: TMenuItem;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;
    PnAudioDetails: TPanel;
    SaveDialog: TSaveDialog;
    SEBitsPerSample: TSpinEdit;
    SeSampleRate: TSpinEdit;
    CbFloatBits: TComboBox;
    procedure MiExitClick(Sender: TObject);
    procedure MiOpenClick(Sender: TObject);
    procedure MiSaveAsClick(Sender: TObject);
    procedure CbEncodingChange(Sender: TObject);
  private
    FAudioFile : TCustomAudioFile;
    FFileName  : TFileName;
    FTempData  : TDAVArrayOfSingleDynArray;
  protected
    procedure DecodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure EncodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
  public
    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);
  end;

var
  FmConvertAudioFile: TFmConvertAudioFile;

implementation

resourcestring
  RCStrDataOffsetNotFound = 'Data offset couldn''t be found';

{$R *.dfm}

procedure TFmConvertAudioFile.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmConvertAudioFile.MiOpenClick(Sender: TObject);
begin
 if OpenDialog.Execute
  then LoadFromFile(OpenDialog.FileName);
end;

procedure TFmConvertAudioFile.MiSaveAsClick(Sender: TObject);
begin
 if SaveDialog.Execute
  then SaveToFile(SaveDialog.FileName);
end;

procedure TFmConvertAudioFile.CbEncodingChange(Sender: TObject);
begin
 SEBitsPerSample.Visible := CbEncoding.ItemIndex = 0;
 CbFloatBits.Visible := CbEncoding.ItemIndex = 1;
end;

procedure TFmConvertAudioFile.DecodeHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel : Integer;
begin
 if Coder is TCustomChannel32DataCoder then
  begin
   SetLength(FTempData, Coder.ChannelCount);
   for Channel := 0 to Coder.ChannelCount - 1 do
    begin
     SetLength(FTempData[Channel], Coder.SampleFrames);
     Move(TCustomChannel32DataCoder(Coder).ChannelPointer[Channel]^[0],
       FTempData[Channel, 0], Coder.SampleFrames * SizeOf(Single));
    end;
  end;
end;

procedure TFmConvertAudioFile.EncodeHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel : Integer;
begin
 if Coder is TCustomChannel32DataCoder then
  begin
   assert(Length(FTempData) = Coder.ChannelCount);
   for Channel := 0 to Coder.ChannelCount - 1 do
    begin
     assert(Length(FTempData[Channel]) = Coder.SampleFrames);
     Move(FTempData[Channel, 0], TCustomChannel32DataCoder(Coder).ChannelPointer[Channel]^[0], Coder.SampleFrames * SizeOf(Single));
    end;
  end;
end;

procedure TFmConvertAudioFile.LoadFromFile(FileName: TFileName);
var
  AudioFileClass : TAudioFileClass;
begin
 AudioFileClass := FileNameToFormat(OpenDialog.FileName);
 if AudioFileClass <> nil then
  begin
   // check if a previous file has been loaded
   if assigned(FAudioFile)
    then FreeAndNil(FAudioFile);

   // create and load audio file from stream (only headers)
   FAudioFile := AudioFileClass.Create(FileName);

  (*
   // seek data offset
   if FAudioFile.DataOffset > 0
    then FFileStream.Seek(FAudioFile.DataOffset, soBeginning)
    else raise Exception.Create(RCStrDataOffsetNotFound);
  *)

   EdChannel.Text := IntToStr(FAudioFile.ChannelCount);
   EdSampleFrames.Text := IntToStr(FAudioFile.SampleFrames);
   EdTotalTime.Text := FloatToStrF(FAudioFile.TotalTime, ffGeneral, 5, 5) + ' s';
   EdSampleRate.Text := FloatToStr(FAudioFile.SampleRate) + ' Hz';
   SeSampleRate.Value := round(FAudioFile.SampleRate);
   if FAudioFile is TAudioFileWAV then
    with TAudioFileWAV(FAudioFile) do
     begin
      EdBitsPerSample.Text := IntToStr(BitsPerSample);
      SEBitsPerSample.Value := BitsPerSample;
      case Encoding of
        aeInteger : EdEncoding.Text := 'Integer';
          aeFloat : EdEncoding.Text := 'Float';
            aeMP3 : EdEncoding.Text := 'MP3';
            aeACM : EdEncoding.Text := 'ACM';
          aeADPCM : EdEncoding.Text := 'ADPCM';
        aeMSADPCM : EdEncoding.Text := 'MS ADPCM';
       aeDVIADPCM : EdEncoding.Text := 'DVI ADPCM';
          aeMuLaw : EdEncoding.Text := 'µ-Law';
           aeALaw : EdEncoding.Text := 'A-Law';
      end;
      if Integer(Encoding) < CbEncoding.Items.Count
       then CbEncoding.ItemIndex := Integer(Encoding);

      FFileName := OpenDialog.FileName;
      MiSaveAs.Enabled := FFileName <> '';
      MiSave.Enabled := FFileName <> '';
      PnAudioDetails.Visible := True;
     end;
  end;
end;

procedure TFmConvertAudioFile.SaveToFile(FileName: TFileName);
var
  NewAudioFile : TCustomAudioFile;
  AudioFormat  : TAudioFileClass;
  Sample       : Integer;
const
  CBlockSize = 2048;
begin
 if FFileName = FileName
  then raise Exception.Create('not yet supported');

 AudioFormat := FileNameToFormat(FileName);
 if AudioFormat = nil
  then raise Exception.Create('no valid file specified');

 NewAudioFile := AudioFormat.Create(FileName);
 try
  if NewAudioFile is TAudioFileWAV then
   with TAudioFileWAV(NewAudioFile) do
    begin
     SampleRate := SeSampleRate.Value;
     case CbEncoding.ItemIndex of
      0 : BitsPerSample := SEBitsPerSample.Value;
      1 : case CbFloatBits.ItemIndex of
           0 : BitsPerSample := 16;
           1 : BitsPerSample := 32;
           2 : BitsPerSample := 64;
          end;
     end;
    end;

  if NewAudioFile is TAudioFileAIFF then
   with TAudioFileAIFF(NewAudioFile) do
    begin
     SampleRate := SeSampleRate.Value;
     case CbEncoding.ItemIndex of
      0 : BitsPerSample := SEBitsPerSample.Value;
      1 : case CbFloatBits.ItemIndex of
           0 : BitsPerSample := 16;
           1 : BitsPerSample := 32;
           2 : BitsPerSample := 64;
          end;
     end;
    end;

  FAudioFile.OnDecode := DecodeHandler;
  NewAudioFile.OnEncode := EncodeHandler;
  try
   Sample := 0;
   while Sample < FAudioFile.SampleFrames do
    begin
     FAudioFile.Decode(Sample, CBlockSize);
     NewAudioFile.Encode(Sample, CBlockSize);
     Inc(Sample, CBlockSize);
    end;
   if FAudioFile.SampleFrames - Sample > 0 then
    begin
     FAudioFile.Decode(Sample, FAudioFile.SampleFrames - Sample);
     NewAudioFile.Encode(Sample, FAudioFile.SampleFrames - Sample);
    end;
  finally
   FAudioFile.OnDecode := nil;
  end;
 finally
  FreeAndNil(NewAudioFile);
 end;

 FFileName := SaveDialog.FileName;
 MiSave.Enabled := FFileName <> '';
end;

end.
