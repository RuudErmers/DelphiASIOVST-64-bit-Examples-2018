program SimpleSemEmbedAudioFile;

{$R 'DAV_AudioFileOscillator.res' 'DAV_AudioFileOscillator.rc'}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, DAV_DLLResources;

{$R *.res}

var
  SeModule  : TPEResourceModule;
  RS        : TResourceStream;
  RD        : TResourceDetails;
  AudioName : string;

function RemoveFileExt(Filename: TFileName): TFileName;
var
  DotPos : Integer;
begin
 result := Filename;
 DotPos := Pos('.', result);
 if DotPos > 0
  then result := Copy(result, 1, DotPos - 1);
 result := Uppercase(result); 
end;

begin
 with TOpenDialog.Create(nil) do
  try
   DefaultExt := '.WAV';
   Filter := 'WAV File (*.wav)|*.wav|AIFF File (*.aiff)|*.aif*|AU File (*.au)|*.au';
   Options := [ofHideReadOnly, {ofAllowMultiSelect, } ofFileMustExist, ofEnableSizing];
   if Execute then
    begin
     AudioName := FileName;
     with TSaveDialog.Create(nil) do
      try
       DefaultExt := '.SEM';
       Filter := 'SE Module (*.SEM)|*.SEM';
       FileName := ChangeFileExt(AudioName, '.SEM');
       if Execute then
        begin
         SeModule := TPEResourceModule.Create;

         RS := TResourceStream.Create(HInstance, 'AudioFileOscillator', 'SEM');
         try
          SeModule.LoadFromStream(RS);
         finally
          FreeAndNil(RS);
         end;

         try
          with TMemoryStream.Create do
           try
            LoadFromFile(AudioName);
            RD := TResourceDetails.CreateResourceDetails(SeModule, 0,
              RemoveFileExt(ExtractFilename(AudioName)), 'WAVETABLE', Size, Memory);
            SeModule.InsertResource(0, RD);
           finally
            Free;
           end;

          SeModule.SortResources;
          SeModule.SaveToFile(FileName);
         finally
          FreeAndNil(SeModule);
         end;
        end;
     finally
      Free;
     end;
    end;
  finally
   Free;
  end;
end.
