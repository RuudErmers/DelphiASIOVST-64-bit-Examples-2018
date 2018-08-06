program WinAmpEmbedPlugin;

{$R 'WinAmp.res' 'WinAmp.rc'}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_DLLResources;

{$R *.res}

var
  WinAmp  : TPEResourceModule;
  RS      : TResourceStream;
  RD      : TResourceDetails;
  DLLName : string;

begin
  with TOpenDialog.Create(nil) do
   begin
    Name := 'OpenDialogVST';
    DefaultExt := '.DLL';
    Filter := 'VST Plugin (*.dll)|*.dll';
    Options := [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing];
    if Execute then
     begin
      DLLName := FileName;                                ..\..\..\UnitCache\$(Platform)
      with TSaveDialog.Create(nil) do
       try
        DefaultExt := '.DLL';
        Filter := 'WinAmp DSP Plugin (*.dll)|*.dll';
        if Execute then
         begin
          WinAmp := TPEResourceModule.Create;

          RS := TResourceStream.Create(HInstance, 'WinAmpVstHost', 'DLL');
          try
           WinAmp.LoadFromStream(RS);
          finally
           FreeAndNil(RS);
          end;

          try
           with TMemoryStream.Create do
            try
             LoadFromFile(DLLName);
             DLLName := ExtractFileName(DLLName);
             if Pos('.', DLLName) > 0
              then DLLName := Copy(DLLName, 1, Pos('.', DLLName) - 1);
             RD := TResourceDetails.CreateResourceDetails(WinAmp, 0, 'DLL', 'DLL', Size, Memory);
             WinAmp.AddResource(RD);
            finally
             Free;
            end;

           WinAmp.SortResources;
           WinAmp.SaveToFile(FileName);
         finally
          FreeAndNil(WinAmp);
         end;
        end;
      finally

      end;
     end;
   end;
end.
