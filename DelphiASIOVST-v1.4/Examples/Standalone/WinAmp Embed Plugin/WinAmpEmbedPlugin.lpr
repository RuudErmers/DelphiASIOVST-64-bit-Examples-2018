program WinAmpEmbedPlugin;

{$I DAV_Compiler.inc}

{$R 'WinAmp.res' 'WinAmp.rc'}

uses
  Interfaces, {$IFDEF FPC} LCLIntf, {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_DLLResources;

var
  WinAmp  : TPEResourceModule;
  RS      : TResourceStream;
  RD      : TResourceDetails;
  DLLName : string;

begin
    Application.Title:='WinAmp Embed Plugin';
  with TOpenDialog.Create(nil) do
   begin
    Name := 'OpenDialogVST';
    DefaultExt := '.DLL';
    Filter := 'VST Plugin (*.dll)|*.dll';
    Options := [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing];
    if Execute then
     begin
      DLLName := FileName;
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
