program StandaloneEmbedPlugin;

{$R 'Standalone.res' 'Standalone.rc'}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, DAV_DLLResources;

{$R *.res}

var
  Standalone : TPEResourceModule;
  RS         : TResourceStream;
  RD         : TResourceDetails;
  DLLName    : string;

begin
 with TOpenDialog.Create(nil) do
  try
   DefaultExt := '.DLL';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing];
   if Execute then
    begin
     DLLName := FileName;
     with TSaveDialog.Create(nil) do
      try
       DefaultExt := '.EXE';
       Filter := 'Standalone (*.exe)|*.exe';
       if Execute then
        begin
         Standalone := TPEResourceModule.Create;

         RS := TResourceStream.Create(HInstance, 'Standalone', 'EXE');
         try
          Standalone.LoadFromStream(RS);
         finally
          FreeAndNil(RS);
         end;

         try
          with TMemoryStream.Create do
           try
            LoadFromFile(DLLName);
            RD := TResourceDetails.CreateResourceDetails(Standalone, 0, 'DLL', 'DLL', Size, Memory);
            Standalone.AddResource(RD);
           finally
            Free;
           end;

          Standalone.SortResources;
          Standalone.SaveToFile(FileName);
         finally
          FreeAndNil(Standalone);
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
