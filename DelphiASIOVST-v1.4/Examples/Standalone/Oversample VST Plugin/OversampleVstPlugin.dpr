program OversampleVstPlugin;

{$R 'OversampleTemplate.res' 'OversampleTemplate.rc'}

uses
  SysUtils, Classes, Dialogs, DAV_DLLResources;

{$R *.res}

procedure WrapVstPlugin(Source, Destination: TFileName);
var
  Template    : TPEResourceModule;
  RS          : TResourceStream;
  RD          : TResourceDetails;
begin
 Template := TPEResourceModule.Create;
 try
  RS := TResourceStream.Create(HInstance, 'Template', 'DLL');
  try
   Template.LoadFromStream(RS);
  finally
   FreeAndNil(RS);
  end;

  with TMemoryStream.Create do
   try
    LoadFromFile(Source);
    RD := TResourceDetails.CreateResourceDetails(Template, 0, 'DLL', 'DLL', Size, Memory);
    Template.AddResource(RD);
   finally
    Free;
   end;

  Template.SortResources;
  Template.SaveToFile(Destination);
 finally
  FreeAndNil(Template);
 end;
end;

var
  Source      : TFileName;
  Destination : TFileName;
  SR          : TSearchRec;

begin
 Source := ParamStr(1);

 if (Source <> '') and (Pos('*', Source) >= 0) then
  begin
   if FindFirst(Source, faAnyFile, SR) = 0 then
    try
     repeat
      WrapVstPlugin(SR.Name, ChangeFileExt(SR.Name, '') + '²' + '.dll');
     until FindNext(SR) <> 0;
    finally
     FindClose(SR);
    end;

   Exit;
  end;

 if not FileExists(Source) then
  with TOpenDialog.Create(nil) do
   try
    DefaultExt := '.DLL';
    Filter := 'VST Plugin (*.dll)|*.dll';
    Options := [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing];
    if Execute
     then Source := FileName
     else Exit;
   finally
    Free;
   end;

 if not FileExists(Source) then Exit;

 Destination := ParamStr(1);
 if Destination = '' then
  with TSaveDialog.Create(nil) do
   try
    DefaultExt := '.DLL';
    Filter := 'VST Plugin (*.dll)|*.dll';
    if Execute
     then Destination := FileName
     else Exit;
   finally
    Free;
   end;

 if Destination = '' then Exit;

 WrapVstPlugin(Source, Destination);

end.
