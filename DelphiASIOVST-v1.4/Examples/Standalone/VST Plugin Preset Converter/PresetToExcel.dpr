program PresetToExcel;

uses
  SysUtils,
  Classes,
  Dialogs,
  nExcel,   // you need to have the Native Excel 2 Suite installed to compile
  DAV_VSTHost;

resourcestring
  RCStrUnknownFileExtensi = 'Unknown file extension';

{$R *.res}

var
  VstDllFile    : TFileName;
  OutputFile    : TFileName;
  PresetFile    : TFileName;
  Prgs, Params  : Integer;
  VstHost       : TVstHost;
  FileExtention : string;

begin
 // check VST plugin filename
 VstDllFile := ParamStr(1);
 if not CheckValidVstPlugin(VstDllFile)
  then
   with TOpenDialog.Create(nil) do
    try
     DefaultExt := '.dll';
     Filter := 'VST Plugin (*.dll)|*.dll';
     Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
     Title := 'Load VST Plugin';
     if Execute
      then VstDllFile := Filename;
    finally
     Free;
    end;
 if not CheckValidVstPlugin(VstDllFile) then
  begin
   MessageDlg('The file ' + VstDllFile + ' is not a valid VST plugin!', mtError, [mbOK], 0);
   Exit;
  end;

 OutputFile := ParamStr(2);
 if OutputFile = '' then
  with TSaveDialog.Create(nil) do
   try
    DefaultExt := '.xls';
    Filter := 'Excel (*.xls)|*.xls|CSV (*.csv)|*.csv|RTF (*.rtf)|*.rtf|HTML (*.html)|*.html|Text (*.txt)|*.txt';
    FileName := ChangeFileExt(VstDllFile, '.xls');
    Title := 'Save as...';
    if Execute then OutputFile := FileName;
   finally
    Free;
   end;
 if OutputFile = '' then
  begin
   MessageDlg('No output file selected!', mtError, [mbOK], 0);
   Exit;
  end;

 PresetFile := ParamStr(3);

 VstHost := TVstHost.Create(nil);
 try
  with VstHost.VstPlugIns.Add do
   try
    LoadFromFile(VstDllFile);
    Active := True;
    if FileExists(PresetFile) then
     if lowercase(ExtractFileExt(PresetFile)) = '.fxp'
      then LoadPreset(PresetFile) else
     if lowercase(ExtractFileExt(PresetFile)) = '.fxp'
      then LoadPreset(PresetFile)
      else
       begin
        MessageDlg('The preset/bank ' + PresetFile + ' file does not seem ' +
          'to have the right extension!', mtWarning, [mbOK], 0);
       end;

    if numPrograms < numParams then
     begin
      with TXLSWorkbook.Create do
       try
        // add new sheet
        with Sheets.Add do
         begin
          Name := EffectName;

          // programs title
          with RCRange[1, 3, 1, 2 + numPrograms] do
           begin
            Merge(False);
            Font.Size := 14;
            Value := 'Programs';
            Borders.ColorIndex := xlColorIndexAutomatic;
            HorizontalAlignment := xlHAlignCenter;
            VerticalAlignment := xlVAlignCenter;
            Borders[xlAround].Weight := xlMedium;
           end;

          // parameters title
          with RCRange[3, 1, 2 + numParams, 1] do
           begin
            Merge(False);
            Font.Size := 14;
            Value := 'Parameters';
            Borders.ColorIndex := xlColorIndexAutomatic;
            HorizontalAlignment := xlHAlignCenter;
            VerticalAlignment := xlVAlignCenter;
            ColumnWidth := 3;
            Orientation := 90;
            Borders[xlAround].Weight := xlMedium;
           end;

          // format programs
          with RCRange[2, 3, 2, 2 + numPrograms] do
           begin
            Font.Size := 10;
            Font.Bold := True;
            Borders.ColorIndex := xlColorIndexAutomatic;
            Borders[xlAround].Weight := xlMedium;
           end;

          // format parameters
          with RCRange[3, 2, 2 + numParams, 2] do
           begin
            Font.Size := 10;
            Font.Bold := True;
            ColumnWidth := 12;
            Borders.ColorIndex := xlColorIndexAutomatic;
            Borders[xlAround].Weight := xlMedium;
           end;

          // format parameter names
          for Params := 0 to numParams - 1 do
           begin
            if ParameterLabel[Params] <> ''
             then Cells.Item[3 + Params, 2].Value :=
              ParameterName[Params] + ' [' + ParameterLabel[Params] + ']'
             else Cells.Item[3 + Params, 2].Value := ParameterName[Params];
           end;

          // format parameter values
          with RCRange[3, 3, 2 + numParams, 2 + numPrograms] do
           begin
            Borders.ColorIndex := xlColorIndexAutomatic;
            Borders[xlAround].Weight := xlMedium;
           end;

          // parameter values
          for Prgs := 0 to numPrograms - 1 do
           begin
            CurrentProgram := Prgs;
            Cells.Item[2, 3 + Prgs].Value := ProgramName;
            Cells.Item[2, 3 + Prgs].AutoFit(True);

            for Params := 0 to numParams - 1 do
             try
              Cells.Item[3 + Params, 3 + Prgs].Value := ParameterDisplay[Params];
             except
              Cells.Item[3 + Params, 3 + Prgs].Value := 'Error!';
             end;
           end;

          // autofit parameter values
          RCRange[2, 3, 2 + numParams, 2 + numPrograms].AutoFit(True);

          // autofit parameter names
          RCRange[3, 2, 2 + numParams, 2].AutoFit(True);
         end;
        FileExtention := LowerCase(ExtractFileExt(OutputFile));
        if FileExtention = '.xls' then SaveAs(OutputFile) else
        if FileExtention = '.csv' then SaveAs(OutputFile, xlCSV) else
        if FileExtention = '.rtf' then SaveAs(OutputFile, xlRTF) else
        if FileExtention = '.html' then SaveAs(OutputFile, xlHTML) else
        if FileExtention = '.txt' then SaveAs(OutputFile, xlText)
         else raise Exception.Create(RCStrUnknownFileExtensi);
       finally
        Free;
       end;
     end
    else
     begin
      with TXLSWorkbook.Create do
       try
        // add new sheet
        with Sheets.Add do
         begin
          Name := EffectName;

          // programs title
          with RCRange[3, 1, 2 + numPrograms, 1] do
           begin
            Merge(False);
            Font.Size := 14;
            Value := 'Programs';
            Borders.ColorIndex := xlColorIndexAutomatic;
            HorizontalAlignment := xlHAlignCenter;
            VerticalAlignment := xlVAlignCenter;
            ColumnWidth := 3;
            Orientation := 90;
            Borders[xlAround].Weight := xlMedium;
           end;

          // parameters title
          with RCRange[1, 3, 1, 2 + numParams] do
           begin
            Merge(False);
            Font.Size := 14;
            Value := 'Parameters';
            Borders.ColorIndex := xlColorIndexAutomatic;
            HorizontalAlignment := xlHAlignCenter;
            VerticalAlignment := xlVAlignCenter;
            Borders[xlAround].Weight := xlMedium;
           end;

          // format programs
          with RCRange[3, 2, 2 + numPrograms, 2] do
           begin
            Font.Size := 10;
            Font.Bold := True;
            Borders.ColorIndex := xlColorIndexAutomatic;
            Borders[xlAround].Weight := xlMedium;
           end;

          // format parameters
          with RCRange[2, 3, 2, 2 + numParams] do
           begin
            Font.Size := 10;
            Font.Bold := True;
            ColumnWidth := 12;
            Borders.ColorIndex := xlColorIndexAutomatic;
            Borders[xlAround].Weight := xlMedium;
           end;

          // format parameter names
          for Params := 0 to numParams - 1 do
           begin
            if ParameterLabel[Params] <> ''
             then Cells.Item[2, 3 + Params].Value := ParameterName[Params] + ' [' + ParameterLabel[Params] + ']'
             else Cells.Item[2, 3 + Params].Value := ParameterName[Params];
           end;

          // format parameter values
          with RCRange[3, 3, 2 + numPrograms, 2 + numParams] do
           begin
            Borders.ColorIndex := xlColorIndexAutomatic;
            Borders[xlAround].Weight := xlMedium;
           end;

          // parameter values
          for Prgs := 0 to numPrograms - 1 do
           begin
            CurrentProgram := Prgs;
            Cells.Item[3 + Prgs, 2].Value := ProgramName;
            Cells.Item[3 + Prgs, 2].AutoFit(True);

            for Params := 0 to numParams - 1 do
             try
              Cells.Item[3 + Prgs, 3 + Params].Value := ParameterDisplay[Params];
             except
              Cells.Item[3 + Prgs, 3 + Params].Value := 'Error!';
             end;
           end;

          // autofit parameter values
          RCRange[3, 2, 2 + numPrograms, 2 + numParams].AutoFit(True);

          // autofit parameter names
          RCRange[2, 3, 2, 2 + numParams].AutoFit(True);
         end;
        FileExtention := LowerCase(ExtractFileExt(OutputFile));
        if FileExtention = '.xls' then SaveAs(OutputFile) else
        if FileExtention = '.csv' then SaveAs(OutputFile, xlCSV) else
        if FileExtention = '.rtf' then SaveAs(OutputFile, xlRTF) else
        if FileExtention = '.html' then SaveAs(OutputFile, xlHTML) else
        if FileExtention = '.txt' then SaveAs(OutputFile, xlText)
         else raise Exception.Create(RCStrUnknownFileExtensi);
       finally
        Free;
       end;
     end;
    Active := False;
   finally
    VstHost.VstPlugIns.Clear;
   end;
 finally
  FreeAndNil(VstHost);
 end;
end.
