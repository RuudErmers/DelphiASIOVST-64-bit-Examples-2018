program VstPluginScreenshotTool;

{$I DAV_Compiler.inc}

{$APPTYPE CONSOLE}

uses
{$IFNDEF FPC}
  PngImage, DAV_GuiPng,
{$ELSE}
  Interfaces, LazPng, imagesforlazarus,
{$ENDIF}
  Windows, Messages, Classes, Controls, Forms, Graphics, SysUtils, FileCtrl,
  DAV_VstEffect, DAV_VstHost, DAV_GuiPixelMap;

resourcestring
  RCStrProductString = 'Vst Plugin Screenshot Tool';
  RCStrVendorString = 'Delphi ASIO & VST Project';
  RCStrCapturing = 'Capturing';
  RCStrWrongSyntax = 'Wrong syntax!';
  RCStrAbout = 'Delphi ASIO & VST Project - Vst Plugin Screenshot Tool';
  RCStrPluginNotActive = 'Plugin not active!';
  RCStrPluginNoEditor = 'Plugin does not feature an editor!';
  RCStrWrongParameters = 'Add parameter or move this tool into a directory containing VST plugins';
  RCStrSelectDirectory = 'Select Directory';

{-$DEFINE Alternative}

procedure RenderScreenshot(FileName: TFileName; ParameterFileName: TFileName = '');
var
  Form      : TForm;
  Bitmap    : TBitmap;
  Png       : TPNGImage;
  Rct       : TRect;
begin
 with TVstHost.Create(nil) do
  try
   ProductString := AnsiString(RCStrProductString);
   VendorString := AnsiString(RCStrVendorString);

   with VstPlugIns.Add do
    try
     // check VST plugin is a valid plugin
     if not CheckValidPlugin(FileName) then exit;

     Writeln(RCStrCapturing + ': ' + FileName);

     // load from file
     LoadFromFile(FileName);

     // activate VST plugin
     Active := True;
     if not Active
      then raise Exception.Create(RCStrPluginNotActive);

     if not (effFlagsHasEditor in VstEffectPointer.EffectFlags)
      then raise Exception.Create(RCStrPluginNoEditor);

     // create form for GUI rendering
     Form := TForm.CreateNew(Application);
     try
      if FileExists(ParameterFileName) and (ExtractFileExt(ParameterFileName) = '.fxp')
       then LoadPreset(ParameterFileName) else
      if FileExists(FileName + '.fxp')
       then LoadPreset(FileName + '.fxp');

      if ParamStr(3) <> '' then
       begin
        // add further parameters here (stream a sound file, etc...)
       end;

      ShowEdit(Form);

      Rct := GetRect;
      Form.BorderStyle := bsNone;
      Form.ClientWidth := Rct.Right - Rct.Left;
      Form.ClientHeight := Rct.Bottom - Rct.Top;
      Form.Left := -Form.ClientWidth;
      Form.Visible := True;
      Application.ProcessMessages;
      Bitmap := TBitmap.Create;
      try
       RenderEditorToBitmap(Bitmap);
       Png := TPNGImage.Create;
       with Png do
        try
         Png.Assign(Bitmap);
         if ParameterFileName = ''
          then ParameterFileName := ChangeFileExt(FileName, '.png');
         if ExtractFileExt(ParameterFileName) = '.png'
          then Png.SaveToFile(ParameterFileName)
          else
         if ExtractFileExt(ParameterFileName) <> '.fxp'
          then WriteLn('Wrong file extension, only .png is allowed!');
        finally
         FreeAndNil(Png);
        end;
      finally
       FreeAndNil(Bitmap);
      end;
     finally
      CloseEdit;
      FreeAndNil(Form);
     end;
    except
     on E:Exception do Writeln('Error: ' + E.Message);
    end;
  finally
   Free;
  end;
end;

var
  Dir : string;
  SR  : TSearchRec;

{$R *.res}

begin
 Writeln(RCStrAbout);

 if FileExists(ParamStr(1))
  then RenderScreenshot(ParamStr(1), ParamStr(2))
  else
   if FindFirst('*.dll', faAnyFile, SR) = 0 then
    try
     repeat
      RenderScreenshot(SR.Name);
     until FindNext(SR) <> 0;
    finally
     // Must free up resources used by these successful finds
     FindClose(SR);
    end
   else
    begin
     Writeln(RCStrWrongSyntax);
     Writeln(RCStrWrongParameters);

     Dir := ExtractFileDir(ParamStr(0));
     SelectDirectory(RCStrSelectDirectory, '', Dir);
     if FindFirst(Dir + '\' + '*.dll', faAnyFile, SR) = 0 then
      try
       repeat
        RenderScreenshot(Dir + '\' + SR.Name);
       until FindNext(SR) <> 0;
      finally
       // Must free up resources used by these successful finds
       FindClose(SR);
      end
    end;
end.
