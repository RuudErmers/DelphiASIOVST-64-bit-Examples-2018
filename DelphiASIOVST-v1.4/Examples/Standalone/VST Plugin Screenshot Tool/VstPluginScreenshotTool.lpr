program VstPluginScreenshotTool;

{$I DAV_Compiler.inc}

{$APPTYPE CONSOLE}

uses
{$IFNDEF FPC}
  PngImage, 
{$ELSE}
  LazPng, imagesforlazarus,
{$ENDIF}
  Windows, Classes, Controls, Forms, Graphics, SysUtils, FileCtrl, Interfaces,
  DAV_VstHost;

resourcestring
  RCStrProductString = 'Vst Plugin Screenshot Tool';
  RCStrVendorString = 'Delphi ASIO & VST Project';
  RCStrCapturing = 'Capturing';
  RCStrWrongSyntax = 'Wrong syntax!';
  RCStrAbout = 'Delphi ASIO & VST Project - Vst Plugin Screenshot Tool';

procedure RenderScreenshot(FileName: TFileName);
var
  Form    : TForm;
  Bitmap  : TBitmap;
  {$IFNDEF FPC}
  Png     : TPNGObject;
  {$ELSE}
  Png     : TPNGImage;
  {$ENDIF}
  Rct     : TRect;
begin
 with TVstHost.Create(nil) do
  try
   ProductString := RCStrProductString;
   VendorString := RCStrVendorString;

   with VstPlugIns.Add do
    try
     // check VST plugin is a valid plugin
     if not CheckValidPlugin(FileName) then exit;

     Writeln(RCStrCapturing + ': ' + FileName);

     // load from file
     LoadFromFile(FileName);

     // activate VST plugin
     Active := True;

     // create form for GUI rendering
     Form := TForm.Create(nil);
     try
      if FileExists(ParamStr(2))
       then LoadPreset(ParamStr(2)) else
      if FileExists(FileName + '.fxp')
       then LoadPreset(FileName + '.fxp');
      Form.BorderStyle := bsNone;
      Form.Position := poScreenCenter;

      if ParamStr(3) <> '' then
       begin
        // add further parameters here (stream a sound file, etc...)
       end;

      ShowEdit(Form);
      Rct := GetRect;
      Form.ClientWidth := Rct.Right - Rct.Left;
      Form.ClientHeight := Rct.Bottom - Rct.Top;
      Form.Visible := True;
//      Form.Repaint;
      Application.ProcessMessages;
      Bitmap := TBitmap.Create;
      try
       RenderEditorToBitmap(Bitmap);
       {$IFNDEF FPC}
       Png := TPNGObject.Create;
       {$ELSE}
       Png := TPNGImage.Create;
       {$ENDIF}
       with Png do
        try
         Png.Assign(Bitmap);
         Png.SaveToFile(FileName + '.png');
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
  then RenderScreenshot(ParamStr(1))
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
     Writeln('Add parameter or move this tool into a directory containing VST plugins');

     Dir := ExtractFileDir(ParamStr(0));
     SelectDirectory('Select Directory', '', Dir);
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
