program SemScreenshotTool;

{$APPTYPE CONSOLE}

uses
  Windows,
  Classes,
  Controls,
  Forms,
  Graphics,
  SysUtils,
  FileCtrl,
  PngImage,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEHost;

procedure RenderScreenshot(FileName: TFileName);
var
  PartNr  : Integer;
  PinNr   : Integer;
  Pin     : TSEPinProperties;
  Pins    : array of TSEPinProperties;
  FullBmp : TBitmap;
  Bitmap  : TBitmap;
  Png     : TPNGObject;
  TpPos   : Integer;
begin
 with TSEHost.Create(nil) do
  try
   with HostedSEModules.Add do
    try
     // check VST plugin is a valid plugin
//     if not CheckValidPlugin(FileName) then exit;

     Writeln('Loading: ' + FileName);

     // load from file
     LoadFromFile(FileName);

     // create form for GUI rendering
     FullBmp := TBitmap.Create;
     with FullBmp, Canvas do
      try
       for PartNr := 0 to PartCount - 1 do
        with Part[PartNr] do
         begin
          Writeln('Capturing: ' + Name);

          Instantiation;
  //        Open;
          try

           // create form for GUI rendering
           Bitmap := TBitmap.Create;
           with Bitmap, Canvas do
            try
             Canvas.Font.Name := 'Arial';
             Canvas.Font.Color := clWhite;
             Canvas.Font.Size := 9;
             Canvas.Brush.Color := $6A240A;
             Canvas.Pen.Color := $6A240A;

             Width := 4 + TextWidth(Properties.Name);
             Height := TextHeight(Properties.Name) + 4;

             FillRect(ClipRect);

             // calculate width and enumerate modules
             Canvas.Font.Size := 7;
             SetLength(Pins, 0);
             while GetPinProperties(Length(Pins), Pin) = True do
              begin
               SetLength(Pins, Length(Pins) + 1);
               Pins[Length(Pins) - 1] := Pin;
               if TextWidth(Pin.Name) + 4 > Width
                then Width := TextWidth(Pin.Name) + 4;

               Height := Height + TextHeight(Pin.Name) + 2;
              end;

             if PartNr * 20 + Width > FullBmp.Width then FullBmp.Width := PartNr * 20 + Width;
             if PartNr * 20 + Height > FullBmp.Height then FullBmp.Height := PartNr * 20 + Height;

             // render caption
             Canvas.Font.Size := 9;
             TextOut((Width - TextWidth(Properties.Name)) div 2, 1, Properties.Name);

             Canvas.Brush.Color := $C8D0D4;
             FillRect(Rect(0, TextHeight(Properties.Name) + 2, Width, Height));
             Canvas.Font.Size := 7;

             // render pins
             for PinNr := 0 to Length(Pins) - 1 do
              begin
               case Pins[PinNr].Datatype of
                dtFSample : Canvas.Font.Color := $960032;
                dtSingle  : Canvas.Font.Color := $8c5000;
                dtInteger : Canvas.Font.Color := clOlive;
                dtEnum    : Canvas.Font.Color := clGreen;
                else Canvas.Font.Color := clBlack;
               end;

               TpPos := (PinNr + 1) * (TextHeight(Properties.Name) + 2) + 4;
               Canvas.Pen.Color := Canvas.Font.Color;

               case Pins[PinNr].Direction of
                drIn : begin
                        MoveTo(0, TpPos + 5);
                        LineTo(3, TpPos + 5);
                        TextOut(4, TpPos, Pins[PinNr].Name);
                       end;
                drParameter : TextOut((Width - TextWidth(Pins[PinNr].Name)) div 2, TpPos, '(' + Pins[PinNr].Name + ')');
                drOut : begin
                         TextOut(Width - 4 - TextWidth(Pins[PinNr].Name), TpPos, Pins[PinNr].Name);
                         MoveTo(Width - 3, TpPos + 5);
                         LineTo(Width, TpPos + 5);
                        end;
               end;
              end;

             // render border
             Pen.Color := $808080;
             MoveTo(Width - 1, 0);
             LineTo(Width - 1, Height - 1);
             LineTo(0, Height - 1);
             Pen.Color := $B4B4B4;
             LineTo(0, 0);
             LineTo(Width - 1, 0);

             // copy current module to complete bmp
             FullBmp.Canvas.Draw(PartNr * 20, PartNr * 20, Bitmap);

             // copy to png
             Png := TPNGObject.Create;
             with Png do
              try
               Png.Assign(Bitmap);
               Png.SaveToFile(FileName + ' - Module ' + IntToStr(PartNr + 1) + '.png');
              finally
               FreeAndNil(Png);
              end;
            finally
             FreeAndNil(Bitmap);
            end;
          finally
//         Close;
          end;
         end;

       // copy to png
       Png := TPNGObject.Create;
       with Png do
        try
         Png.Assign(FullBmp);
         Png.SaveToFile(FileName + '.png');
        finally
         FreeAndNil(Png);
        end;
      finally
       FreeAndNil(FullBmp);
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
begin
 Writeln('Delphi ASIO & VST Project - SEM Screenshot Tool');

 if FileExists(ParamStr(1))
  then RenderScreenshot(ParamStr(1))
  else
   if FindFirst('*.SEM', faAnyFile, SR) = 0 then
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
     Writeln('Wrong syntax!');
     Writeln('Add parameter or move this tool into a directory that contains SE modules');

     Dir := ExtractFileDir(ParamStr(0));
     SelectDirectory('Select Directory', '', Dir);
     if FindFirst(Dir + '\' + '*.SEM', faAnyFile, SR) = 0 then
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
