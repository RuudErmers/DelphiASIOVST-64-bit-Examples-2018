unit PluginGUI;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Graphics, Menus, Controls, Dialogs, ExtDlgs, Registry, DAV_Types, 
  DAV_VSTModule, DAV_GuiPixelMap, DAV_GuiFileFormats, DAV_GuiPng, DAV_GuiLabel,
  DAV_GuiStitchedControls, DAV_GuiStitchedDial;

type
  TExtraData = class
  private
    FFileName: TFileName;
  public
    constructor Create(Filename: TFileName); virtual;
    property Filename: TFileName read FFileName write FFileName;
  end;

  TFmCustomGui = class(TForm)
    MiAddBitmap: TMenuItem;
    MiAddDial: TMenuItem;
    MiAddText: TMenuItem;
    MiDesignMode: TMenuItem;
    MiLoad: TMenuItem;
    MiParameter: TMenuItem;
    MiProperties: TMenuItem;
    MiSetBackground: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    PopupMenu: TPopupMenu;
    OpenPictureDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MiAddBitmapClick(Sender: TObject);
    procedure MiAddDialClick(Sender: TObject);
    procedure MiAddTextClick(Sender: TObject);
    procedure MiDesignModeClick(Sender: TObject);
    procedure MiLoadClick(Sender: TObject);
    procedure MiSetBackgroundClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FPopupPos   : TPoint;
    FBackground : TGuiCustomPixelMap;
    function CreateRegistry: TRegistry;
    procedure RenderBrushedMetalBackground;
    procedure SetBackgRound(Filename: TFilename);
    procedure AssignParameterClick(Sender: TObject);
  protected
    procedure Clear;
    procedure RemoveAllCustomControls;
  public
    procedure LoadFromINI(FileName: TFileName);
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    procedure ParameterUpdate(ParameterIndex: Integer);
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Inifiles, DAV_GuiCommon, DAV_GuiBlend, DAV_VSTModuleWithPrograms,
  DAV_VSTParameters;

const
  CRegistryBaseKey = 'SOFTWARE\Delphi ASIO & VST Project\GR32\';

{ TExtraData }

constructor TExtraData.Create(Filename: TFileName);
begin
 FFileName := Filename;
end;


{ TFmCustomGr32Gui }

procedure TFmCustomGui.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
 LoadFromRegistry;
end;

procedure TFmCustomGui.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmCustomGui.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmCustomGui.FormResize(Sender: TObject);
begin
 FBackground.SetSize(ClientWidth, ClientHeight);
 RenderBrushedMetalBackground;
end;

procedure TFmCustomGui.FormShow(Sender: TObject);
var
  ParamIndex : Integer;
  MenuItem   : TMenuItem;
begin
 Assert(Owner is TVSTModule);

 // clear existing menu items
 MiParameter.Clear;

 for ParamIndex := 0 to TVSTModule(Owner).numParams - 1 do
  begin
   MenuItem := TMenuItem.Create(MiParameter);
   with MenuItem do
    begin
     Caption := string(TVSTModule(Self.Owner).ParameterName[ParamIndex]);
     RadioItem := True;
     Tag := ParamIndex;
     OnClick := AssignParameterClick;
    end;
   MiParameter.Add(MenuItem);
  end;
end;

procedure TFmCustomGui.AssignParameterClick(Sender: TObject);
begin
 if ActiveControl is TGuiStitchedDial then
  begin
   TGuiStitchedDial(ActiveControl).Tag := TMenuItem(Sender).Tag;
   ParameterUpdate(TMenuItem(Sender).Tag);
  end else
(*
 if ActiveControl is TGuiLabel then
  begin
   TGuiLabel(ActiveControl).Tag := TMenuItem(Sender).Tag;
   ParameterUpdate(TMenuItem(Sender).Tag);
  end else
*)
end;

procedure TFmCustomGui.MiAddBitmapClick(Sender: TObject);
begin
 if OpenPictureDialog.Execute
  then //AddBitmapLayer(OpenPictureDialog.FileName, FPopupPos.X, FPopupPos.Y);
end;

procedure TFmCustomGui.MiAddDialClick(Sender: TObject);
begin
 if OpenPictureDialog.Execute
  then //AddDialLayer(OpenPictureDialog.FileName, FPopupPos.X, FPopupPos.y);
end;

procedure TFmCustomGui.MiAddTextClick(Sender: TObject);
begin
 //AddTextLayer('Text', FPopupPos.X, FPopupPos.Y);
end;

procedure TFmCustomGui.MiDesignModeClick(Sender: TObject);
begin
 MiDesignMode.Checked := not MiDesignMode.Checked;
 MiSetBackground.Enabled := MiDesignMode.Checked;
 if not MiDesignMode.Checked
  then SaveToRegistry;
end;

procedure TFmCustomGui.MiLoadClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  begin
   DefaultExt := '.gui';
   Filter := 'GUI Designer (*.gui)|*.gui';
   if Execute
    then LoadFromINI(FileName);
  end;
end;

procedure TFmCustomGui.MiSetBackgroundClick(Sender: TObject);
begin
 if OpenPictureDialog.Execute then
  begin
   SetBackgRound(OpenPictureDialog.Filename);
   with CreateRegistry do
    try
     if OpenKey(CRegistryBaseKey + 'Background', True) then
      begin
       WriteString('Filename', OpenPictureDialog.FileName);
       WriteInteger('Width', FBackground.Width);
       WriteInteger('Height', FBackground.Height);
      end;
    finally
     CloseKey;
     Free;
    end;
  end;
end;

procedure TFmCustomGui.ParameterUpdate(ParameterIndex: Integer);
var
  ControlIndex : Integer;
begin
 Assert(Owner is TVSTModule);

 for ControlIndex := 0 to ControlCount - 1 do
  if Controls[ControlIndex] is TGuiStitchedDial then
   with TVSTModule(Owner), TGuiStitchedDial(Controls[ControlIndex]) do
    begin
     if Tag = ParameterIndex
      then StitchedImageIndex := Round((GlyphCount - 1) * VSTParameter[ParameterIndex]);
    end else
  if Controls[ControlIndex] is TGuiLabel then
   with TVSTModule(Owner), TGuiLabel(Controls[ControlIndex]) do
    begin
     if Tag = ParameterIndex
      then Text := string(ParameterDisplay[ParameterIndex] + ' ' +
        ParameterLabel[ParameterIndex]);
    end else
end;

procedure TFmCustomGui.PopupMenuPopup(Sender: TObject);
var
  MenuItemIndex : Integer;
begin
 // allow parameter / properties menu item visible if a selection is active
 MiProperties.Visible := ActiveControl <> nil;
 MiParameter.Visible := ActiveControl <> nil;

 if MiProperties.Visible then
  begin
//   MiProperties.Add();
  end;

(*
 if (ActiveControl <> nil) and Supports(ActiveControl, IVSTParameter) then
  begin
   for MenuItemIndex := 0 to MiParameter.Count - 1 do
    with MiParameter[MenuItemIndex]
     do Checked := Tag = (FSelection as IVSTParameter).Parameter;
  end;
*)

 // store popup position
 FPopupPos := ScreenToClient(Mouse.CursorPos);
end;

procedure TFmCustomGui.Clear;
begin

end;

function TFmCustomGui.CreateRegistry: TRegistry;
begin
 Result := TRegistry.Create;
 Result.RootKey := HKEY_CURRENT_USER;
end;

procedure TFmCustomGui.SetBackgRound(Filename: TFilename);
begin
 if FileExists(Filename)
  then FBackground.LoadFromFile(Filename);
end;

procedure TFmCustomGui.RenderBrushedMetalBackground;
var
  x, y         : Integer;
  FilterState  : Integer;
  ScanLinePtr  : PPixel32Array;
  MetalColor   : TPixel32;
  CurrentColor : TPixel32;
begin
 with FBackground do
  begin
   FilterState := 0;
   MetalColor := ConvertColor(Color);
   CurrentColor := MetalColor;
   FrameRect(Rect(0, 0, Width, Height), pxBlack32);
   for y := 1 to Height - 2 do
    begin
     ScanLinePtr := Scanline[Y];
     for x := 1 to Width - 2 do
      begin
       FilterState := ((Random(128) * 32 + FilterState * 992) shr 10);
       CurrentColor.R := MetalColor.R - FilterState;
       CurrentColor.G := MetalColor.G - FilterState;
       CurrentColor.B := MetalColor.B - FilterState;
       BlendPixelInplace(CurrentColor, ScanLinePtr^[x]);
      end;
    end;
   EMMS;
  end;
end;

procedure TFmCustomGui.RemoveAllCustomControls;
begin
 //
end;

procedure TFmCustomGui.LoadFromINI(FileName: TFileName);
var
  LayerIndex   : Integer;
  SectionName  : string;
  Sections     : TStringList;
  CurrentValue : string;
begin
 FBackground.Clear;
 RemoveAllCustomControls;

(*
 Gr32Gui.Layers.Clear;

 with TIniFile.Create(FileName) do
  begin
   SectionName := 'Background';

   if ValueExists(SectionName, 'FileName')
    then SetBackgRound(ReadString(SectionName, 'FileName', ''))
      else RenderBrushedMetalBackground;

   Sections := TStringList.Create;
   try
    ReadSections(Sections);
    for LayerIndex := 1 to Sections.Count - 1 do
     begin
      SectionName := Sections.Strings[LayerIndex];
      CurrentValue := ReadString(SectionName, 'Type', '');

      if CurrentValue = 'Text Layer' then
       begin
        CurrentValue := ReadString(SectionName, 'Text', '');
        with AddTextLayer(CurrentValue) do
         begin
          Alpha := ReadInteger(SectionName, 'Alpha', 255);
          Antialias := ReadInteger(SectionName, 'Antialias', 2);
          Font.Name := ReadString(SectionName, 'Font Name', Font.Name);
          Font.Size := ReadInteger(SectionName, 'Font Size', Font.Size);
          Font.Color := ReadInteger(SectionName, 'Font Color', Font.Color);
          Location := FloatRect(ReadFloat(SectionName, 'Left', Location.Left),
            ReadFloat(SectionName, 'Top', Location.Top),
            ReadFloat(SectionName, 'Right', Location.Right),
            ReadFloat(SectionName, 'Bottom', Location.Bottom));
         end;
       end else
      if CurrentValue = 'Dial Layer' then
       begin
        CurrentValue := ReadString(SectionName, 'Filename', '');
        with AddDialLayer(CurrentValue) do
         begin
          GlyphCount := ReadInteger(SectionName, 'Glyph Count', GlyphCount);
          StitchIndex := ReadInteger(SectionName, 'Glyph Index', StitchIndex);
          StitchDirection := TStitchDirection(ReadInteger(SectionName, 'Direction', Integer(StitchDirection)));
          Location := FloatRect(ReadFloat(SectionName, 'Left', Location.Left),
            ReadFloat(SectionName, 'Top', Location.Top),
            ReadFloat(SectionName, 'Right', Location.Right),
            ReadFloat(SectionName, 'Bottom', Location.Bottom));
         end;
       end else
      if CurrentValue = 'Bitmap Layer' then
       begin
        CurrentValue := ReadString(SectionName, 'Filename', '');
        AddBitmapLayer(CurrentValue);

        with AddBitmapLayer(CurrentValue) do
         begin
          Location := FloatRect(ReadFloat(SectionName, 'Left', Location.Left),
            ReadFloat(SectionName, 'Top', Location.Top),
            ReadFloat(SectionName, 'Right', Location.Right),
            ReadFloat(SectionName, 'Bottom', Location.Bottom));
         end;
       end;
     end;
   finally
    FreeAndNil(Sections);
   end;
  end;
*)
end;

procedure TFmCustomGui.LoadFromRegistry;
var
  LayerIndex   : Integer;
  KeyName      : string;
  Keys         : TStringList;
  CurrentValue : string;
//  CurrentLoc   : TFloatRect;
begin
 FBackground.Clear;
(*
 Gr32Gui.Layers.Clear;

 with CreateRegistry do
  try
   if OpenKey(CRegistryBaseKey + 'Background', True) then
    try
     // eventually read filename
     if FileExists(ReadString('Filename'))
      then SetBackgRound(ReadString('Filename'))
      else RenderBrushedMetalBackground;

     // eventually read width
     if ValueExists('Width')
      then ClientWidth := ReadInteger('Width');

     // eventually read width
     if ValueExists('Height')
      then ClientHeight := ReadInteger('Height');
    finally
     CloseKey;
    end;

   if OpenKey(CRegistryBaseKey, True) then
    begin
     Keys := TStringList.Create;
     try
      GetKeyNames(Keys);
      CloseKey;
      
      for LayerIndex := 1 to Keys.Count - 1 do
       begin
        KeyName := Keys.Strings[LayerIndex];

        // ignore background key
        if KeyName = 'Background'
         then Continue;

        if OpenKey(CRegistryBaseKey + KeyName, True) then
         try
          // continue if type doesn't exists
          if not ValueExists('Type')
           then Continue;

          CurrentValue := ReadString('Type');

          if CurrentValue = 'Text Layer' then
           begin
            // eventually read text
            if ValueExists('Text')
             then CurrentValue := ReadString('Text')
             else CurrentValue := '';

            // add text layer
            with AddTextLayer(CurrentValue) do
             begin
              // eventually read alpha property
              if ValueExists('Alpha')
               then Alpha := ReadInteger('Alpha');

              // eventually read antialias property
              if ValueExists('Antialias')
               then Antialias := ReadInteger('Antialias');

              // eventually read font name property
              if ValueExists('Font Name')
               then Font.Name := ReadString('Font Name');

              // eventually read font size property
              if ValueExists('Font Size')
               then Font.Size := ReadInteger('Font Size');

              // eventually read font color property
              if ValueExists('Font Color')
               then Font.Color := ReadInteger('Font Color');

              // eventually read parameter
              if ValueExists('Parameter')
               then Parameter := ReadInteger('Parameter');

              // read location
              if ValueExists('Location') then
               begin
                ReadBinaryData('Location', CurrentLoc, SizeOf(TFloatRect));
                Location := CurrentLoc;
               end;
             end;
           end else
          if CurrentValue = 'Dial Layer' then
           begin
            // eventually read filename
            if ValueExists('Filename')
             then CurrentValue := ReadString('Filename')
             else CurrentValue := '';

            with AddDialLayer(CurrentValue) do
             begin
              // eventually glyph count
              if ValueExists('Glyph Count')
               then GlyphCount := ReadInteger('Glyph Count');

              // eventually glyph index
              if ValueExists('Glyph Index')
               then StitchIndex := ReadInteger('Glyph Index');

              // eventually glyph direction
              if ValueExists('Glyph Direction')
               then StitchDirection := TStitchDirection(ReadInteger('Glyph Direction'));

              // eventually read parameter
              if ValueExists('Parameter')
               then Parameter := ReadInteger('Parameter');

              // read location
              if ValueExists('Location') then
               begin
                ReadBinaryData('Location', CurrentLoc, SizeOf(TFloatRect));
                Location := CurrentLoc;
               end;
             end;
           end else
          if CurrentValue = 'Bitmap Layer' then
           begin
            // eventually read filename
            if ValueExists('Filename')
             then CurrentValue := ReadString('Filename')
             else CurrentValue := '';

            with AddBitmapLayer(CurrentValue) do
             begin
              // read location
              if ValueExists('Location') then
               begin
                ReadBinaryData('Location', CurrentLoc, SizeOf(TFloatRect));
                Location := CurrentLoc;
               end;
             end;
           end;

         finally
          CloseKey;
         end;
       end;
     finally
      FreeAndNil(Keys);
     end;
    end;
  finally
   Free;
  end;
*)
end;

procedure TFmCustomGui.SaveToRegistry;
var
  LayerIndex   : Integer;
  KeyName      : string;
  Keys         : TStringList;
//  CurrentLoc   : TFloatRect;
begin
 with CreateRegistry do
  try
   if OpenKey(CRegistryBaseKey, True) then
    try
     // delete existing keys (except background key)
     Keys := TStringList.Create;
     try
      GetKeyNames(Keys);
      for LayerIndex := 0 to Keys.Count - 1 do
       if Keys.Strings[LayerIndex] <> 'Background'
        then DeleteKey(Keys.Strings[LayerIndex]);
     finally
      Keys.Free;
     end;
    finally
     CloseKey;
    end;

(*
  Selection := nil;

  for LayerIndex := 0 to Gr32Gui.Layers.Count - 1 do
   begin
    KeyName := 'Layer ' + IntToStr(LayerIndex + 1);
    if OpenKey(CRegistryBaseKey + KeyName, True) then
     try
      if Gr32Gui.Layers[LayerIndex] is TTextLayer then
       with TTextLayer(Gr32Gui.Layers[LayerIndex]) do
        begin
         WriteString('Type', 'Text Layer');
         WriteInteger('Alpha', Alpha);
         WriteInteger('Antialias', Antialias);
         WriteString('Font Name', Font.Name);
         WriteInteger('Font Size', Font.Size);
         WriteInteger('Font Color', Font.Color);

         if Parameter >= 0
          then WriteInteger('Parameter', Parameter)
          else WriteString('Text', Text);

         CurrentLoc := Location;
         WriteBinaryData('Location', CurrentLoc, SizeOf(TFloatRect));
        end else
      if Gr32Gui.Layers[LayerIndex] is TDialLayer then
       with TDialLayer(Gr32Gui.Layers[LayerIndex]) do
        begin
         WriteString('Type', 'Dial Layer');

         // hack to store the filename
         if TObject(Tag) is TExtraData
          then WriteString('Filename', TExtraData(Tag).FileName);

         WriteInteger('Glyph Count', GlyphCount);
         WriteInteger('Glyph Index', StitchIndex);
         WriteInteger('Direction', Integer(StitchDirection));

         WriteInteger('Parameter', Parameter);

         CurrentLoc := Location;
         WriteBinaryData('Location', CurrentLoc, SizeOf(TFloatRect));
        end else
       if Gr32Gui.Layers[LayerIndex] is TRubberbandLayer
        then Continue else
       if Gr32Gui.Layers[LayerIndex] is TBitmapLayer then
        with TBitmapLayer(Gr32Gui.Layers[LayerIndex]) do
         begin
          WriteString('Type', 'Bitmap Layer');

          // hack to store the filename
          if TObject(Tag) is TExtraData
           then WriteString('Filename', TExtraData(Tag).FileName);

          CurrentLoc := Location;
          WriteBinaryData('Location', CurrentLoc, SizeOf(TFloatRect));
         end
     finally
      CloseKey;
     end;
    end;
*)
  finally
   Free;
  end;
end;

end.
