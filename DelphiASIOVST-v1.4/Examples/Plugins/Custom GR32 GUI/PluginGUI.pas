unit PluginGUI;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Graphics, Menus, Controls, Dialogs, ExtDlgs, Registry, DAV_Types, 
  DAV_VSTModule, GR32, GR32_Image, GR32_Blend, GR32_Layers, GuiLayers;

type
  TExtraData = class
  private
    FFileName: TFileName;
  public
    constructor Create(Filename: TFileName); virtual;
    property Filename: TFileName read FFileName write FFileName;
  end;

  TFmCustomGr32Gui = class(TForm)
    Gr32Gui: TImage32;
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
    OpenPictureDialog: TOpenPictureDialog;
    PopupMenu: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Gr32GuiKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Gr32GuiMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure MiAddBitmapClick(Sender: TObject);
    procedure MiAddDialClick(Sender: TObject);
    procedure MiAddTextClick(Sender: TObject);
    procedure MiDesignModeClick(Sender: TObject);
    procedure MiLoadClick(Sender: TObject);
    procedure MiSetBackgroundClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    FSelection : TPositionedLayer;
    FRBLayer   : TRubberbandLayer;
    FPopupPos  : TPoint;
    function CreateRegistry: TRegistry;
    procedure RenderBrushedMetalBackground;
    procedure SetBackgRound(Filename: TFilename);
    procedure SetSelection(const Value: TPositionedLayer);
    procedure UpdateSelection;
    procedure LayerChangeHandler(Sender: TObject);
    procedure LayerDestroyHandler(Sender: TObject);
    procedure LayerMouseDown(Sender: TObject; Buttons: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AssignParameterClick(Sender: TObject);
    procedure DialLayerChanged(Sender: TObject);
  protected
    function AddLayer(ItemClass: TLayerClass): TPositionedLayer; virtual;
    function AddBitmapLayer(FileName: TFileName; X: Single = 0; Y: Single = 0): TBitmapLayer; virtual;
    function AddDialLayer(FileName: TFileName; X: Single = 0; Y: Single = 0): TDialLayer; virtual;
    function AddTextLayer(Caption: string; X: Single = 0; Y: Single = 0): TTextLayer; virtual;
    procedure Clear;
  public
    procedure LoadFromINI(FileName: TFileName);
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    procedure ParameterUpdate(ParameterIndex: Integer);

    property Selection: TPositionedLayer read FSelection write SetSelection;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Inifiles, GraphicEx, DAV_VSTModuleWithPrograms, DAV_VSTParameters;

const
  CRegistryBaseKey = 'SOFTWARE\Delphi ASIO & VST Project\GR32\';

{ TExtraData }

constructor TExtraData.Create(Filename: TFileName);
begin
 FFileName := Filename;
end;


{ TFmCustomGr32Gui }

procedure TFmCustomGr32Gui.FormCreate(Sender: TObject);
begin
 LoadFromRegistry;

 // set open picture dialog filters
 OpenPictureDialog.Filter := FileFormatList.GetGraphicFilter([ftAnimation,
   ftLayered, ftMultiImage, ftRaster, ftVector], fstNone, [foIncludeAll], nil);
end;

procedure TFmCustomGr32Gui.FormShow(Sender: TObject);
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

procedure TFmCustomGr32Gui.AssignParameterClick(Sender: TObject);
begin
 if Selection is TTextLayer then
  begin
   TTextLayer(Selection).Parameter := TMenuItem(Sender).Tag;
   ParameterUpdate(TMenuItem(Sender).Tag);
  end else
 if Selection is TDialLayer then
  begin
   TDialLayer(Selection).Parameter := TMenuItem(Sender).Tag;
   ParameterUpdate(TMenuItem(Sender).Tag);
  end else
end;

procedure TFmCustomGr32Gui.MiAddBitmapClick(Sender: TObject);
begin
 if OpenPictureDialog.Execute
  then AddBitmapLayer(OpenPictureDialog.FileName, FPopupPos.X, FPopupPos.Y);
end;

procedure TFmCustomGr32Gui.MiAddDialClick(Sender: TObject);
begin
 if OpenPictureDialog.Execute
  then AddDialLayer(OpenPictureDialog.FileName, FPopupPos.X, FPopupPos.y);
end;

procedure TFmCustomGr32Gui.MiAddTextClick(Sender: TObject);
begin
 AddTextLayer('Text', FPopupPos.X, FPopupPos.Y);
end;

procedure TFmCustomGr32Gui.MiDesignModeClick(Sender: TObject);
begin
 MiDesignMode.Checked := not MiDesignMode.Checked;
 MiSetBackground.Enabled := MiDesignMode.Checked;
 if not MiDesignMode.Checked
  then SaveToRegistry;
end;

procedure TFmCustomGr32Gui.MiLoadClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  begin
   DefaultExt := '.gui';
   Filter := 'GUI Designer (*.gui)|*.gui';
   if Execute
    then LoadFromINI(FileName);
  end;
end;

procedure TFmCustomGr32Gui.MiSetBackgroundClick(Sender: TObject);
begin
 if OpenPictureDialog.Execute then
  begin
   SetBackgRound(OpenPictureDialog.Filename);
   with CreateRegistry do
    try
     if OpenKey(CRegistryBaseKey + 'Background', True) then
      begin
       WriteString('Filename', OpenPictureDialog.FileName);
       WriteInteger('Width', Gr32Gui.Bitmap.Width);
       WriteInteger('Height', Gr32Gui.Bitmap.Height);
      end;
    finally
     CloseKey;
     Free;
    end;
  end;
end;

procedure TFmCustomGr32Gui.ParameterUpdate(ParameterIndex: Integer);
var
  LayerIndex : Integer;
begin
 Assert(Owner is TVSTModule);

 with TVSTModule(Owner) do
  for LayerIndex := 0 to Gr32Gui.Layers.Count - 1 do
   if Gr32Gui.Layers[LayerIndex] is TTextLayer then
    with TTextLayer(Gr32Gui.Layers[LayerIndex]) do
     begin
      if Parameter = ParameterIndex
       then Text := string(ParameterDisplay[ParameterIndex] + ' ' +
         ParameterLabel[ParameterIndex]);
     end else
   if Gr32Gui.Layers[LayerIndex] is TDialLayer then
    with TDialLayer(Gr32Gui.Layers[LayerIndex]) do
     if Parameter = ParameterIndex
      then StitchIndex := Round((GlyphCount - 1) * VSTParameter[ParameterIndex]);
end;

procedure TFmCustomGr32Gui.DialLayerChanged(Sender: TObject);
begin
 Assert(Sender is TDialLayer);
 with TVSTModule(Self.Owner), TDialLayer(Sender) do
  begin
   VSTParameter[Parameter] := StitchIndex / (GlyphCount - 1);
  end;
end;

procedure TFmCustomGr32Gui.PopupMenuPopup(Sender: TObject);
var
  MenuItemIndex : Integer;
begin
 // allow parameter / properties menu item visible if a selection is active
 MiProperties.Visible := Assigned(FSelection);
 MiParameter.Visible := Assigned(FSelection);

 if MiProperties.Visible then
  begin
//   MiProperties.Add();
  end;

 if Assigned(FSelection) and Supports(FSelection, IVSTParameter) then
  begin
   for MenuItemIndex := 0 to MiParameter.Count - 1 do
    with MiParameter[MenuItemIndex]
     do Checked := Tag = (FSelection as IVSTParameter).Parameter;
  end;

 // store popup position
 FPopupPos := ScreenToClient(Mouse.CursorPos);
end;

function TFmCustomGr32Gui.AddLayer(ItemClass: TLayerClass): TPositionedLayer;
begin
 Result := TPositionedLayer(Gr32Gui.Layers.Add(ItemClass));
end;

function TFmCustomGr32Gui.AddBitmapLayer(FileName: TFileName; X,
  Y: Single): TBitmapLayer;
var
  BgGraphic      : TGraphic;
  BgGraphicClass : TGraphicClass;
begin
 Result := TBitmapLayer(AddLayer(TBitmapLayer));
 with Result do
  begin
   Bitmap.DrawMode := dmBlend;
   Scaled := True;
   AlphaHit := True;
   Cropped := True;
   OnMouseDown := LayerMouseDown;

   // hack to store the filename and other extra data
   Tag := Integer(TExtraData.Create(FileName));

   BgGraphicClass := FileFormatList.GraphicFromExtension(ExtractFileExt(FileName));
   if BgGraphicClass <> nil then
    begin
     BgGraphic := BgGraphicClass.Create;
     try
      BgGraphic.LoadFromFile(FileName);
      Bitmap.Assign(BgGraphic);
      Location := FloatRect(X, Y, X + BgGraphic.Width, Y + BgGraphic.Height);
     finally
      FreeAndNil(BgGraphic);
     end;
    end;
   Changed;
  end;
end;

function TFmCustomGr32Gui.AddDialLayer(FileName: TFileName; X,
  Y: Single): TDialLayer;
var
  BgGraphic      : TGraphic;
  BgGraphicClass : TGraphicClass;
begin
 Result := TDialLayer(AddLayer(TDialLayer));
 with Result do
  begin
   Bitmap.DrawMode := dmBlend;
   Scaled := True;
   AlphaHit := True;
   Cropped := True;
   OnMouseDown := LayerMouseDown;
   OnChanged := DialLayerChanged;

   // hack to store the filename and other extra data
   Tag := Integer(TExtraData.Create(FileName));

   BgGraphicClass := FileFormatList.GraphicFromExtension(ExtractFileExt(FileName));
   if BgGraphicClass <> nil then
    begin
     BgGraphic := BgGraphicClass.Create;
     try
      BgGraphic.LoadFromFile(FileName);
      LoadFromGraphic(BgGraphic);
     finally
      FreeAndNil(BgGraphic);
     end;
    end;
   Changed;
  end;
end;

function TFmCustomGr32Gui.AddTextLayer(Caption: string; X,
  Y: Single): TTextLayer;
begin
 Result := TTextLayer(AddLayer(TTextLayer));
 with Result do
  begin
   Bitmap.DrawMode := dmBlend;
   Bitmap.ResamplerClassName := 'TLinearResampler';
   Scaled := True;
   AlphaHit := False;
   Cropped := True;
   Font.Size := 10;
   Text := Caption;
   Location := FloatRect(X, Y, X + Bitmap.TextWidth(Text),
     Y + Bitmap.TextHeight(Text));
   OnMouseDown := LayerMouseDown;
   Changed;
  end;
end;

procedure TFmCustomGr32Gui.Clear;
begin

end;

function TFmCustomGr32Gui.CreateRegistry: TRegistry;
begin
 Result := TRegistry.Create;
 Result.RootKey := HKEY_CURRENT_USER;
end;

procedure TFmCustomGr32Gui.SetBackgRound(Filename: TFilename);
begin
 if FileExists(Filename)
  then Gr32Gui.Bitmap.LoadFromFile(Filename);
end;

procedure TFmCustomGr32Gui.SetSelection(const Value: TPositionedLayer);
begin
 if Value <> FSelection then
  begin
   if FRBLayer <> nil then
    begin
     FRBLayer.ChildLayer := nil;
     FRBLayer.LayerOptions := LOB_NO_UPDATE;
     Gr32Gui.Invalidate;
    end;

   FSelection := Value;

   if MiDesignMode.Checked
    then UpdateSelection;
  end;
end;

procedure TFmCustomGr32Gui.UpdateSelection;
begin
 if (FSelection <> nil) then
  begin
   FSelection.OnChange := LayerChangeHandler;
   FSelection.OnDestroy := LayerDestroyHandler;
   if FRBLayer = nil then
    begin
     FRBLayer := TRubberBandLayer.Create(Gr32Gui.Layers);
     FRBLayer.MinHeight := 1;
     FRBLayer.MinWidth := 1;
    end else FRBLayer.BringToFront;
   with FRBLayer do
    begin
     Scaled := True;
     ChildLayer := FSelection;
     if not (ChildLayer is TTextLayer)
      then Options := [roProportional, roConstrained];
     LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS or LOB_NO_UPDATE;
    end;
  end;
end;

procedure TFmCustomGr32Gui.LayerChangeHandler(Sender: TObject);
begin
 if Assigned(FRBLayer) and Assigned(FRBLayer.ChildLayer) then
  begin
   with FRBLayer.ChildLayer.Location do
    if (Left <> FRBLayer.Location.Left) or
       (Top <> FRBLayer.Location.Top) or
       (Right <> FRBLayer.Location.Right) or
       (Bottom <> FRBLayer.Location.Bottom)
     then FRBLayer.Location := FRBLayer.ChildLayer.Location;
  end;
end;

procedure TFmCustomGr32Gui.LayerDestroyHandler(Sender: TObject);
begin
 if Selection = Sender
  then Selection := nil;
end;

procedure TFmCustomGr32Gui.LayerMouseDown(Sender: TObject;
  Buttons: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Sender <> nil
  then Selection := TPositionedLayer(Sender);
end;

procedure TFmCustomGr32Gui.Gr32GuiKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if FRBLayer <> nil then
  with FRBLayer do
   case Key of
    VK_DELETE : if Assigned(Selection) then Selection.Free;
    VK_LEFT: Location := FloatRect(Location.Left - 1, Location.Top,
      Location.Right - 1, Location.Bottom);
    VK_RIGHT: Location := FloatRect(Location.Left + 1, Location.Top,
      Location.Right + 1, Location.Bottom);
    VK_UP: Location := FloatRect(Location.Left, Location.Top - 1,
      Location.Right, Location.Bottom - 1);
    VK_DOWN: Location := FloatRect(Location.Left, Location.Top + 1,
      Location.Right, Location.Bottom + 1);
   end;
end;

procedure TFmCustomGr32Gui.Gr32GuiMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
 if Layer = nil
  then Selection := nil;
end;

procedure TFmCustomGr32Gui.RenderBrushedMetalBackground;
var
  x, y         : Integer;
  FilterState  : Integer;
  ColorLinePtr : PColor32Array;
  MetalColor   : TColor32;
begin
 with Gr32Gui.Bitmap do
  begin
   SetSize(ClientWidth, ClientHeight);

   FilterState := 0;
   MetalColor := Color32(Color);
   with Gr32Gui.Bitmap do
    begin
     FrameRectS(0, 0, Width - 1, Height - 1, clBlack32);
     for y := 1 to Height - 2 do
      begin
       ColorLinePtr := Scanline[Y];
       for x := 1 to Width - 2 do
        begin
         FilterState := ((Random(128) * 32 + FilterState * 992) shr 10);
         BlendMem(Lighten(MetalColor, FilterState - 64), ColorLinePtr^[x]);
        end;
      end;
    end;
   EMMS;
  end;
end;

procedure TFmCustomGr32Gui.LoadFromINI(FileName: TFileName);
var
  LayerIndex   : Integer;
  SectionName  : string;
  Sections     : TStringList;
  CurrentValue : string;
begin
 Gr32Gui.Bitmap.Clear;
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
end;

procedure TFmCustomGr32Gui.LoadFromRegistry;
var
  LayerIndex   : Integer;
  KeyName      : string;
  Keys         : TStringList;
  CurrentValue : string;
  CurrentLoc   : TFloatRect;
begin
 Gr32Gui.Bitmap.Clear;
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
end;

procedure TFmCustomGr32Gui.SaveToRegistry;
var
  LayerIndex   : Integer;
  KeyName      : string;
  Keys         : TStringList;
  CurrentLoc   : TFloatRect;
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
  finally
   Free;
  end;
end;

end.
