unit TestDAV_GuiStitchedControls;

interface

uses
  TestFramework, SysUtils, Windows, Classes, Contnrs, Messages, Forms,
  Controls, Graphics, DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiStitchedControls,
  DAV_GuiStitchedDial, DAV_GuiStitchedSwitch, DAV_GuiStitchedDisplay,
  DAV_GuiStitchedImageList, DAV_GuiStitchedPngList;

type
  TestTGuiStitchedImageList = class(TTestCase)
  strict private
    FGuiStitchedImageList: TGuiStitchedImageList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLinkControl;
    procedure TestItems;
  end;

  TestTGuiStitchedSwitch = class(TTestCase)
  strict private
    FGuiStitchedSwitch: TGuiStitchedSwitch;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLinkImageList;
  end;

  TestTGuiStitchedDial = class(TTestCase)
  strict private
    FGuiStitchedDial: TGuiStitchedDial;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLinkImageList;
  end;

implementation

{ TestTGuiStitchedImageList }

procedure TestTGuiStitchedImageList.SetUp;
begin
 FGuiStitchedImageList := TGuiStitchedImageList.Create(nil);
end;

procedure TestTGuiStitchedImageList.TearDown;
begin
 FreeAndNil(FGuiStitchedImageList);
end;

procedure TestTGuiStitchedImageList.TestItems;
begin
 with FGuiStitchedImageList.StitchedImages.Add do
  begin
   StitchedPixelMap.SetSize(8, 8);
   Width := 8;
   Height := 8;
   GlyphCount := 2;
   GlyphCount := 4;
   StitchKind := skVertical;
   StitchKind := skHorizontal;
  end;
 FGuiStitchedImageList.StitchedImages.Clear;

 CheckEquals(0, FGuiStitchedImageList.StitchedImages.Count);
end;

procedure TestTGuiStitchedImageList.TestLinkControl;
var
  Switch : TGuiStitchedSwitch;
begin
 Switch := TGuiStitchedSwitch.Create(nil);
 try
  Switch.StitchedImageList := FGuiStitchedImageList;
 finally
  FreeAndNil(Switch);
 end;
end;


{ TestTGuiStitchedSwitch }

procedure TestTGuiStitchedSwitch.SetUp;
begin
 inherited;
 FGuiStitchedSwitch := TGuiStitchedSwitch.Create(nil);
end;

procedure TestTGuiStitchedSwitch.TearDown;
begin
 inherited;
 FreeAndNil(FGuiStitchedSwitch);
end;

procedure TestTGuiStitchedSwitch.TestLinkImageList;
var
  ImageList : TGuiStitchedImageList;
begin
 ImageList := TGuiStitchedImageList.Create(nil);
 try
  with ImageList.StitchedImages.Add do
   begin
    StitchedPixelMap.SetSize(8, 8);
    Width := 8;
    Height := 8;
    GlyphCount := 2;
   end;

  with ImageList.StitchedImages.Add do
   begin
    StitchedPixelMap.SetSize(10, 10);
    Width := 10;
    Height := 10;
    GlyphCount := 5;
    StitchKind := skVertical;
   end;
  CheckEquals(2, ImageList.Count);

  // assign new image list and first stitched image
  FGuiStitchedSwitch.StitchedImageList := ImageList;
  FGuiStitchedSwitch.StitchedImageIndex := 0;
  FGuiStitchedSwitch.GlyphIndex := 0;

  // assign second stitched image
  FGuiStitchedSwitch.StitchedImageIndex := 1;
  FGuiStitchedSwitch.GlyphIndex := 4;

  // delete first item and check whether the index has been corrected
  ImageList.StitchedImages.Delete(0);
  CheckEquals(0, FGuiStitchedSwitch.StitchedImageIndex);
//  CheckEquals(-1, FGuiStitchedSwitch.GlyphIndex);

  // unlink list
  FGuiStitchedSwitch.StitchedImageList := nil;
  CheckEquals(0, FGuiStitchedSwitch.GlyphCount);
  CheckEquals(-1, FGuiStitchedSwitch.GlyphIndex);

  // assign image list and first stitched image
  FGuiStitchedSwitch.StitchedImageList := ImageList;
  FGuiStitchedSwitch.StitchedImageIndex := 0;
 finally
  FreeAndNil(ImageList);
 end;

 CheckEquals(0, FGuiStitchedSwitch.GlyphCount);
 CheckEquals(-1, FGuiStitchedSwitch.GlyphIndex);
 CheckEquals(-1, FGuiStitchedSwitch.StitchedImageIndex);
 CheckTrue(FGuiStitchedSwitch.StitchedImageList = nil);
end;


{ TestTGuiStitchedDial }

procedure TestTGuiStitchedDial.SetUp;
begin
 FGuiStitchedDial := TGuiStitchedDial.Create(nil);
end;

procedure TestTGuiStitchedDial.TearDown;
begin
 FreeAndNil(FGuiStitchedDial);
end;

procedure TestTGuiStitchedDial.TestLinkImageList;
var
  PngList : TGuiStitchedPngList;
begin
 PngList := TGuiStitchedPngList.Create(nil);
 try
  with PngList.StitchedPNGs.Add do
   begin
    StitchedPixelMap.SetSize(8, 8);
    Width := 8;
    Height := 8;
    GlyphCount := 2;
   end;

  with PngList.StitchedPNGs.Add do
   begin
    StitchedPixelMap.SetSize(10, 10);
    Width := 10;
    Height := 10;
    GlyphCount := 5;
    StitchKind := skVertical;
   end;
  CheckEquals(2, PngList.Count);

  // assign new image list and first stitched image
  FGuiStitchedDial.StitchedImageList := PngList;
  FGuiStitchedDial.StitchedImageIndex := 0;
  FGuiStitchedDial.Value := 20;

  // assign second stitched image
  FGuiStitchedDial.StitchedImageIndex := 1;
  FGuiStitchedDial.Value := 90;

  // delete first item and check whether the index has been corrected
  PngList.StitchedPNGs.Delete(0);
  CheckEquals(0, FGuiStitchedDial.StitchedImageIndex);

  // unlink list
  FGuiStitchedDial.StitchedImageList := nil;
  CheckEquals(0, FGuiStitchedDial.GlyphCount);

  // assign image list and first stitched image
  FGuiStitchedDial.StitchedImageList := PngList;
  FGuiStitchedDial.StitchedImageIndex := 0;
 finally
  FreeAndNil(PngList);
 end;

 CheckEquals(0, FGuiStitchedDial.GlyphCount);
 CheckEquals(-1, FGuiStitchedDial.StitchedImageIndex);
 CheckTrue(FGuiStitchedDial.StitchedImageList = nil);
end;

initialization
  RegisterTest(TestTGuiStitchedImageList.Suite);
  RegisterTest(TestTGuiStitchedSwitch.Suite);
  RegisterTest(TestTGuiStitchedDial.Suite);

end.
