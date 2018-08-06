unit TestDAV_GuiPixelMap;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}fpcunit, testutils, testregistry, {$ELSE} TestFramework, {$ENDIF}
  SysUtils, Windows, Classes, Messages, Controls, Graphics, DAV_GuiCommon,
  DAV_GuiPixelMap, DAV_MemoryUtils, DAV_GuiCustomMap, DAV_GuiBlend, DAV_Common,
  DAV_GuiByteMap;

type
  TestTGuiCustomPixelMap = class(TTestCase)
  protected
    FGuiPixelMap : TGuiCustomPixelMap;
    procedure TestAssign; virtual; abstract;
  public
    procedure TearDown; override;
  published
    procedure TestResize;
    procedure TestRectangleClear;
    procedure TestMakeOpaqueResetAlpha;
  end;

  TestTGuiPixelMapMemory = class(TestTGuiCustomPixelMap)
  public
    procedure SetUp; override;
  published
    procedure TestAssign; override;
  end;

  TestTGuiPixelMapDIB = class(TestTGuiCustomPixelMap)
  public
    procedure SetUp; override;
  published
    procedure TestAssign; override;
  end;

implementation

{ TestTGuiCustomPixelMap }

procedure TestTGuiCustomPixelMap.TearDown;
begin
 FreeAndNil(FGuiPixelMap);
end;

procedure TestTGuiCustomPixelMap.TestMakeOpaqueResetAlpha;
begin
 with FGuiPixelMap do
  begin
   SetSize(2, 2);
   Clear;
   CheckEquals(0, Pixel[0, 0].A);
   MakeOpaque;
   CheckEquals($FF, Pixel[0, 0].A);
   CheckEquals($FF, Pixel[0, 1].A);
   CheckEquals($FF, Pixel[1, 0].A);
   CheckEquals($FF, Pixel[1, 1].A);
   ResetAlpha(0);
   CheckEquals(0, Pixel[0, 0].A);
   CheckEquals(0, Pixel[0, 1].A);
   CheckEquals(0, Pixel[1, 0].A);
   CheckEquals(0, Pixel[1, 1].A);
  end;
end;

procedure TestTGuiCustomPixelMap.TestRectangleClear;
begin
 with FGuiPixelMap do
  begin
   SetSize(1, 1);
   FillRect(ClientRect, pxGray32);
   CheckEquals(pxGray32.ARGB, Pixel[0, 0].ARGB);
   Clear;
   CheckEquals(0, Pixel[0, 0].ARGB);
  end;
end;

procedure TestTGuiCustomPixelMap.TestResize;
var
  Index : Integer;
begin
 for Index := 0 to $1FF do
  with FGuiPixelMap do
   begin
    SetSize(0, Index);
    Width := 1;
    SetSize(Index, 0);
    Height := 1;
    SetSize(0, 0);
    SetSize(Index, Index);
   end;
end;


{ TestTGuiPixelMapMemory }

procedure TestTGuiPixelMapMemory.SetUp;
begin
 FGuiPixelMap := TGuiPixelMapMemory.Create;
end;

procedure TestTGuiPixelMapMemory.TestAssign;
var
  PixelMap : TGuiPixelMapMemory;
begin
 inherited;
 PixelMap := TGuiPixelMapMemory.Create;
 PixelMap.Assign(FGuiPixelMap);
end;


{ TestTGuiPixelMapDIB }

procedure TestTGuiPixelMapDIB.SetUp;
begin
 FGuiPixelMap := TGuiPixelMapDIB.Create;
end;

procedure TestTGuiPixelMapDIB.TestAssign;
var
  PixelMap : TGuiPixelMapDIB;
begin
 inherited;
 PixelMap := TGuiPixelMapDIB.Create;
 PixelMap.Assign(FGuiPixelMap);
end;


initialization
  {$IFDEF FPC}
  RegisterTest(TestTGuiPixelMapMemory);
  RegisterTest(TestTGuiPixelMapDIB);
  {$ELSE}
  RegisterTest(TestTGuiPixelMapMemory.Suite);
  RegisterTest(TestTGuiPixelMapDIB.Suite);
  {$ENDIF}

end.
