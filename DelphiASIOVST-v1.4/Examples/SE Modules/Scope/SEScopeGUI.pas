unit SEScopeGUI;

interface

uses
  Windows, DAV_SEModule, DAV_SEGUI, SEScopeModule;

const
  pinEnumOut = 2;

type
  TSEScopeGui = class(TSEGUIBase)
  private
    FDrawTrace : array [0..CScopeChannels - 1] of Boolean;
    FValues    : array [0..CScopeChannels - 1, 0..CScopeBufferSize + 3] of TSEFloatSample; // must allow generator to overfill by 2 samples
    FFontInfo  : TSeFontInfo;
    function InvalidateControl: Integer;
  protected
    procedure GuiPaint(hDC: HDC; wi: PSEWndInfo); override;
    procedure GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
    destructor Destroy; override;
(*
    function GuiIdle: Boolean; virtual;
    procedure Initialise(LoadedFromFile): Boolean;
*)
  end;

implementation

uses
  DAV_Types, SysUtils;

constructor TSEScopeGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
var
  i : Integer;
begin
 inherited;

 FDrawTrace[0] := True;
 FDrawTrace[1] := True;
 // clear values
 for i := CScopeBufferSize - 1 downto 0 do
  begin
   FValues[0][i] := 1000; // off-screen
   FValues[1][i] := 1000; // off-screen
  end;

 CallHost(seGuiHostSetWindowSize, 100, 100);
 CallHost(seGuiHostSetWindowType, 0); // 0 = Draw on SE's window (default), 1 = HWND based

// CallHost(seGuiHostSetWindowFlags, Integer(HWF_RESIZEABLE or HWF_NO_CUSTOM_GFX_ON_STRUCTURE));
 CallHost(seGuiHostSetWindowFlags, Integer(hwfResizable));
end;

destructor TSEScopeGui.Destroy;
begin
 inherited;
end;

(*
procedure TSEScopeGui.Initialise(LoadedFromFile: Boolean);
begin
 inherited Initialise(LoadedFromFile);
 CallHost(seGuiHostSetIdle, 1);
end;

function TSEScopeGui.GuiIdle: Boolean;
var
  test : Integer;
begin
 test := 9;
 Result := True;
end;
*)

(*
procedure TSEScopeGui.OnWindowOpen(wi: PSEWndInfo);
begin
  // get the full path of an imbedded file when you only know it's short name
  const int MAX_STRING_LENGTH = 300;

  // Both destination is UNICODE (two-byte) character string
  unsigned short dest[MAX_STRING_LENGTH];

  CallHost( seGuiHostPlugGetExtraData, PN_ENUM_OUT, MAX_STRING_LENGTH, &dest);

  // to convert to ascii
  char ascii_text[MAX_STRING_LENGTH];
  WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_text, MAX_STRING_LENGTH, NULL, NULL);


  // example of accessing the Window handle ( windowtype=1 only)
  HWND hWnd = (HWND) CallHost(seGuiHostGetWindowHandle, wi.context_handle );

RECT r;
  GetClientRect( hWnd, &r );
HWND hTextArea = CreateWindow( "edit", 
   NULL, WS_CHILD | ES_LEFT | WS_SIZEBOX | ES_MULTILINE | WS_VSCROLL,
   r.left, r.top, r.right - r.left, r.bottom - r.top, 
   hWnd, NULL,(HINSTANCE)Handle(), NULL );
ShowWindow( hTextArea, SW_SHOW );
end;*)

procedure TSEScopeGui.GuiPaint(hDC: HDC; wi :PSEWndInfo);
var
  FontHandle      : HFONT;
  Scale, y        : Single;
  Mid             : TPoint;
  BackgroundBrush : HBRUSH;
  R               : TRect;
  DarkedColor, v  : Integer;
  FontHeight      : Integer;
  Pen             : HPEN;
  OldPen          : HGDIOBJ;
  TickWidth, i    : Integer;
  Step, Chan, x   : Integer;
  OldFont         : HGDIOBJ;
  txt             : string;
  ChunkName       : TChunkName;
begin
(* testing
  long parent_context = wi.context_handle;
  HWND h = 0;
  while( h == 0 )
  begin
    parent_context = CallHost(seGuiHostGetParentContext, parent_context );
    h = (HWND) CallHost( seGuiHostGetWindowHandle, parent_context );
  end;

  sepoint offset(0,0);
  CallHost(seGuiHostMapWindowPoints, wi.context_handle, parent_context, @offset, 0 );
*)

  ChunkName := 'tty';
  FontHandle := HFONT(CallHost(seGuiHostGetFontInfo, wi.ContextHandle, Integer(ChunkName), @FFontInfo));

  Scale := wi.height / 2.15;
  Mid.x := wi.width div 2;
  Mid.y := wi.height div 2;

  if FFontInfo.ColorBackground >= 0 then // -1 indicates transparent background
   begin
    // Fill in solid background black
    BackgroundBrush := CreateSolidBrush(FFontInfo.ColorBackground);
    try
     R.Top    := 0;
     R.Left   := 0;
     R.Right  := wi.width + 1;
     R.Bottom := wi.height + 1;
     FillRect(hDC, r, BackgroundBrush);
    finally
     // cleanup objects
     DeleteObject(BackgroundBrush);
    end;
   end;

  // create a green pen
  DarkedColor := (FFontInfo.color shr 1) and $7F7F7F;
  pen := CreatePen(PS_SOLID, 1, DarkedColor); // dark green

  // 'select' it
  OldPen := SelectObject(hDC, pen);

  // BACKGROUND LINES
  // horizontal line
  MoveToEx(hDC, 0, Mid.y, nil);
  LineTo(hDC, wi.width, Mid.y);

  // vertical line
  MoveToEx(hDC, Mid.x, 0, nil);
  LineTo(hDC, Mid.x, wi.Height);

  // voltage ticks
  TickWidth := 2;
  Step := 1;
  if wi.height < 50 then step := 4;

  v := -10;
  while v <= 10 do
   begin
    y := v * scale * 0.1;

    if (v mod 5 = 0)
     then TickWidth := 4
     else TickWidth := 2;

    MoveToEx(hDC, mid.x - TickWidth, mid.y + Round(y), nil);
    LineTo(hDC, mid.x + TickWidth, mid.y + Round(y));
    Inc(v, Step);
   end;

  // labels
  if wi.height > 30 then
   begin
    FontHeight := FFontInfo.FontHeight;

    OldFont := SelectObject(hDC, FontHandle);

    SetTextColor(hDC, FFontInfo.Color);
    SetBkMode(hDC, TRANSPARENT);
    SetTextAlign(hDC, TA_LEFT);

    v := -10;
    while v <= 10 do
     begin
      y := v * scale * 0.1;
      txt := IntToStr(v);
      TextOut(hDC, Mid.x + TickWidth, Mid.y - Round(y - FontHeight * 0.5), @txt[1], Length(txt));
      v := v + 5;
    end;
(*
    if( input_state_error_flag )
    begin
      TextOut( hDC,0,0, "IN STATE ERR!" );
    end;
*)
    SelectObject(hDC, OldFont);
  end;

  // clean up
  SelectObject(hDC, OldPen);
  DeleteObject(pen);

  // trace
  pen := CreatePen(PS_SOLID, 1, FFontInfo.Color);
  SelectObject(hDC, pen);

  for Chan := 0 to CScopeChannels do
   begin
    if (FDrawTrace[chan]) then
     begin
      MoveToEx(hDC, 0, Mid.y - Round(FValues[chan][0] * scale), nil);

      for i := 1 to CScopeBufferSize - 1 do
       begin
        x := (i * wi.width) div CScopeBufferSize;
        LineTo(hDC, x, Mid.y - Round(FValues[chan][i] * scale));
       end;
     end;
    SelectObject(hDC, OldPen);
    DeleteObject(Pen);

    // trace yellow
    pen := CreatePen(PS_SOLID, 1, RGB(250,250,0)); // yellow
    SelectObject(hDC, Pen);
   end;

  // cleanup
  SelectObject(hDC, OldPen);
  DeleteObject(Pen);
end;

procedure TSEScopeGui.GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer);
begin
  assert(ALength = SizeOf(Fvalues));
  Move(FValues[0, 0], AData^, ALength);
  InvalidateControl;
end;

function TSEScopeGui.InvalidateControl: Integer;
begin
 Result := CallHost(seGuiHostRequestRepaint);
end;

end.
