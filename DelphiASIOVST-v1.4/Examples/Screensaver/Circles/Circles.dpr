program Circles;

uses
  Forms,
  Windows,
  Messages,
  MainUnit in 'MainUnit.pas' {Form1};

{$E scr}

{$R *.res}

var
  Mutex            : THandle;
  MainWindowHandle : HWND;
begin
  Mutex := CreateMutex(nil, False, 'Circles');
  if (GetLastError = ERROR_ALREADY_EXISTS) or (Mutex = 0) or
     (WaitForSingleObject(Mutex, 100) = WAIT_TIMEOUT) then
   try
    MainWindowHandle := FindWindow(nil, 'Circles');
    SetWindowText(MainWindowHandle, '');
    MainWindowHandle := FindWindow(nil, 'Circles');
    if MainWindowHandle <> 0 then
     if IsIconic(MainWindowHandle)
      then ShowWindow(MainWindowHandle, SW_RESTORE)
      else BringWindowToTop(MainWindowHandle);
    Exit;
   finally
    if Mutex <> 0 then ReleaseMutex(Mutex);
    Application.Terminate;
   end;

(*
  // Check whether screensaver is already running
  Wnd := FindWindow('TMainForm', 'ScreenSaver');

  // eventually send message to close screensaver
  if Wnd <> 0 then
   if SendMessage(Wnd, WM_User, 0, 0) = 1
    then Halt(0);
*)

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Circles Screensaver';
  Application.CreateForm(TMainForm, MainForm);
//  Application.ShowMainForm := False;
  Application.Run;
  if Mutex <> 0 then ReleaseMutex(Mutex);
end.

